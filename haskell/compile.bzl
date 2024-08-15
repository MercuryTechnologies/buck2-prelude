# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//cxx:preprocessor.bzl",
    "cxx_inherited_preprocessor_infos",
    "cxx_merge_cpreprocessors",
)
load(
    "@prelude//haskell:library_info.bzl",
    "HaskellLibraryProvider",
    "HaskellLibraryInfoTSet",
)
load(
    "@prelude//haskell:toolchain.bzl",
    "HaskellToolchainInfo",
    "HaskellToolchainLibrary",
    "DynamicHaskellPackageDbInfo",
    "HaskellPackageDbTSet",
)
load(
    "@prelude//haskell:util.bzl",
    "attr_deps",
    "attr_deps_haskell_lib_infos",
    "attr_deps_haskell_link_infos",
    "attr_deps_haskell_toolchain_libraries",
    "get_artifact_suffix",
    "is_haskell_boot",
    "is_haskell_src",
    "output_extensions",
    "src_to_module_name",
    "srcs_to_pairs",
)
load(
    "@prelude//linking:link_info.bzl",
    "LinkStyle",
)
load("@prelude//:paths.bzl", "paths")
load("@prelude//utils:graph_utils.bzl", "post_order_traversal")
load("@prelude//utils:strings.bzl", "strip_prefix")

CompiledModuleInfo = provider(fields = {
    "abi": provider_field(Artifact),
    "interfaces": provider_field(list[Artifact]),
    # TODO[AH] track this module's package-name/id & package-db instead.
    "db_deps": provider_field(list[Artifact]),
})

def _compiled_module_project_as_abi(mod: CompiledModuleInfo) -> cmd_args:
    return cmd_args(mod.abi)

def _compiled_module_project_as_interfaces(mod: CompiledModuleInfo) -> cmd_args:
    return cmd_args(mod.interfaces)

def _compiled_module_reduce_as_packagedb_deps(children: list[dict[Artifact, None]], mod: CompiledModuleInfo | None) -> dict[Artifact, None]:
    # TODO[AH] is there a better way to avoid duplicate package-dbs?
    #   Using a projection instead would produce duplicates.
    result = {db: None for db in mod.db_deps} if mod else {}
    for child in children:
        result.update(child)
    return result

CompiledModuleTSet = transitive_set(
    args_projections = {
        "abi": _compiled_module_project_as_abi,
        "interfaces": _compiled_module_project_as_interfaces,
    },
    reductions = {
        "packagedb_deps": _compiled_module_reduce_as_packagedb_deps,
    },
)

DynamicCompileResultInfo = provider(fields = {
    "modules": dict[str, CompiledModuleTSet],
})

# The type of the return value of the `_compile()` function.
CompileResultInfo = record(
    objects = field(list[Artifact]),
    hi = field(list[Artifact]),
    stubs = field(Artifact),
    hashes = field(list[Artifact]),
    producing_indices = field(bool),
    module_tsets = field(DynamicValue),
    src_prefix = field(str),
)

PackagesInfo = record(
    exposed_package_libs = cmd_args,
    exposed_package_args = cmd_args,
    packagedb_args = cmd_args,
    transitive_deps = field(HaskellLibraryInfoTSet),
    bin_paths = cmd_args,
)

_Module = record(
    source = field(Artifact),
    interfaces = field(list[Artifact]),
    hash = field(Artifact),
    objects = field(list[Artifact]),
    stub_dir = field(Artifact | None),
    prefix_dir = field(str),
)


def _strip_prefix(prefix, s):
    stripped = strip_prefix(prefix, s)

    return stripped if stripped != None else s


def _modules_by_name(ctx: AnalysisContext, *, sources: list[Artifact], link_style: LinkStyle, enable_profiling: bool, suffix: str) -> dict[str, _Module]:
    modules = {}

    osuf, hisuf = output_extensions(link_style, enable_profiling)

    for src in sources:
        bootsuf = ""
        if is_haskell_boot(src.short_path):
            bootsuf = "-boot"
        elif not is_haskell_src(src.short_path):
            continue

        module_name = src_to_module_name(src.short_path) + bootsuf
        interface_path = paths.replace_extension(src.short_path, "." + hisuf + bootsuf)
        interface = ctx.actions.declare_output("mod-" + suffix, interface_path)
        interfaces = [interface]
        object_path = paths.replace_extension(src.short_path, "." + osuf + bootsuf)
        object = ctx.actions.declare_output("mod-" + suffix, object_path)
        objects = [object]
        hash = ctx.actions.declare_output("mod-" + suffix, interface_path + ".hash")

        if link_style in [LinkStyle("static"), LinkStyle("static_pic")]:
            dyn_osuf, dyn_hisuf = output_extensions(LinkStyle("shared"), enable_profiling)
            interface_path = paths.replace_extension(src.short_path, "." + dyn_hisuf + bootsuf)
            interface = ctx.actions.declare_output("mod-" + suffix, interface_path)
            interfaces.append(interface)
            object_path = paths.replace_extension(src.short_path, "." + dyn_osuf + bootsuf)
            object = ctx.actions.declare_output("mod-" + suffix, object_path)
            objects.append(object)

        if bootsuf == "":
            stub_dir = ctx.actions.declare_output("stub-" + suffix + "-" + module_name, dir=True)
        else:
            stub_dir = None

        prefix_dir = "mod-" + suffix

        src_strip_prefix = getattr(ctx.attrs, "src_strip_prefix", None)

        if src_strip_prefix:
            prefix_dir += "/" + src_strip_prefix

        modules[module_name] = _Module(
            source = src,
            interfaces = interfaces,
            hash = hash,
            objects = objects,
            stub_dir = stub_dir,
            prefix_dir = prefix_dir)

    return modules

def target_metadata(
        ctx: AnalysisContext,
        *,
        sources: list[Artifact],
        suffix: str = "",
    ) -> Artifact:
    md_file = ctx.actions.declare_output(ctx.attrs.name + suffix + ".md.json")
    md_gen = ctx.attrs._generate_target_metadata[RunInfo]

    haskell_toolchain = ctx.attrs._haskell_toolchain[HaskellToolchainInfo]
    toolchain_libs = [
        dep[HaskellToolchainLibrary].name
        for dep in ctx.attrs.deps
        if HaskellToolchainLibrary in dep
    ]

    # The object and interface file paths are depending on the real module name
    # as inferred by GHC, not the source file path; currently this requires the
    # module name to correspond to the source file path as otherwise GHC will
    # not be able to find the created object or interface files in the search
    # path.
    #
    # (module X.Y.Z must be defined in a file at X/Y/Z.hs)

    def get_metadata(ctx, _artifacts, resolved, outputs):

        # Add -package-db and -package/-expose-package flags for each Haskell
        # library dependency.

        packages_info = get_packages_info(
            ctx,
            LinkStyle("shared"),
            specify_pkg_version = False,
            enable_profiling = False,
            use_empty_lib = True,
            resolved = resolved,
        )
        package_flag = _package_flag(haskell_toolchain)
        ghc_args = cmd_args()
        ghc_args.add("-hide-all-packages")
        ghc_args.add(package_flag, "base")

        ghc_args.add(cmd_args(toolchain_libs, prepend=package_flag))
        ghc_args.add(cmd_args(packages_info.exposed_package_args))
        ghc_args.add(cmd_args(packages_info.packagedb_args, prepend = "-package-db"))
        ghc_args.add(ctx.attrs.compiler_flags)

        md_args = cmd_args(md_gen)
        md_args.add(packages_info.bin_paths)
        md_args.add("--ghc", haskell_toolchain.compiler)
        md_args.add(cmd_args(ghc_args, format="--ghc-arg={}"))
        md_args.add(
            "--source-prefix",
            _strip_prefix(str(ctx.label.cell_root), str(ctx.label.path)),
        )
        md_args.add(cmd_args(sources, format="--source={}"))

        md_args.add(
            _attr_deps_haskell_lib_package_name_and_prefix(ctx),
        )
        md_args.add("--output", outputs[md_file].as_output())

        ctx.actions.run(md_args, category = "haskell_metadata", identifier = suffix if suffix else None)

    ctx.actions.dynamic_output(
        dynamic = [],
        promises = [haskell_toolchain.packages.dynamic] if haskell_toolchain.packages else [],
        inputs = [],
        outputs = [md_file.as_output()],
        f = get_metadata,
    )

    return md_file

def _attr_deps_haskell_lib_package_name_and_prefix(ctx: AnalysisContext) -> cmd_args:
    args = cmd_args(prepend = "--package")

    for dep in attr_deps(ctx) + ctx.attrs.template_deps:
        lib = dep.get(HaskellLibraryProvider)
        if lib == None:
            continue

        lib_info = lib.lib.values()[0]
        args.add(cmd_args(
            lib_info.name,
            cmd_args(lib_info.db, parent = 1),
            delimiter = ":",
        ))

    return args

def _package_flag(toolchain: HaskellToolchainInfo) -> str:
    if toolchain.support_expose_package:
        return "-expose-package"
    else:
        return "-package"

def get_packages_info(
        ctx: AnalysisContext,
        link_style: LinkStyle,
        specify_pkg_version: bool,
        enable_profiling: bool,
        use_empty_lib: bool,
        resolved: None | dict[DynamicValue, ResolvedDynamicValue] = None) -> PackagesInfo:
    haskell_toolchain = ctx.attrs._haskell_toolchain[HaskellToolchainInfo]

    # Collect library dependencies. Note that these don't need to be in a
    # particular order.
    direct_deps_link_info = attr_deps_haskell_link_infos(ctx)
    libs = ctx.actions.tset(HaskellLibraryInfoTSet, children = [
        lib.prof_info[link_style] if enable_profiling else lib.info[link_style]
        for lib in direct_deps_link_info
    ])

    # base is special and gets exposed by default
    package_flag = _package_flag(haskell_toolchain)
    exposed_package_libs = cmd_args()
    exposed_package_args = cmd_args([package_flag, "base"])

    for lib in libs.traverse():
        exposed_package_libs.hidden(lib.libs)

    packagedb_args = cmd_args(libs.project_as_args(
        "empty_package_db" if use_empty_lib else "package_db",
    ))

    if haskell_toolchain.packages and resolved != None:
        haskell_toolchain = ctx.attrs._haskell_toolchain[HaskellToolchainInfo]
        pkg_deps = resolved[haskell_toolchain.packages.dynamic]
        package_db = pkg_deps[DynamicHaskellPackageDbInfo].packages

        direct_toolchain_libs = [
            dep[HaskellToolchainLibrary].name
            for dep in ctx.attrs.deps
            if HaskellToolchainLibrary in dep
        ]

        toolchain_libs = direct_toolchain_libs + libs.reduce("packages")

        package_db_tset = ctx.actions.tset(
            HaskellPackageDbTSet,
            children = [package_db[name] for name in toolchain_libs if name in package_db]
        )

        packagedb_args.add(package_db_tset.project_as_args("package_db"))

        direct_package_paths = [package_db[name].value.path for name in direct_toolchain_libs if name in package_db]
        bin_paths = cmd_args(direct_package_paths, format="--bin-path={}/bin")
    else:
        bin_paths = cmd_args()

    haskell_direct_deps_lib_infos = attr_deps_haskell_lib_infos(
        ctx,
        link_style,
        enable_profiling,
    )

    # Expose only the packages we depend on directly
    for lib in haskell_direct_deps_lib_infos:
        pkg_name = lib.name
        if (specify_pkg_version):
            pkg_name += "-{}".format(lib.version)

        exposed_package_args.add(package_flag, pkg_name)

    return PackagesInfo(
        exposed_package_libs = exposed_package_libs,
        exposed_package_args = exposed_package_args,
        packagedb_args = packagedb_args,
        transitive_deps = libs,
        bin_paths = bin_paths,
    )

CommonCompileModuleArgs = record(
    command = field(cmd_args),
    args_for_file = field(cmd_args),
    package_env_args = field(cmd_args),
)

def _common_compile_module_args(
    ctx: AnalysisContext,
    *,
    resolved: dict[DynamicValue, ResolvedDynamicValue],
    enable_haddock: bool,
    enable_profiling: bool,
    link_style: LinkStyle,
    direct_deps_info: list[HaskellLibraryInfoTSet],
    pkgname: str | None = None,
) -> CommonCompileModuleArgs:
    haskell_toolchain = ctx.attrs._haskell_toolchain[HaskellToolchainInfo]

    command = cmd_args(ctx.attrs._ghc_wrapper[RunInfo])
    command.add("--ghc", haskell_toolchain.compiler)

    # Some rules pass in RTS (e.g. `+RTS ... -RTS`) options for GHC, which can't
    # be parsed when inside an argsfile.
    command.add(haskell_toolchain.compiler_flags)
    command.add(ctx.attrs.compiler_flags)

    command.add("-c")

    if getattr(ctx.attrs, "main", None) != None:
        command.add(["-main-is", ctx.attrs.main])

    if enable_haddock:
        command.add("-haddock")

    args_for_file = cmd_args()

    args_for_file.add("-no-link", "-i")
    args_for_file.add("-hide-all-packages")

    if enable_profiling:
        args_for_file.add("-prof")

    if link_style == LinkStyle("shared"):
        args_for_file.add("-dynamic", "-fPIC")
    elif link_style == LinkStyle("static_pic"):
        args_for_file.add("-fPIC", "-fexternal-dynamic-refs")

    osuf, hisuf = output_extensions(link_style, enable_profiling)
    args_for_file.add("-osuf", osuf, "-hisuf", hisuf)

    non_haskell_sources = [
        src
        for (path, src) in srcs_to_pairs(ctx.attrs.srcs)
        if not is_haskell_src(path) and not is_haskell_boot(path)
    ]

    if non_haskell_sources:
        warning("{} specifies non-haskell file in `srcs`, consider using `srcs_deps` instead".format(ctx.label))

        args_for_file.hidden(non_haskell_sources)

    # Add args from preprocess-able inputs.
    inherited_pre = cxx_inherited_preprocessor_infos(ctx.attrs.deps)
    pre = cxx_merge_cpreprocessors(ctx, [], inherited_pre)
    pre_args = pre.set.project_as_args("args")
    args_for_file.add(cmd_args(pre_args, format = "-optP={}"))

    if pkgname:
        args_for_file.add(["-this-unit-id", pkgname])

    # Add -package-db and -package/-expose-package flags for each Haskell
    # library dependency.

    libs = ctx.actions.tset(HaskellLibraryInfoTSet, children = direct_deps_info)

    direct_toolchain_libs = [
        dep[HaskellToolchainLibrary].name
        for dep in ctx.attrs.deps
        if HaskellToolchainLibrary in dep
    ]
    toolchain_libs = direct_toolchain_libs + libs.reduce("packages")

    if haskell_toolchain.packages:
        pkg_deps = resolved[haskell_toolchain.packages.dynamic]
        package_db = pkg_deps[DynamicHaskellPackageDbInfo].packages
    else:
        package_db = []

    package_db_tset = ctx.actions.tset(
        HaskellPackageDbTSet,
        children = [package_db[name] for name in toolchain_libs if name in package_db]
    )

    direct_package_paths = [package_db[name].value.path for name in direct_toolchain_libs if name in package_db]
    args_for_file.add(cmd_args(
        direct_package_paths,
        format="--bin-path={}/bin",
    ))

    packagedb_args = cmd_args(libs.project_as_args("empty_package_db"))
    packagedb_args.add(package_db_tset.project_as_args("package_db"))

    # TODO[AH] Avoid duplicates and share identical env files.
    #   The set of package-dbs can be known at the package level, not just the
    #   module level. So, we could generate this file outside of the
    #   dynamic_output action.
    package_env_file = ctx.actions.declare_output(".".join([
        ctx.label.name,
        "package-db",
        output_extensions(link_style, enable_profiling)[1],
        "env",
    ]))
    package_env = cmd_args(delimiter = "\n")
    package_env.add(cmd_args(
        packagedb_args,
        format = "package-db {}",
    ).relative_to(package_env_file, parent = 1))
    ctx.actions.write(
        package_env_file,
        package_env,
    )
    package_env_args = cmd_args(
        package_env_file,
        prepend = "-package-env",
        hidden = packagedb_args,
    )

    return CommonCompileModuleArgs(
        command = command,
        args_for_file = args_for_file,
        package_env_args = package_env_args,
    )

def _compile_module(
    ctx: AnalysisContext,
    *,
    common_args: CommonCompileModuleArgs,
    link_style: LinkStyle,
    enable_profiling: bool,
    enable_th: bool,
    module_name: str,
    module: _Module,
    module_tsets: dict[str, CompiledModuleTSet],
    md_file: Artifact,
    graph: dict[str, list[str]],
    package_deps: dict[str, list[str]],
    outputs: dict[Artifact, Artifact],
    artifact_suffix: str,
    direct_deps_by_name: dict[str, typing.Any],
    toolchain_deps_by_name: dict[str, None],
) -> CompiledModuleTSet:
    compile_cmd = cmd_args(common_args.command)
    # These compiler arguments can be passed in a response file.
    compile_args_for_file = cmd_args(common_args.args_for_file)

    haskell_toolchain = ctx.attrs._haskell_toolchain[HaskellToolchainInfo]

    packagedb_tag = ctx.actions.artifact_tag()
    compile_args_for_file.add(packagedb_tag.tag_artifacts(common_args.package_env_args))

    dep_file = ctx.actions.declare_output(".".join([
        ctx.label.name,
        module_name or "pkg",
        "package-db",
        output_extensions(link_style, enable_profiling)[1],
        "dep",
    ])).as_output()
    tagged_dep_file = packagedb_tag.tag_artifacts(dep_file)
    compile_args_for_file.add("--buck2-packagedb-dep", tagged_dep_file)

    objects = [outputs[obj] for obj in module.objects]
    his = [outputs[hi] for hi in module.interfaces]
    if module.stub_dir != None:
        stubs = outputs[module.stub_dir]

    compile_args_for_file.add("-o", objects[0].as_output())
    compile_args_for_file.add("-ohi", his[0].as_output())

    # Set the output directories. We do not use the -outputdir flag, but set the directories individually.
    # Note, the -outputdir option is shorthand for the combination of -odir, -hidir, -hiedir, -stubdir and -dumpdir.
    # But setting -hidir effectively disables the use of the search path to look up interface files,
    # as ghc exclusively looks in that directory when it is set.
    for dir in ["o", "hie", "dump"]:
        compile_args_for_file.add(
           "-{}dir".format(dir), cmd_args([cmd_args(stubs.as_output(), parent=1), module.prefix_dir], delimiter="/"),
        )
    if module.stub_dir != None:
        compile_args_for_file.add("-stubdir", stubs.as_output())

    if link_style in [LinkStyle("static_pic"), LinkStyle("static")]:
        compile_args_for_file.add("-dynamic-too")
        compile_args_for_file.add("-dyno", objects[1].as_output())
        compile_args_for_file.add("-dynohi", his[1].as_output())

    compile_args_for_file.add(module.source)

    aux_deps = ctx.attrs.srcs_deps.get(module.source)
    if aux_deps:
        compile_args_for_file.hidden(aux_deps)

    if haskell_toolchain.use_argsfile:
        argsfile = ctx.actions.declare_output(
            "haskell_compile_" + artifact_suffix + ".argsfile",
        )
        ctx.actions.write(argsfile.as_output(), compile_args_for_file, allow_args = True)
        compile_cmd.add(cmd_args(argsfile, format = "@{}"))
        compile_cmd.hidden(compile_args_for_file)
    else:
        compile_cmd.add(compile_args_for_file)

    compile_cmd.add(
        cmd_args(
            cmd_args(md_file, format = "-i{}", ignore_artifacts=True).parent(),
            "/",
            module.prefix_dir,
            delimiter=""
        )
    )

    toolchain_deps = []
    library_deps = []
    exposed_package_modules = []
    exposed_package_dbs = []
    for dep_pkgname, dep_modules in package_deps.items():
        if dep_pkgname in toolchain_deps_by_name:
            toolchain_deps.append(dep_pkgname)
        elif dep_pkgname in direct_deps_by_name:
            library_deps.append(dep_pkgname)
            exposed_package_dbs.append(direct_deps_by_name[dep_pkgname].package_db)
            for dep_modname in dep_modules:
                exposed_package_modules.append(direct_deps_by_name[dep_pkgname].modules[dep_modname])
        else:
            fail("Unknown library dependency '{}'. Add the library to the `deps` attribute".format(dep_pkgname))

    # Transitive module dependencies from other packages.
    cross_package_modules = ctx.actions.tset(
        CompiledModuleTSet,
        children = exposed_package_modules,
    )
    # Transitive module dependencies from the same package.
    this_package_modules = [
        module_tsets[dep_name]
        for dep_name in graph[module_name]
    ]

    dependency_modules = ctx.actions.tset(
        CompiledModuleTSet,
        children = [cross_package_modules] + this_package_modules,
    )

    compile_cmd.add(cmd_args(library_deps, prepend = "-package"))
    compile_cmd.add(cmd_args(toolchain_deps, prepend = "-package"))

    abi_tag = ctx.actions.artifact_tag()

    compile_cmd.hidden(
        abi_tag.tag_artifacts(dependency_modules.project_as_args("interfaces")))
    compile_cmd.add("-fbyte-code-and-object-code")
    if enable_th:
        compile_cmd.add("-fprefer-byte-code")

    compile_cmd.add(cmd_args(dependency_modules.reduce("packagedb_deps").keys(), prepend = "--buck2-package-db"))

    dep_file = ctx.actions.declare_output("dep-{}_{}".format(module_name, artifact_suffix)).as_output()

    tagged_dep_file = abi_tag.tag_artifacts(dep_file)

    compile_cmd.add("--buck2-dep", tagged_dep_file)
    compile_cmd.add("--abi-out", outputs[module.hash].as_output())
    compile_cmd.hidden(dependency_modules.project_as_args("abi"))

    ctx.actions.run(
        compile_cmd, category = "haskell_compile_" + artifact_suffix.replace("-", "_"), identifier = module_name,
        dep_files = {
            "abi": abi_tag,
            "packagedb": packagedb_tag,
        }
    )

    module_tset = ctx.actions.tset(
        CompiledModuleTSet,
        value = CompiledModuleInfo(
            abi = module.hash,
            interfaces = module.interfaces,
            db_deps = exposed_package_dbs,
        ),
        children = [cross_package_modules] + this_package_modules,
    )

    return module_tset


# Compile all the context's sources.
def compile(
        ctx: AnalysisContext,
        link_style: LinkStyle,
        enable_profiling: bool,
        enable_haddock: bool,
        md_file: Artifact,
        pkgname: str | None = None) -> CompileResultInfo:
    artifact_suffix = get_artifact_suffix(link_style, enable_profiling)

    modules = _modules_by_name(ctx, sources = ctx.attrs.srcs, link_style = link_style, enable_profiling = enable_profiling, suffix = artifact_suffix)

    def do_compile(ctx, artifacts, resolved, outputs, md_file=md_file, modules=modules):
        # Collect library dependencies. Note that these don't need to be in a
        # particular order.
        toolchain_deps_by_name = {
            lib.name: None
            for lib in attr_deps_haskell_toolchain_libraries(ctx)
        }
        direct_deps_info = [
            lib.prof_info[link_style] if enable_profiling else lib.info[link_style]
            for lib in attr_deps_haskell_link_infos(ctx)
        ]
        direct_deps_by_name = {
            info.value.name: struct(
                package_db = info.value.empty_db,
                modules = resolved[info.value.dynamic[enable_profiling]][DynamicCompileResultInfo].modules,
            )
            for info in direct_deps_info
        }
        common_args = _common_compile_module_args(
            ctx,
            resolved = resolved,
            enable_haddock = enable_haddock,
            enable_profiling = enable_profiling,
            link_style = link_style,
            direct_deps_info = direct_deps_info,
            pkgname = pkgname,
        )

        md = artifacts[md_file].read_json()
        th_modules = md["th_modules"]
        module_map = md["module_mapping"]
        graph = md["module_graph"]
        package_deps = md["package_deps"]

        boot_rev_deps = {}
        for module_name, boot_deps in md["boot_deps"].items():
            for boot_dep in boot_deps:
                boot_rev_deps.setdefault(boot_dep + "-boot", []).append(module_name)

        # TODO GHC --dep-json should integrate boot modules directly into the dependency graph.
        for module_name, module in modules.items():
            if not module_name.endswith("-boot"):
                continue

            # Add boot modules to the module graph
            graph[module_name] = []
            # TODO GHC --dep-json should report boot module dependencies.
            # The following is a naive approximation of the boot module's dependencies,
            # taking the corresponding module's dependencies
            # minus those that depend on the boot module.

            # Add module dependencies for the boot module
            graph[module_name].extend([
                dep
                for dep in graph[module_name[:-5]]
                if not dep in boot_rev_deps[module_name]
            ])

            # Add package dependencies for the boot module
            package_deps[module_name] = package_deps.get(module_name[:-5], [])

        for module_name, boot_deps in md["boot_deps"].items():
            for boot_dep in boot_deps:
                graph.setdefault(module_name, []).append(boot_dep + "-boot")

        mapped_modules = { module_map.get(k, k): v for k, v in modules.items() }
        module_tsets = {}

        for module_name in post_order_traversal(graph):
            module_tsets[module_name] = _compile_module(
                ctx,
                common_args = common_args,
                link_style = link_style,
                enable_profiling = enable_profiling,
                enable_th = module_name in th_modules,
                module_name = module_name,
                module = mapped_modules[module_name],
                module_tsets = module_tsets,
                graph = graph,
                package_deps = package_deps.get(module_name, {}),
                outputs = outputs,
                md_file=md_file,
                artifact_suffix = artifact_suffix,
                direct_deps_by_name = direct_deps_by_name,
                toolchain_deps_by_name = toolchain_deps_by_name,
            )

        return [DynamicCompileResultInfo(modules = module_tsets)]

    haskell_toolchain = ctx.attrs._haskell_toolchain[HaskellToolchainInfo]

    interfaces = [interface for module in modules.values() for interface in module.interfaces]
    objects = [object for module in modules.values() for object in module.objects]
    stub_dirs = [
        module.stub_dir
        for module in modules.values()
        if module.stub_dir != None
    ]
    abi_hashes = [module.hash for module in modules.values()]

    dyn_module_tsets = ctx.actions.dynamic_output(
        dynamic = [md_file],
        promises = [
            info.value.dynamic[enable_profiling]
            for lib in attr_deps_haskell_link_infos(ctx)
            for info in [
                lib.prof_info[link_style]
                if enable_profiling else
                lib.info[link_style]
            ]
        ] + ([ haskell_toolchain.packages.dynamic ] if haskell_toolchain.packages else [ ]),
        inputs = ctx.attrs.srcs,
        outputs = [o.as_output() for o in interfaces + objects + stub_dirs + abi_hashes],
        f = do_compile)

    stubs_dir = ctx.actions.declare_output("stubs-" + artifact_suffix, dir=True)

    # collect the stubs from all modules into the stubs_dir
    ctx.actions.run(
        cmd_args([
            "bash", "-exuc",
            """\
            mkdir -p \"$0\"
            for stub; do
              find \"$stub\" -mindepth 1 -maxdepth 1 -exec cp -r -t \"$0\" '{}' ';'
            done
            """,
            stubs_dir.as_output(),
            stub_dirs
        ]),
        category = "haskell_stubs",
        identifier = artifact_suffix,
        local_only = True,
    )

    return CompileResultInfo(
        objects = objects,
        hi = interfaces,
        hashes = abi_hashes,
        stubs = stubs_dir,
        producing_indices = False,
        module_tsets = dyn_module_tsets,
        src_prefix = getattr(ctx.attrs, "src_strip_prefix", ""),
    )
