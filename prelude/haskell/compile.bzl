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
)
load(
    "@prelude//haskell:util.bzl",
    "attr_deps",
    "attr_deps_haskell_lib_infos",
    "attr_deps_haskell_link_infos",
    "get_artifact_suffix",
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
load("@prelude//utils:graph_utils.bzl", "post_order_traversal", "breadth_first_traversal")
load("@prelude//utils:strings.bzl", "strip_prefix")

DynamicCompileResultInfo = record(
)

# The type of the return value of the `_compile()` function.
CompileResultInfo = record(
    objects = field(list[Artifact]),
    hi = field(list[Artifact]),
    stubs = field(Artifact),
    producing_indices = field(bool),
    dynamic = field(None | DynamicValue),
)

CompileArgsInfo = record(
    result = field(CompileResultInfo),
    srcs = field(cmd_args),
    args_for_cmd = field(cmd_args),
    args_for_file = field(cmd_args),
)

PackagesInfo = record(
    exposed_package_imports = field(list[Artifact]),
    exposed_package_objects = field(list[Artifact]),
    exposed_package_libs = cmd_args,
    exposed_package_args = cmd_args,
    packagedb_args = cmd_args,
    transitive_deps = field(HaskellLibraryInfoTSet),
)

_Module = record(
    source = field(Artifact),
    interfaces = field(list[Artifact]),
    objects = field(list[Artifact]),
    stub_dir = field(Artifact),
    prefix_dir = field(str),
)


def _strip_prefix(prefix, s):
    stripped = strip_prefix(prefix, s)

    return stripped if stripped != None else s


def _modules_by_name(ctx: AnalysisContext, *, sources: list[Artifact], link_style: LinkStyle, enable_profiling: bool, suffix: str) -> dict[str, _Module]:
    modules = {}

    osuf, hisuf = output_extensions(link_style, enable_profiling)

    for src in sources:
        if not is_haskell_src(src.short_path):
            continue

        module_name = src_to_module_name(src.short_path)
        interface_path = paths.replace_extension(src.short_path, "." + hisuf)
        interface = ctx.actions.declare_output("mod-" + suffix, interface_path)
        interfaces = [interface]
        object_path = paths.replace_extension(src.short_path, "." + osuf)
        object = ctx.actions.declare_output("mod-" + suffix, object_path)
        objects = [object]

        if link_style in [LinkStyle("static"), LinkStyle("static_pic")]:
            dyn_osuf, dyn_hisuf = output_extensions(LinkStyle("shared"), enable_profiling)
            interface_path = paths.replace_extension(src.short_path, "." + dyn_hisuf)
            interface = ctx.actions.declare_output("mod-" + suffix, interface_path)
            interfaces.append(interface)
            object_path = paths.replace_extension(src.short_path, "." + dyn_osuf)
            object = ctx.actions.declare_output("mod-" + suffix, object_path)
            objects.append(object)

        stub_dir = ctx.actions.declare_output("stub-" + suffix + "-" + module_name, dir=True)
        modules[module_name] = _Module(source = src, interfaces = interfaces, objects = objects, stub_dir = stub_dir, prefix_dir = "mod-" + suffix)

    return modules

def target_metadata(
        ctx: AnalysisContext,
        *,
        pkgname: str,
        sources: list[Artifact],
    ) -> Artifact:
    md_file = ctx.actions.declare_output(ctx.attrs.name + ".md.json")
    md_gen = ctx.attrs._generate_target_metadata[RunInfo]

    haskell_toolchain = ctx.attrs._haskell_toolchain[HaskellToolchainInfo]
    toolchain_libs = [
        dep[HaskellToolchainLibrary].name
        for dep in ctx.attrs.deps
        if HaskellToolchainLibrary in dep
    ]

    # Add -package-db and -package/-expose-package flags for each Haskell
    # library dependency.
    packages_info = get_packages_info(
        ctx,
        LinkStyle("shared"),
        specify_pkg_version = False,
        enable_profiling = False,
        use_empty_lib = True,
    )

    # The object and interface file paths are depending on the real module name
    # as inferred by GHC, not the source file path; currently this requires the
    # module name to correspond to the source file path as otherwise GHC will
    # not be able to find the created object or interface files in the search
    # path.
    #
    # (module X.Y.Z must be defined in a file at X/Y/Z.hs)

    package_flag = _package_flag(haskell_toolchain)
    ghc_args = cmd_args()
    ghc_args.add("-hide-all-packages")
    ghc_args.add(package_flag, "base")
    ghc_args.add(cmd_args(toolchain_libs, prepend=package_flag))
    ghc_args.add(cmd_args(packages_info.exposed_package_args))
    ghc_args.add(packages_info.packagedb_args)
    ghc_args.add(ctx.attrs.compiler_flags)

    md_args = cmd_args(md_gen)
    md_args.add("--pkgname", pkgname)
    md_args.add("--output", md_file.as_output())
    md_args.add("--ghc", haskell_toolchain.compiler)
    md_args.add(cmd_args(ghc_args, format="--ghc-arg={}"))
    md_args.add(
        "--source-prefix",
        _strip_prefix(str(ctx.label.cell_root), str(ctx.label.path)),
    )
    md_args.add(cmd_args(sources, format="--source={}"))

    md_args.add(cmd_args(
        _attr_deps_haskell_lib_metadata_files(ctx),
        format="--dependency-metadata={}",
    ))

    ctx.actions.run(md_args, category = "haskell_metadata")

    return md_file

def _attr_deps_haskell_lib_metadata_files(ctx: AnalysisContext) -> list[Artifact]:
    result = []

    for dep in attr_deps(ctx) + ctx.attrs.template_deps:
        lib = dep.get(HaskellLibraryProvider)
        if lib == None:
            continue

        md = lib.metadata
        if md == None:
            continue

        result.append(md)

    return result

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
        transitive_deps: [None, dict[str, list[str]]] = None,
        pkgname: str | None = None) -> PackagesInfo:
    haskell_toolchain = ctx.attrs._haskell_toolchain[HaskellToolchainInfo]

    # Collect library dependencies. Note that these don't need to be in a
    # particular order.
    direct_deps_link_info = attr_deps_haskell_link_infos(ctx)
    libs = ctx.actions.tset(HaskellLibraryInfoTSet, children = [
        lib.prof_info[link_style] if enable_profiling else lib.info[link_style]
        for lib in direct_deps_link_info
    ])

    for lib in direct_deps_link_info:
        info = lib.prof_info[link_style] if enable_profiling else lib.info[link_style]
        direct = info.reduce("root")
        dynamic = direct.dynamic

    # base is special and gets exposed by default
    package_flag = _package_flag(haskell_toolchain)
    exposed_package_imports = []
    exposed_package_objects = []
    exposed_package_libs = cmd_args()
    exposed_package_args = cmd_args([package_flag, "base"])

    packagedb_args = cmd_args()

    if transitive_deps != None:
        lib_objects = {}
        lib_interfaces = {}
        for lib in libs.traverse():
            lib_objects[lib.name] = {}
            lib_interfaces[lib.name] = {}

            for o in lib.objects[enable_profiling]:
                # this should prefer the dyn_o -- since it is used for TH
                lib_objects[lib.name][src_to_module_name(o.short_path)] = o

            for hi in lib.import_dirs[enable_profiling]:
                mod_name = src_to_module_name(hi.short_path)
                lib_interfaces[lib.name].setdefault(mod_name, []).append(hi)

        for pkg, mods in transitive_deps.items():
            if pkg == pkgname:
                # Skip dependencies from the same package.
                continue
            for mod in mods:
                exposed_package_objects.append(lib_objects[pkg][mod])
                exposed_package_imports.extend(lib_interfaces[pkg][mod])
    else:
        for lib in libs.traverse():
            exposed_package_imports.extend(lib.import_dirs[enable_profiling])
            exposed_package_objects.extend(lib.objects[enable_profiling])
            # libs of dependencies might be needed at compile time if
            # we're using Template Haskell:
            exposed_package_libs.hidden(lib.libs)

    packagedb_args.add(libs.project_as_args("empty_package_db" if use_empty_lib else "package_db"))

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
        exposed_package_imports = exposed_package_imports,
        exposed_package_objects = exposed_package_objects,
        exposed_package_libs = exposed_package_libs,
        exposed_package_args = exposed_package_args,
        packagedb_args = packagedb_args,
        transitive_deps = libs,
    )


def _common_compile_args(
        ctx: AnalysisContext,
        link_style: LinkStyle,
        enable_profiling: bool,
        enable_th: bool,
        pkgname: str | None,
        modname: str | None = None,
        transitive_deps: [None, dict[str, list[str]]] = None,
        use_empty_lib = True) -> cmd_args:
    toolchain_libs = [dep[HaskellToolchainLibrary].name for dep in ctx.attrs.deps if HaskellToolchainLibrary in dep]

    compile_args = cmd_args()
    compile_args.add("-no-link", "-i")
    compile_args.add("-hide-all-packages")
    compile_args.add(cmd_args(toolchain_libs, prepend="-package"))

    if enable_profiling:
        compile_args.add("-prof")

    if link_style == LinkStyle("shared"):
        compile_args.add("-dynamic", "-fPIC")
    elif link_style == LinkStyle("static_pic"):
        compile_args.add("-fPIC", "-fexternal-dynamic-refs")

    osuf, hisuf = output_extensions(link_style, enable_profiling)
    compile_args.add("-osuf", osuf, "-hisuf", hisuf)

    # Add -package-db and -package/-expose-package flags for each Haskell
    # library dependency.
    packages_info = get_packages_info(
        ctx,
        link_style,
        specify_pkg_version = False,
        enable_profiling = enable_profiling,
        use_empty_lib = use_empty_lib,
        transitive_deps = transitive_deps,
        pkgname = pkgname,
    )

    compile_args.add(packages_info.exposed_package_args)
    compile_args.hidden(packages_info.exposed_package_imports)
    compile_args.add(packages_info.packagedb_args)
    if enable_th:
        compile_args.add(packages_info.exposed_package_libs)
        if modname:
            for o in packages_info.exposed_package_objects:
                if o.extension != ".o":
                    prefix = o.owner.name + "-" + modname
                    o_copy = ctx.actions.declare_output(prefix, paths.replace_extension(o.short_path, ".o"))
                    compile_args.add(ctx.actions.symlink_file(o_copy, o))
                else:
                    compile_args.add(o)
        else:
            compile_args.hidden(packages_info.exposed_package_objects)

    # Add args from preprocess-able inputs.
    inherited_pre = cxx_inherited_preprocessor_infos(ctx.attrs.deps)
    pre = cxx_merge_cpreprocessors(ctx, [], inherited_pre)
    pre_args = pre.set.project_as_args("args")
    compile_args.add(cmd_args(pre_args, format = "-optP={}"))

    if pkgname:
        compile_args.add(["-this-unit-id", pkgname])

    return compile_args

# NOTE this function is currently only used by `haskell_haddock_lib`
def compile_args(
        ctx: AnalysisContext,
        link_style: LinkStyle,
        enable_profiling: bool,
        enable_th: bool,
        pkgname = None,
        suffix: str = "") -> CompileArgsInfo:
    haskell_toolchain = ctx.attrs._haskell_toolchain[HaskellToolchainInfo]

    compile_cmd = cmd_args()
    compile_cmd.add(haskell_toolchain.compiler_flags)

    # Some rules pass in RTS (e.g. `+RTS ... -RTS`) options for GHC, which can't
    # be parsed when inside an argsfile.
    compile_cmd.add(ctx.attrs.compiler_flags)

    # TODO[CB] use the empty lib once using hi haddock
    compile_args = _common_compile_args(ctx, link_style, enable_profiling, enable_th, pkgname, use_empty_lib = False)

    if getattr(ctx.attrs, "main", None) != None:
        compile_args.add(["-main-is", ctx.attrs.main])

    artifact_suffix = get_artifact_suffix(link_style, enable_profiling, suffix)

    # TODO[AH] These are only used for haddock and conflict with tracking
    # per-module outputs individually. Rework the Haddock part to support this.
    objects = ctx.actions.declare_output(
        "objects-" + artifact_suffix,
        dir = True,
    )
    hi = ctx.actions.declare_output("hi-" + artifact_suffix, dir = True)
    stubs = ctx.actions.declare_output("stubs-" + artifact_suffix, dir = True)

    compile_args.add(
        "-odir",
        objects.as_output(),
        "-hidir",
        hi.as_output(),
        "-hiedir",
        hi.as_output(),
        "-stubdir",
        stubs.as_output(),
    )

    srcs = cmd_args()
    for (path, src) in srcs_to_pairs(ctx.attrs.srcs):
        # hs-boot files aren't expected to be an argument to compiler but does need
        # to be included in the directory of the associated src file
        if is_haskell_src(path):
            srcs.add(src)
        else:
            srcs.hidden(src)

    producing_indices = "-fwrite-ide-info" in ctx.attrs.compiler_flags

    return CompileArgsInfo(
        result = CompileResultInfo(
            objects = [objects],
            hi = [hi],
            stubs = stubs,
            producing_indices = producing_indices,
            dynamic = None,
        ),
        srcs = srcs,
        args_for_cmd = compile_cmd,
        args_for_file = compile_args,
    )

def _compile_module_args(
        ctx: AnalysisContext,
        module: _Module,
        link_style: LinkStyle,
        enable_profiling: bool,
        enable_th: bool,
        outputs: dict[Artifact, Artifact],
        pkgname = None,
        transitive_deps: [None, dict[str, list[str]]] = None) -> CompileArgsInfo:
    haskell_toolchain = ctx.attrs._haskell_toolchain[HaskellToolchainInfo]

    compile_cmd = cmd_args()
    compile_cmd.add(haskell_toolchain.compiler_flags)

    # Some rules pass in RTS (e.g. `+RTS ... -RTS`) options for GHC, which can't
    # be parsed when inside an argsfile.
    compile_cmd.add(ctx.attrs.compiler_flags)
    compile_cmd.add("-c")

    compile_args = _common_compile_args(ctx, link_style, enable_profiling, enable_th, pkgname, modname = src_to_module_name(module.source.short_path), transitive_deps = transitive_deps)

    objects = [outputs[obj] for obj in module.objects]
    his = [outputs[hi] for hi in module.interfaces]
    stubs = outputs[module.stub_dir]

    compile_args.add("-outputdir", cmd_args([cmd_args(stubs.as_output()).parent(), module.prefix_dir], delimiter="/"))
    compile_args.add("-o", objects[0].as_output())
    compile_args.add("-ohi", his[0].as_output())
    compile_args.add("-stubdir", stubs.as_output())

    if link_style in [LinkStyle("static_pic"), LinkStyle("static")]:
        compile_args.add("-dynamic-too")
        compile_args.add("-dyno", objects[1].as_output())
        compile_args.add("-dynohi", his[1].as_output())

    srcs = cmd_args(module.source)
    for (path, src) in srcs_to_pairs(ctx.attrs.srcs):
        # hs-boot files aren't expected to be an argument to compiler but does need
        # to be included in the directory of the associated src file
        if not is_haskell_src(path):
            srcs.hidden(src)

    producing_indices = "-fwrite-ide-info" in ctx.attrs.compiler_flags

    return CompileArgsInfo(
        result = CompileResultInfo(
            objects = objects,
            hi = his,
            stubs = stubs,
            producing_indices = producing_indices,
            dynamic = None,
        ),
        srcs = srcs,
        args_for_cmd = compile_cmd,
        args_for_file = compile_args,
    )


def _compile_module(
    ctx: AnalysisContext,
    *,
    link_style: LinkStyle,
    enable_profiling: bool,
    enable_th: bool,
    module_name: str,
    modules: dict[str, _Module],
    md_file: Artifact,
    graph: dict[str, list[str]],
    transitive_deps: dict[str, list[str]],
    outputs: dict[Artifact, Artifact],
    artifact_suffix: str,
    pkgname: str | None = None,
) -> None:
    module = modules[module_name]

    haskell_toolchain = ctx.attrs._haskell_toolchain[HaskellToolchainInfo]
    compile_cmd = cmd_args(haskell_toolchain.compiler)

    args = _compile_module_args(ctx, module, link_style, enable_profiling, enable_th, outputs, pkgname, transitive_deps = transitive_deps)

    if args.args_for_file:
        if haskell_toolchain.use_argsfile:
            argsfile = ctx.actions.declare_output(
                "haskell_compile_" + artifact_suffix + ".argsfile",
            )
            for_file = cmd_args(args.args_for_file).add(args.srcs)
            ctx.actions.write(argsfile.as_output(), for_file, allow_args = True)
            compile_cmd.add(cmd_args(argsfile, format = "@{}"))
            compile_cmd.hidden(for_file)
        else:
            compile_cmd.add(args.args_for_file)
            compile_cmd.add(args.srcs)

    compile_cmd.add(args.args_for_cmd)

    compile_cmd.add(
        cmd_args(
            cmd_args(md_file, format = "-i{}").parent(),
            "/",
            module.prefix_dir,
            delimiter=""
        )
    )

    for dep_name in breadth_first_traversal(graph, [module_name])[1:]:
        dep = modules[dep_name]
        compile_cmd.hidden(dep.interfaces)
        if enable_th:
            compile_cmd.hidden(dep.objects)

    ctx.actions.run(compile_cmd, category = "haskell_compile_" + artifact_suffix.replace("-", "_"), identifier = module_name)



# Compile all the context's sources.
def compile(
        ctx: AnalysisContext,
        link_style: LinkStyle,
        enable_profiling: bool,
        md_file: Artifact,
        pkgname: str | None = None) -> CompileResultInfo:
    artifact_suffix = get_artifact_suffix(link_style, enable_profiling)

    modules = _modules_by_name(ctx, sources = ctx.attrs.srcs, link_style = link_style, enable_profiling = enable_profiling, suffix = artifact_suffix)

    def do_compile(ctx, artifacts, outputs, md_file=md_file, modules=modules):
        md = artifacts[md_file].read_json()
        th_modules = md["th_modules"]
        module_map = md["module_mapping"]
        graph = md["module_graph"]
        transitive_deps = md["transitive_deps"]

        mapped_modules = { module_map.get(k, k): v for k, v in modules.items() }

        for module_name in post_order_traversal(graph):
            _compile_module(
                ctx,
                link_style = link_style,
                enable_profiling = enable_profiling,
                enable_th = module_name in th_modules,
                module_name = module_name,
                modules = mapped_modules,
                graph = graph,
                transitive_deps = transitive_deps[module_name],
                outputs = outputs,
                md_file=md_file,
                artifact_suffix = artifact_suffix,
                pkgname = pkgname,
            )

    interfaces = [interface for module in modules.values() for interface in module.interfaces]
    objects = [object for module in modules.values() for object in module.objects]
    stub_dirs = [module.stub_dir for module in modules.values()]

    dynamic = ctx.actions.dynamic_output(
        dynamic = [md_file],
        inputs = ctx.attrs.srcs,
        outputs = [o.as_output() for o in interfaces + objects + stub_dirs],
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
        stubs = stubs_dir,
        producing_indices = False,
        dynamic = dynamic,
    )
