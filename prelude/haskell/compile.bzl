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
    "@prelude//haskell:link_info.bzl",
    "HaskellLinkInfo",
    "merge_haskell_link_infos",
)
load(
    "@prelude//haskell:toolchain.bzl",
    "HaskellToolchainInfo",
    "HaskellToolchainLibrary",
)
load(
    "@prelude//haskell:util.bzl",
    "attr_deps",
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

# The type of the return value of the `_compile()` function.
CompileResultInfo = record(
    objects = field(Artifact),
    hi = field(Artifact),
    stubs = field(Artifact),
    producing_indices = field(bool),
)

CompileArgsInfo = record(
    result = field(CompileResultInfo),
    srcs = field(cmd_args),
    args_for_cmd = field(cmd_args),
    args_for_file = field(cmd_args),
)

# If the target is a haskell library, the HaskellLibraryProvider
# contains its HaskellLibraryInfo. (in contrast to a HaskellLinkInfo,
# which contains the HaskellLibraryInfo for all the transitive
# dependencies). Direct dependencies are treated differently from
# indirect dependencies for the purposes of module visibility.
HaskellLibraryProvider = provider(
    fields = {
        "lib": provider_field(typing.Any, default = None),  # dict[LinkStyle, HaskellLibraryInfo]
        "prof_lib": provider_field(typing.Any, default = None),  # dict[LinkStyle, HaskellLibraryInfo]
    },
)

# A record of a Haskell library.
HaskellLibraryInfo = record(
    # The library target name: e.g. "rts"
    name = str,
    # package config database: e.g. platform009/build/ghc/lib/package.conf.d
    db = Artifact,
    # e.g. "base-4.13.0.0"
    id = str,
    # Import dirs indexed by profiling enabled/disabled
    import_dirs = dict[bool, Artifact],
    stub_dirs = list[Artifact],

    # This field is only used as hidden inputs to compilation, to
    # support Template Haskell which may need access to the libraries
    # at compile time.  The real library flags are propagated up the
    # dependency graph via MergedLinkInfo.
    libs = field(list[Artifact], []),
    # Package version, used to specify the full package when exposing it,
    # e.g. filepath-1.4.2.1, deepseq-1.4.4.0.
    # Internal packages default to 1.0.0, e.g. `fbcode-dsi-logger-hs-types-1.0.0`.
    version = str,
    is_prebuilt = bool,
    profiling_enabled = bool,
)

PackagesInfo = record(
    exposed_package_args = cmd_args,
    packagedb_args = cmd_args,
    transitive_deps = field(list[HaskellLibraryInfo]),
)

_Module = record(
    source = field(Artifact),
    interface = field(Artifact),
    object = field(Artifact),
    stub_dir = field(Artifact),
    prefix_dir = field(str),
)


def _strip_prefix(p, path_prefix):
    if p.startswith(path_prefix):
        return p[len(path_prefix):]
    else:
        return p

def _modules_by_name(ctx: AnalysisContext, *, sources: list[Artifact], link_style: LinkStyle, enable_profiling: bool, suffix: str) -> dict[str, _Module]:
    modules = {}

    osuf, hisuf = output_extensions(link_style, enable_profiling)

    for src in sources:
        if not is_haskell_src(src.short_path):
            continue

        module_name = src_to_module_name(src.short_path)
        interface_path = paths.replace_extension(src.short_path, "." + hisuf)
        interface = ctx.actions.declare_output("mod-" + suffix, interface_path)
        object_path = paths.replace_extension(src.short_path, "." + osuf)
        object = ctx.actions.declare_output("mod-" + suffix, object_path)
        stub_dir = ctx.actions.declare_output("stub-" + suffix + "-" + module_name, dir=True)
        modules[module_name] = _Module(source = src, interface = interface, object = object, stub_dir = stub_dir, prefix_dir = "mod-" + suffix)

    return modules

def ghc_depends(ctx: AnalysisContext, *, sources: list[Artifact]) -> Artifact:
    haskell_toolchain = ctx.attrs._haskell_toolchain[HaskellToolchainInfo]

    toolchain_libs = [dep[HaskellToolchainLibrary].name for dep in ctx.attrs.deps if HaskellToolchainLibrary in dep]

    # Add -package-db and -package/-expose-package flags for each Haskell
    # library dependency.
    packages_info = get_packages_info(
        ctx,
        LinkStyle("shared"),
        specify_pkg_version = False,
        enable_profiling = False,
    )

    dep_file = ctx.actions.declare_output(ctx.attrs.name + ".depends")

    # The object and interface file paths are depending on the real module name
    # as inferred by GHC, not the source file path; currently this requires the
    # module name to correspond to the source file path as otherwise GHC will
    # not be able to find the created object or interface files in the search
    # path.
    #
    # (module X.Y.Z must be defined in a file at X/Y/Z.hs)

    # Note: `-outputdir ''` removes the prefix directory of all targets:
    #       backend/src/Foo/Util.<ext> => Foo/Util.<ext>
    dep_args = cmd_args(haskell_toolchain.compiler, "-M", "-outputdir", "", "-dep-makefile", dep_file.as_output())

    dep_args.add("-hide-all-packages")
    dep_args.add("-package", "base")
    dep_args.add(cmd_args(toolchain_libs, prepend="-package"))
    dep_args.add(packages_info.exposed_package_args)
    dep_args.add(packages_info.packagedb_args)

    dep_args.add(ctx.attrs.compiler_flags)
    dep_args.add(sources)
    ctx.actions.run(dep_args, category = "ghc_depends")

    return dep_file

def _parse_depends(depends: str, path_prefix: str) -> dict[str, list[str]]:
    graph = {}

    for line in depends.splitlines():
        if line.startswith("#"):
            continue

        k, v = line.strip().split(" : ", 1)
        vs = v.split(" ")

        module_name = src_to_module_name(k)
        deps = [
            src_to_module_name(_strip_prefix(v, path_prefix).lstrip("/"))
            for v in vs
            if not is_haskell_src(v)
        ]

        graph.setdefault(module_name, []).extend(deps)

    return graph

def _attr_deps_haskell_link_infos(ctx: AnalysisContext) -> list[HaskellLinkInfo]:
    return filter(
        None,
        [
            d.get(HaskellLinkInfo)
            for d in attr_deps(ctx) + ctx.attrs.template_deps
        ],
    )

def _attr_deps_haskell_lib_infos(
        ctx: AnalysisContext,
        link_style: LinkStyle,
        enable_profiling: bool) -> list[HaskellLibraryInfo]:
    if enable_profiling and link_style == LinkStyle("shared"):
        fail("Profiling isn't supported when using dynamic linking")
    return [
        x.prof_lib[link_style] if enable_profiling else x.lib[link_style]
        for x in filter(None, [
            d.get(HaskellLibraryProvider)
            for d in attr_deps(ctx) + ctx.attrs.template_deps
        ])
    ]

def _package_flag(toolchain: HaskellToolchainInfo) -> str:
    if toolchain.support_expose_package:
        return "-expose-package"
    else:
        return "-package"

def get_packages_info(
        ctx: AnalysisContext,
        link_style: LinkStyle,
        specify_pkg_version: bool,
        enable_profiling: bool) -> PackagesInfo:
    haskell_toolchain = ctx.attrs._haskell_toolchain[HaskellToolchainInfo]

    # Collect library dependencies. Note that these don't need to be in a
    # particular order and we really want to remove duplicates (there
    # are a *lot* of duplicates).
    libs = {}
    direct_deps_link_info = _attr_deps_haskell_link_infos(ctx)
    merged_hs_link_info = merge_haskell_link_infos(direct_deps_link_info)

    hs_link_info = merged_hs_link_info.prof_info if enable_profiling else merged_hs_link_info.info

    for lib in hs_link_info[link_style]:
        libs[lib.db] = lib  # lib.db is a good enough unique key

    # base is special and gets exposed by default
    package_flag = _package_flag(haskell_toolchain)
    exposed_package_args = cmd_args([package_flag, "base"])

    packagedb_args = cmd_args()

    for lib in libs.values():
        exposed_package_args.hidden(lib.import_dirs.values())
        exposed_package_args.hidden(lib.stub_dirs)

        # libs of dependencies might be needed at compile time if
        # we're using Template Haskell:
        exposed_package_args.hidden(lib.libs)

        packagedb_args.hidden(lib.import_dirs.values())
        packagedb_args.hidden(lib.stub_dirs)
        packagedb_args.hidden(lib.libs)

    for lib in libs.values():
        # These we need to add for all the packages/dependencies, i.e.
        # direct and transitive (e.g. `fbcode-common-hs-util-hs-array`)
        packagedb_args.add("-package-db", lib.db)

    haskell_direct_deps_lib_infos = _attr_deps_haskell_lib_infos(
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
        exposed_package_args = exposed_package_args,
        packagedb_args = packagedb_args,
        transitive_deps = libs.values(),
    )


def _common_compile_args(
        ctx: AnalysisContext,
        link_style: LinkStyle,
        enable_profiling: bool,
        pkgname: str | None) -> cmd_args:
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
    )

    compile_args.add(packages_info.exposed_package_args)
    compile_args.add(packages_info.packagedb_args)

    # Add args from preprocess-able inputs.
    inherited_pre = cxx_inherited_preprocessor_infos(ctx.attrs.deps)
    pre = cxx_merge_cpreprocessors(ctx, [], inherited_pre)
    pre_args = pre.set.project_as_args("args")
    compile_args.add(cmd_args(pre_args, format = "-optP={}"))

    if pkgname:
        compile_args.add(["-this-unit-id", pkgname])

    return compile_args

def compile_args(
        ctx: AnalysisContext,
        link_style: LinkStyle,
        enable_profiling: bool,
        pkgname = None,
        suffix: str = "") -> CompileArgsInfo:
    haskell_toolchain = ctx.attrs._haskell_toolchain[HaskellToolchainInfo]

    compile_cmd = cmd_args()
    compile_cmd.add(haskell_toolchain.compiler_flags)

    # Some rules pass in RTS (e.g. `+RTS ... -RTS`) options for GHC, which can't
    # be parsed when inside an argsfile.
    compile_cmd.add(ctx.attrs.compiler_flags)

    compile_args = _common_compile_args(ctx, link_style, enable_profiling, pkgname)

    if getattr(ctx.attrs, "main", None) != None:
        compile_args.add(["-main-is", ctx.attrs.main])

    artifact_suffix = get_artifact_suffix(link_style, enable_profiling, suffix)

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
            objects = objects,
            hi = hi,
            stubs = stubs,
            producing_indices = producing_indices,
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
        outputs: dict[Artifact, Artifact],
        pkgname = None) -> CompileArgsInfo:
    haskell_toolchain = ctx.attrs._haskell_toolchain[HaskellToolchainInfo]

    compile_cmd = cmd_args()
    compile_cmd.add(haskell_toolchain.compiler_flags)

    # Some rules pass in RTS (e.g. `+RTS ... -RTS`) options for GHC, which can't
    # be parsed when inside an argsfile.
    compile_cmd.add(ctx.attrs.compiler_flags)
    compile_cmd.add("-c")

    compile_args = _common_compile_args(ctx, link_style, enable_profiling, pkgname)

    object = outputs[module.object]
    hi = outputs[module.interface]
    stubs = outputs[module.stub_dir]

    compile_args.add("-ohi", cmd_args(hi.as_output()))
    compile_args.add("-o", cmd_args(object.as_output()))
    compile_args.add("-stubdir", stubs.as_output())

    srcs = cmd_args(module.source)
    for (path, src) in srcs_to_pairs(ctx.attrs.srcs):
        # hs-boot files aren't expected to be an argument to compiler but does need
        # to be included in the directory of the associated src file
        if not is_haskell_src(path):
            srcs.hidden(src)

    producing_indices = "-fwrite-ide-info" in ctx.attrs.compiler_flags

    return CompileArgsInfo(
        result = CompileResultInfo(
            objects = object,
            hi = hi,
            stubs = stubs,
            producing_indices = producing_indices,
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
    module_name: str,
    modules: dict[str, _Module],
    dep_file: Artifact,
    graph: dict[str, list[str]],
    outputs: dict[Artifact, Artifact],
    artifact_suffix: str,
    pkgname: str | None = None,
) -> None:
    module = modules[module_name]

    haskell_toolchain = ctx.attrs._haskell_toolchain[HaskellToolchainInfo]
    compile_cmd = cmd_args(haskell_toolchain.compiler)

    args = _compile_module_args(ctx, module, link_style, enable_profiling, outputs, pkgname)

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
            cmd_args(dep_file, format = "-i{}").parent(),
            "/",
            module.prefix_dir,
            delimiter=""
        )
    )

    for dep_name in breadth_first_traversal(graph, [module_name])[1:]:
        dep = modules[dep_name]
        compile_cmd.hidden(dep.interface, dep.object)

    ctx.actions.run(compile_cmd, category = "haskell_compile_" + artifact_suffix.replace("-", "_"), identifier = module_name, no_outputs_cleanup = True)



# Compile all the context's sources.
def compile(
        ctx: AnalysisContext,
        link_style: LinkStyle,
        enable_profiling: bool,
        dep_file: Artifact,
        pkgname: str | None = None) -> CompileResultInfo:
    artifact_suffix = get_artifact_suffix(link_style, enable_profiling)

    modules = _modules_by_name(ctx, sources = ctx.attrs.srcs, link_style = link_style, enable_profiling = enable_profiling, suffix = artifact_suffix)

    def do_compile(ctx, artifacts, outputs, dep_file=dep_file, modules=modules):
        graph = _parse_depends(artifacts[dep_file].read_string(), _strip_prefix(str(ctx.label.path), str(ctx.label.cell_root)))

        for module_name in post_order_traversal(graph):
            _compile_module(
                ctx,
                link_style = link_style,
                enable_profiling = enable_profiling,
                module_name = module_name,
                modules = modules,
                graph = graph,
                outputs = outputs,
                dep_file=dep_file,
                artifact_suffix = artifact_suffix,
                pkgname = pkgname,
            )

    interfaces = [module.interface for module in modules.values()]
    objects = [module.object for module in modules.values()]
    stub_dirs = [module.stub_dir for module in modules.values()]

    ctx.actions.dynamic_output(
        dynamic = [dep_file],
        inputs = ctx.attrs.srcs,
        outputs = interfaces + objects + stub_dirs,
        f = do_compile)

    object_dir = ctx.actions.declare_output("objects-" + artifact_suffix, dir=True)

    ctx.actions.copied_dir(object_dir.as_output(), {
        a.short_path : a for a in objects
    })

    hi_dir = ctx.actions.declare_output("hi-" + artifact_suffix, dir=True)

    ctx.actions.copied_dir(hi_dir.as_output(), {
        a.short_path : a for a in interfaces
    })

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
        objects = object_dir,
        hi = hi_dir,
        stubs = stubs_dir,
        producing_indices = False,
    )