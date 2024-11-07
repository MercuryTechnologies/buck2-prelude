load("@prelude//decls/toolchains_common.bzl", "toolchains_common")
load(
    "@prelude//haskell:toolchain.bzl",
    "HaskellToolchainInfo",
    "HaskellToolchainLibrary",
    "DynamicHaskellPackageDbInfo",
)

worker_libs = [
    "base",
    "binary",
    "bytestring",
    "containers",
    "deepseq",
    "exceptions",
    "extra",
    "filepath",
    "ghc",
    "ghc-persistent-worker-plugin",
    "grpc-haskell",
    "network",
    "process",
    "proto3-suite",
    "proto3-wire",
    "stm",
    "text",
    "transformers",
    "vector",
    "unix",
]

_worker_srcs = [
    "@prelude//haskell/worker/impl/plugin/src:Internal/AbiHash.hs",
    "@prelude//haskell/worker/impl/plugin/src:Internal/Args.hs",
    "@prelude//haskell/worker/impl/plugin/src:Internal/Cache.hs",
    "@prelude//haskell/worker/impl/plugin/src:Internal/Compile.hs",
    "@prelude//haskell/worker/impl/plugin/src:Internal/Error.hs",
    "@prelude//haskell/worker/impl/plugin/src:Internal/Log.hs",
    "@prelude//haskell/worker/impl/plugin/src:Internal/Session.hs",
    "@prelude//haskell/worker/impl/server/app:Server.hs",
    "@prelude//haskell/worker/impl/server/app:Pool.hs",
    "@prelude//haskell/worker/impl/server/app:Worker.hs",
    "@prelude//haskell/worker/impl/comm/src:Message.hs",
    "@prelude//haskell/worker/impl/buck-worker:Args.hs",
    "@prelude//haskell/worker/impl/buck-worker-2:Main.hs",
    "@prelude//haskell/worker/impl/buck-worker:BuckWorker.hs",
]

HaskellWorkerInfo = provider(
    fields = {
        "srcs": provider_field(list[typing.Any]),
        "deps": provider_field(list[Dependency]),
        "compiler_flags": provider_field(list[str]),
        "plugin_db": provider_field(typing.Any), # Artifact
    }
)

def _resolve_haskell_toolchain_lib_impl(actions: AnalysisActions, artifacts, dynamic_values, outputs, arg):
    pkg_deps = dynamic_values[arg.packages.dynamic]
    package_db = pkg_deps.providers[DynamicHaskellPackageDbInfo].packages
    db = package_db[arg.plugin_name]

    actions.write(outputs[arg.out].as_output(), db.value.db, with_inputs = True)

    return []

_resolve_haskell_toolchain_lib = dynamic_actions(
    impl = _resolve_haskell_toolchain_lib_impl,
)

def _worker_config_impl(ctx: AnalysisContext) -> list[Provider]:
    db = ctx.actions.declare_output("db")

    haskell_toolchain = ctx.attrs._haskell_toolchain[HaskellToolchainInfo]

    ctx.actions.dynamic_output_new(_resolve_haskell_toolchain_lib(
        dynamic = [],
        dynamic_values = [haskell_toolchain.packages.dynamic] if haskell_toolchain.packages else [],
        outputs = [db.as_output()],
        arg = struct(
            packages = haskell_toolchain.packages,
            plugin_name = ctx.attrs.plugin[HaskellToolchainLibrary].name,
            out = db,
        ),
    ))

    return [
        DefaultInfo(),
        HaskellWorkerInfo(
            srcs = ctx.attrs.srcs,
            deps = ctx.attrs.deps,
            compiler_flags = ctx.attrs.compiler_flags,
            plugin_db = db,
        ),
    ]

worker_config = rule(
    impl = _worker_config_impl,
    attrs = {
        "srcs": attrs.list(attrs.source(), default = _worker_srcs),
        "deps": attrs.list(attrs.dep()),
        "compiler_flags": attrs.list(attrs.string()),
        "plugin": attrs.dep(providers = [HaskellToolchainLibrary], default = "@prelude//haskell/worker:ghc-persistent-worker-plugin"),
        "_haskell_toolchain": toolchains_common.haskell(),
    }
)
