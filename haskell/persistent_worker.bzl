load(
    "@prelude//haskell:toolchain.bzl",
    "HaskellToolchainInfo",
)

def _persistent_worker_impl(ctx: AnalysisContext) -> list[Provider]:
    worker = ctx.attrs.worker[RunInfo] #haskell_toolchain.worker
    worker_proxy = ctx.attrs.worker_proxy[RunInfo] # haskell_toolchain.worker_proxy

    cmd = cmd_args(worker_proxy, "--exe", worker)
    cmd.add(ctx.attrs.proxy_args, "--")
    if ctx.attrs.make:
        cmd.add("--make")
    cmd.add(ctx.attrs.worker_args)
    return [DefaultInfo(), WorkerInfo(cmd)]

persistent_worker = rule(
    impl = _persistent_worker_impl,
    attrs = {
        "worker": attrs.dep(providers = [RunInfo]),
        "worker_proxy": attrs.dep(providers = [RunInfo]),
        "make": attrs.bool(default = False),
        "proxy_args": attrs.list(attrs.arg(), default = []),
        "worker_args": attrs.list(attrs.arg(), default = []),
    },
)
