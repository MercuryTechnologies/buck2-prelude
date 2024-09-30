load("@prelude//:genrule.bzl", "process_genrule")
load("@prelude//haskell:toolchain.bzl", "HaskellToolchainInfo")

def haskell_genrule_impl(ctx: AnalysisContext) -> list[Provider]:

    providers = process_genrule(ctx, ctx.attrs.out, ctx.attrs.outs)

    haskell_toolchain = ctx.attrs._haskell_toolchain[HaskellToolchainInfo]

    unkeyed_variables = {
        "hc": haskell_toolchain.compiler,
    }
    providers.append(TemplatePlaceholderInfo(unkeyed_variables = unkeyed_variables))


    return providers
