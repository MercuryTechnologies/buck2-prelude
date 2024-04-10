# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//linking:link_info.bzl",
    "LinkStyle",
)
load(
    "@prelude//utils:utils.bzl",
    "map_val",
    "value_or",
)
load(":link.bzl", "GoBuildMode", "link")
load(":package_builder.bzl", "build_package")

def go_exported_library_impl(ctx: AnalysisContext) -> list[Provider]:
    lib = build_package(
        ctx,
        "main",
        ctx.attrs.srcs,
        package_root = ctx.attrs.package_root,
        deps = ctx.attrs.deps,
        compiler_flags = ctx.attrs.compiler_flags,
        shared = True,
        race = ctx.attrs._race,
        embedcfg = ctx.attrs.embedcfg,
        # We need to set CGO_DESABLED for "pure" Go libraries, otherwise CGo files may be selected for compilation.
        force_disable_cgo = True,
    )
    (bin, runtime_files, _external_debug_info) = link(
        ctx,
        lib.pkg,
        deps = ctx.attrs.deps,
        build_mode = GoBuildMode(ctx.attrs.build_mode),
        link_style = value_or(map_val(LinkStyle, ctx.attrs.link_style), LinkStyle("static_pic")),
        linker_flags = ctx.attrs.linker_flags,
        external_linker_flags = ctx.attrs.external_linker_flags,
        shared = True,
        race = ctx.attrs._race,
    )
    return [
        DefaultInfo(
            default_output = bin,
            other_outputs = runtime_files,
        ),
    ]
