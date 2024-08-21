# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//linking:link_groups.bzl",
    "merge_link_group_lib_info",
)
load(
    "@prelude//linking:link_info.bzl",
    "MergedLinkInfo",
    "create_merged_link_info_for_propagation",
)
load(
    "@prelude//linking:linkable_graph.bzl",
    "create_linkable_graph",
)
load(
    "@prelude//linking:shared_libraries.bzl",
    "SharedLibraryInfo",
    "merge_shared_libraries",
)
load(
    "@prelude//utils:utils.bzl",
    "map_idx",
)
load(":compile.bzl", "GoPkgCompileInfo", "get_inherited_compile_pkgs")
load(":coverage.bzl", "GoCoverageMode")
load(":link.bzl", "GoPkgLinkInfo", "get_inherited_link_pkgs")
load(":package_builder.bzl", "build_package")
load(":packages.bzl", "go_attr_pkg_name", "merge_pkgs")

def cgo_library_impl(ctx: AnalysisContext) -> list[Provider]:
    pkg_name = go_attr_pkg_name(ctx)

    race = ctx.attrs._race
    asan = ctx.attrs._asan
    coverage_mode = GoCoverageMode(ctx.attrs._coverage_mode) if ctx.attrs._coverage_mode else None

    # Build Go library.
    compiled_pkg, pkg_info = build_package(
        ctx,
        pkg_name,
        ctx.attrs.go_srcs + ctx.attrs.srcs + ctx.attrs.headers,
        package_root = ctx.attrs.package_root,
        deps = ctx.attrs.deps + ctx.attrs.exported_deps,
        race = race,
        asan = asan,
        coverage_mode = coverage_mode,
        embedcfg = ctx.attrs.embedcfg,
    )

    pkgs = {
        pkg_name: compiled_pkg,
    }

    # We need to keep pre-processed cgo source files,
    # because they are required for any editing and linting (like VSCode+gopls)
    # to work with cgo. And when nearly every FB service client is cgo,
    # we need to support it well.
    return [
        DefaultInfo(default_output = compiled_pkg.pkg, other_outputs = [pkg_info.cgo_gen_dir]),
        GoPkgCompileInfo(pkgs = merge_pkgs([
            pkgs,
            get_inherited_compile_pkgs(ctx.attrs.exported_deps),
        ])),
        GoPkgLinkInfo(pkgs = merge_pkgs([
            pkgs,
            get_inherited_link_pkgs(ctx.attrs.deps + ctx.attrs.exported_deps),
        ])),
        create_merged_link_info_for_propagation(ctx, filter(None, [d.get(MergedLinkInfo) for d in ctx.attrs.deps])),
        merge_shared_libraries(
            ctx.actions,
            deps = filter(None, map_idx(SharedLibraryInfo, ctx.attrs.deps)),
        ),
        merge_link_group_lib_info(deps = ctx.attrs.deps),
        create_linkable_graph(
            ctx,
            deps = ctx.attrs.deps,
        ),
        pkg_info,
    ]
