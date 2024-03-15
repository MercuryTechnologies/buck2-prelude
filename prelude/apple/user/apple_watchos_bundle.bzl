# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//apple:apple_bundle.bzl", "apple_bundle_impl")
load("@prelude//apple:apple_rules_impl_utility.bzl", "apple_bundle_extra_attrs")
load("@prelude//user:rule_spec.bzl", "RuleRegistrationSpec")
load("@prelude//decls/common.bzl", "Traversal")
load("@prelude//decls/ios_rules.bzl", "AppleBundleExtension")
load(":watch_transition.bzl", "watch_transition")

def _apple_bundle_base_attrs():
    return {
        # Attributes comes from `attributes.bzl` but since it's autogenerated, we cannot easily abstract
        "asset_catalogs_compilation_options": attrs.dict(key = attrs.string(), value = attrs.any(), default = {}),
        "binary": attrs.option(attrs.dep(), default = None),
        "codesign_flags": attrs.list(attrs.string(), default = []),
        "codesign_identity": attrs.option(attrs.string(), default = None),
        "contacts": attrs.list(attrs.string(), default = []),
        "default_host_platform": attrs.option(attrs.configuration_label(), default = None),
        "default_platform": attrs.option(attrs.string(), default = None),
        "deps": attrs.list(attrs.dep(), default = []),
        "extension": attrs.one_of(attrs.enum(AppleBundleExtension), attrs.string()),
        "ibtool_flags": attrs.option(attrs.list(attrs.string()), default = None),
        "ibtool_module_flag": attrs.option(attrs.bool(), default = None),
        "incremental_bundling_enabled": attrs.option(attrs.bool(), default = None),
        "info_plist": attrs.source(),
        "info_plist_substitutions": attrs.dict(key = attrs.string(), value = attrs.string(), sorted = False, default = {}),
        "labels": attrs.list(attrs.string(), default = []),
        "licenses": attrs.list(attrs.source(), default = []),
        "platform_binary": attrs.option(attrs.list(attrs.tuple(attrs.regex(), attrs.dep())), default = None),
        "product_name": attrs.option(attrs.string(), default = None),
        "resource_group": attrs.option(attrs.string(), default = None),
        "resource_group_map": attrs.option(attrs.list(attrs.tuple(attrs.string(), attrs.list(attrs.tuple(attrs.dep(), attrs.enum(Traversal.values()), attrs.option(attrs.string()))))), default = None),
        "skip_copying_swift_stdlib": attrs.option(attrs.bool(), default = None),
        "try_skip_code_signing": attrs.option(attrs.bool(), default = None),
        "xcode_product_type": attrs.option(attrs.string(), default = None),
    }

def _apple_watchos_bundle_attrs():
    attributes = {}
    attributes.update(_apple_bundle_base_attrs())
    attributes.update(apple_bundle_extra_attrs())
    return attributes

def apple_watchos_bundle_impl(ctx: AnalysisContext) -> list[Provider]:
    # This rule is _equivalent_ to `apple_bundle` except it applies
    # an incoming watchOS transition.
    return apple_bundle_impl(ctx)

registration_spec = RuleRegistrationSpec(
    name = "apple_watchos_bundle",
    impl = apple_watchos_bundle_impl,
    attrs = _apple_watchos_bundle_attrs(),
    cfg = watch_transition,
)
