# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//cxx:target_sdk_version.bzl", "get_target_sdk_version")

# Returns the target_sdk_version specified for this build, falling
# back to the toolchain version when unset.
def get_min_deployment_version_for_node(ctx: AnalysisContext) -> str:
    version = get_target_sdk_version(ctx)
    if version == None:
        fail("No target_sdk_version set on target or toolchain")

    return version
