# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# TODO(cjhopman): This was generated by scripts/hacks/rules_shim_with_docs.py,
# but should be manually edited going forward. There may be some errors in
# the generated docs, and so those should be verified to be accurate and
# well-formatted (and then delete this TODO)

def _srcs_arg():
    return {
        "srcs": attrs.named_set(attrs.source(), sorted = True, default = [], doc = """
    A list of Haskell sources to be built by this rule. The dictionary option is deprecated.
"""),
    }

def _deps_arg():
    return {
        "deps": attrs.list(attrs.dep(), default = [], doc = """
    Either `haskell_library()` or `prebuilt_haskell_library()` rules
     from which this rules sources import modules or native linkable rules exporting symbols
     this rules sources call into.
"""),
        "srcs_deps": attrs.dict(attrs.source(), attrs.list(attrs.source()), default = {}, doc = """
    Allows to declare dependencies for sources manually, additionally to the dependencies automatically detected.
        """),
    }

def _compiler_flags_arg():
    return {
        "compiler_flags": attrs.list(attrs.arg(), default = [], doc = """
    Flags to pass to the Haskell compiler when compiling this rule's sources.
"""),
    }

def _exported_linker_flags_arg():
    return {
        "exported_linker_flags": attrs.list(attrs.string(), default = [], doc = """
    Linker flags used by dependent rules when linking with this library.
"""),
    }

def _scripts_arg():
    return {
        "_generate_target_metadata": attrs.dep(
            providers = [RunInfo],
            default = "prelude//haskell/tools:generate_target_metadata",
        ),
        "_ghc_wrapper": attrs.dep(
            providers = [RunInfo],
            default = "prelude//haskell/tools:ghc_wrapper",
        ),
    }

haskell_common = struct(
    srcs_arg = _srcs_arg,
    deps_arg = _deps_arg,
    compiler_flags_arg = _compiler_flags_arg,
    exported_linker_flags_arg = _exported_linker_flags_arg,
    scripts_arg = _scripts_arg,
)
