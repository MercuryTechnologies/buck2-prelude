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

load("@prelude//linking:types.bzl", "Linkage")
load(":common.bzl", "LinkableDepType", "buck", "prelude_rule")
load(":haskell_common.bzl", "haskell_common")
load(":native_common.bzl", "native_common")

haskell_binary = prelude_rule(
    name = "haskell_binary",
    docs = """
        A `haskell_binary()` rule represents a groups of Haskell sources
        and deps which build an executable.
    """,
    examples = """
        ```

        haskell_binary(
          name = 'foo',
          srcs = [
            'Foo.hs',
          ],
        )

        ```
    """,
    further = None,
    attrs = (
        # @unsorted-dict-items
        {
            "main": attrs.option(attrs.string(), default = None, doc = """
                The main module serving as the entry point into the binary. If not specified,
                 the compiler default is used.
            """),
        } |
        native_common.link_group_deps() |
        native_common.link_group_public_deps_label() |
        native_common.link_style() |
        haskell_common.srcs_arg() |
        haskell_common.compiler_flags_arg() |
        haskell_common.deps_arg() |
        haskell_common.scripts_arg() |
        buck.platform_deps_arg() |
        {
            "contacts": attrs.list(attrs.string(), default = []),
            "default_host_platform": attrs.option(attrs.configuration_label(), default = None),
            "deps_query": attrs.option(attrs.query(), default = None),
            "enable_profiling": attrs.bool(default = False),
            "ghci_platform_preload_deps": attrs.list(attrs.tuple(attrs.regex(), attrs.set(attrs.dep(), sorted = True)), default = []),
            "ghci_preload_deps": attrs.set(attrs.dep(), sorted = True, default = []),
            "labels": attrs.list(attrs.string(), default = []),
            "licenses": attrs.list(attrs.source(), default = []),
            "link_deps_query_whole": attrs.bool(default = False),
            "linker_flags": attrs.list(attrs.arg(), default = []),
            "platform": attrs.option(attrs.string(), default = None),
            "platform_linker_flags": attrs.list(attrs.tuple(attrs.regex(), attrs.list(attrs.arg())), default = []),
        }
    ),
)

haskell_ghci = prelude_rule(
    name = "haskell_ghci",
    docs = "",
    examples = None,
    further = None,
    attrs = (
        # @unsorted-dict-items
        {
            "compiler_flags": attrs.list(attrs.string(), default = []),
            "contacts": attrs.list(attrs.string(), default = []),
            "default_host_platform": attrs.option(attrs.configuration_label(), default = None),
            "deps": attrs.list(attrs.dep(), default = []),
            "deps_query": attrs.option(attrs.query(), default = None),
            "enable_profiling": attrs.bool(default = False),
            "extra_script_templates": attrs.list(attrs.source(), default = []),
            "ghci_bin_dep": attrs.option(attrs.dep(), default = None),
            "ghci_init": attrs.option(attrs.source(), default = None),
            "labels": attrs.list(attrs.string(), default = []),
            "licenses": attrs.list(attrs.source(), default = []),
            "linker_flags": attrs.list(attrs.arg(), default = []),
            "platform": attrs.option(attrs.string(), default = None),
            "platform_deps": attrs.list(attrs.tuple(attrs.regex(), attrs.set(attrs.dep(), sorted = True)), default = []),
            "platform_preload_deps": attrs.list(attrs.tuple(attrs.regex(), attrs.set(attrs.dep(), sorted = True)), default = []),
            "preload_deps": attrs.set(attrs.dep(), sorted = True, default = []),
            "srcs": attrs.named_set(attrs.source(), sorted = True, default = []),
        }
    ),
)

haskell_haddock = prelude_rule(
    name = "haskell_haddock",
    docs = "",
    examples = None,
    further = None,
    attrs = (
        # @unsorted-dict-items
        {
            "contacts": attrs.list(attrs.string(), default = []),
            "default_host_platform": attrs.option(attrs.configuration_label(), default = None),
            "deps": attrs.list(attrs.dep(), default = []),
            "deps_query": attrs.option(attrs.query(), default = None),
            "haddock_flags": attrs.list(attrs.arg(), default = []),
            "labels": attrs.list(attrs.string(), default = []),
            "licenses": attrs.list(attrs.source(), default = []),
            "platform": attrs.option(attrs.string(), default = None),
            "platform_deps": attrs.list(attrs.tuple(attrs.regex(), attrs.set(attrs.dep(), sorted = True)), default = []),
        }
    ),
)

haskell_ide = prelude_rule(
    name = "haskell_ide",
    docs = "",
    examples = None,
    further = None,
    attrs = (
        # @unsorted-dict-items
        {
            "compiler_flags": attrs.list(attrs.string(), default = []),
            "contacts": attrs.list(attrs.string(), default = []),
            "default_host_platform": attrs.option(attrs.configuration_label(), default = None),
            "deps": attrs.list(attrs.dep(), default = []),
            "deps_query": attrs.option(attrs.query(), default = None),
            "extra_script_templates": attrs.list(attrs.source(), default = []),
            "labels": attrs.list(attrs.string(), default = []),
            "licenses": attrs.list(attrs.source(), default = []),
            "link_style": attrs.enum(LinkableDepType),
            "linker_flags": attrs.list(attrs.arg(), default = []),
            "platform": attrs.option(attrs.string(), default = None),
            "platform_deps": attrs.list(attrs.tuple(attrs.regex(), attrs.set(attrs.dep(), sorted = True)), default = []),
            "srcs": attrs.named_set(attrs.source(), sorted = True, default = []),
        }
    ),
)

haskell_library = prelude_rule(
    name = "haskell_library",
    docs = """
        A `haskell_library()` rule is used to identity a group of
        Haskell sources.
    """,
    examples = """
        ```

        haskell_library(
          name = 'fileutil',
          srcs = [
            'FileUtil.hs',
          ],
        )

        ```
    """,
    further = None,
    attrs = (
        # @unsorted-dict-items
        haskell_common.srcs_arg() |
        haskell_common.compiler_flags_arg() |
        haskell_common.deps_arg() |
        haskell_common.scripts_arg() |
        buck.platform_deps_arg() |
        native_common.link_whole(link_whole_type = attrs.bool(default = False)) |
        native_common.preferred_linkage(preferred_linkage_type = attrs.enum(Linkage.values())) |
        {
            "contacts": attrs.list(attrs.string(), default = []),
            "default_host_platform": attrs.option(attrs.configuration_label(), default = None),
            "enable_profiling": attrs.bool(default = False),
            "ghci_platform_preload_deps": attrs.list(attrs.tuple(attrs.regex(), attrs.set(attrs.dep(), sorted = True)), default = []),
            "ghci_preload_deps": attrs.set(attrs.dep(), sorted = True, default = []),
            "haddock_flags": attrs.list(attrs.arg(), default = []),
            "labels": attrs.list(attrs.string(), default = []),
            "licenses": attrs.list(attrs.source(), default = []),
            "linker_flags": attrs.list(attrs.arg(), default = []),
            "platform": attrs.option(attrs.string(), default = None),
            "platform_linker_flags": attrs.list(attrs.tuple(attrs.regex(), attrs.list(attrs.arg())), default = []),
        }
    ),
)

haskell_toolchain_library = prelude_rule(
    name = "haskell_toolchain_library",
    docs  = """
       Declare a library available as part of the GHC toolchain.
    """,
    attrs = {
    },
)


haskell_prebuilt_library = prelude_rule(
    name = "haskell_prebuilt_library",
    docs = """
        A `prebuilt_haskell_library()` rule is used to identify Haskell
        prebuilt libraries and their associated interface files.
    """,
    examples = """
        ```

        prebuilt_haskell_library(
          name = 'file',
          static_interfaces = [
            'interfaces',
          ],
          shared_interfaces = [
            'interfaces_dyn',
          ],
          static_libs = [
            'libFileUtil.a',
          ],
          shared_libs = {
            'libFileUtil.so': 'libFileUtil.so',
          },
        )

        ```
    """,
    further = None,
    attrs = (
        # @unsorted-dict-items
        {
            "deps": attrs.list(attrs.dep(), default = [], doc = """
                Other `prebuilt_haskell_library()` rules from which this library
                 imports modules.
            """),
            "static_libs": attrs.list(attrs.source(), default = [], doc = """
                The libraries to use when building a statically linked top-level target.
            """),
            "shared_libs": attrs.dict(key = attrs.string(), value = attrs.source(), sorted = False, default = {}, doc = """
                A map of shared library names to shared library paths to use when building a
                 dynamically linked top-level target.
            """),
            "exported_compiler_flags": attrs.list(attrs.string(), default = [], doc = """
                Compiler flags used by dependent rules when compiling with this library.
            """),
        } |
        haskell_common.exported_linker_flags_arg() |
        {
            "contacts": attrs.list(attrs.string(), default = []),
            "cxx_header_dirs": attrs.list(attrs.source(), default = []),
            "db": attrs.source(),
            "default_host_platform": attrs.option(attrs.configuration_label(), default = None),
            "enable_profiling": attrs.bool(default = False),
            "id": attrs.string(default = ""),
            "import_dirs": attrs.list(attrs.source(), default = []),
            "labels": attrs.list(attrs.string(), default = []),
            "licenses": attrs.list(attrs.source(), default = []),
            "pic_profiled_static_libs": attrs.list(attrs.source(), default = []),
            "pic_static_libs": attrs.list(attrs.source(), default = []),
            "profiled_static_libs": attrs.list(attrs.source(), default = []),
            "version": attrs.string(default = ""),
        }
    ),
)

haskell_rules = struct(
    haskell_binary = haskell_binary,
    haskell_ghci = haskell_ghci,
    haskell_haddock = haskell_haddock,
    haskell_ide = haskell_ide,
    haskell_library = haskell_library,
    haskell_prebuilt_library = haskell_prebuilt_library,
    haskell_toolchain_library = haskell_toolchain_library,
)
