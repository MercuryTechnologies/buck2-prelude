# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//utils:arglike.bzl", "ArgLike")

HaskellPlatformInfo = provider(fields = {
    "name": provider_field(typing.Any, default = None),
})

HaskellPackagesInfo = record(
    dynamic = DynamicValue,
)

HaskellPackage = record(
    db = ArgLike,
    path = Artifact,
)

HaskellToolchainInfo = provider(
    # @unsorted-dict-items
    fields = {
        "compiler": provider_field(RunInfo),
        "compiler_flags": provider_field(typing.Any, default = None),
        "linker": provider_field(RunInfo),
        "linker_flags": provider_field(typing.Any, default = None),
        "haddock": provider_field(RunInfo),
        "compiler_major_version": provider_field(str | None, default = None),
        "package_name_prefix": provider_field(typing.Any, default = None),
        "packager": provider_field(RunInfo),
        "use_argsfile": provider_field(bool, default = False),
        "support_expose_package": provider_field(bool, default = False),
        "archive_contents": provider_field(typing.Any, default = None),
        "ghci_script_template": provider_field(Artifact | None, default = None),
        "ghci_iserv_template": provider_field(Artifact | None, default = None),
        "ide_script_template": provider_field(Artifact | None, default = None),
        "ghci_binutils_path": provider_field(typing.Any, default = None),
        "ghci_lib_path": provider_field(typing.Any, default = None),
        "ghci_ghc_path": provider_field(typing.Any, default = None),
        "ghci_iserv_path": provider_field(typing.Any, default = None),
        "ghci_iserv_prof_path": provider_field(typing.Any, default = None),
        "ghci_cxx_path": provider_field(typing.Any, default = None),
        "ghci_cc_path": provider_field(typing.Any, default = None),
        "ghci_cpp_path": provider_field(typing.Any, default = None),
        "ghci_packager": provider_field(typing.Any, default = None),
        "cache_links": provider_field(typing.Any, default = None),
        "script_template_processor": provider_field(Dependency | None, default = None),
        "packages": provider_field(HaskellPackagesInfo | None, default = None),
        "use_persistent_workers": provider_field(bool, default = False),
        "use_worker": provider_field(bool, default = False),
        "worker_make": provider_field(bool, default = False),
        "ghc_dir": provider_field(Artifact | None, default = None),
        # RTS options passed to GHC, changing the behavior of the compiler process, not the resulting binaries like
        # `-with-rtsopts` would.
        "ghc_rts_flags": provider_field(typing.Any, default = None),
    },
)

HaskellToolchainLibrary = provider(
    fields = {
        "name": provider_field(str),
        "dynamic": DynamicValue,
    },
)

DynamicHaskellToolchainLibraryInfo = provider(
    fields = {
        "id": provider_field(str),
    },
)

def _haskell_package_info_as_package_db(p: HaskellPackage):
    return cmd_args(p.db)

def _haskell_package_set_root(children: list[HaskellPackage], p: HaskellPackage | None):
    return p

HaskellPackageDbTSet = transitive_set(
    args_projections = {
        "package_db": _haskell_package_info_as_package_db,
    },
    reductions = {
        "root": _haskell_package_set_root,
    },
)

DynamicHaskellPackageDbInfo = provider(fields = {
    "packages": dict[str, HaskellPackageDbTSet],
})

NativeToolchainLibrary = provider(
    fields = {
        "name": provider_field(str),
        "lib_root": provider_field(Artifact),
        "rel_path_to_root": provider_field(str),
    },
)
