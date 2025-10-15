load(
    "@prelude//linking:link_info.bzl",
    "LinkInfo",
    "SharedLibLinkable",
)

# See: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/packages.html#installedpackageinfo-a-package-specification
PkgConfLinkFields = record(
    library_dirs = cmd_args,
    extra_libraries = cmd_args,
    cc_options = cmd_args,
    ld_options = cmd_args,
)

def get_pkg_conf_link_fields(
        *,
        pkgname: str,
        link_infos: list[LinkInfo]) -> PkgConfLinkFields:
    """
    Arguments:
        pkgname: Used for debug messages
        link_infos: Artifacts to link
    """

    # TODO: Should these be relative to `${pkgroot}`?
    # `cmd_args(relative_to=...)` is supposed to accept an `OutputArtifact` but
    # our version of Buck2 must be too old to support it.
    # I'm not sure it matters though.
    library_dirs = cmd_args(parent = 1)
    extra_libraries = cmd_args()
    cc_options = cmd_args()
    ld_options = cmd_args()

    for link_info in link_infos:
        if link_info.pre_flags:
            ld_options.add(link_info.pre_flags)
        if link_info.post_flags:
            ld_options.add(link_info.post_flags)

        for linkable in link_info.linkables:
            if isinstance(linkable, SharedLibLinkable):
                library_dirs.add(linkable.lib)

                # This seems extremely incorrect but it's what
                # `../linking/link_info.bzl` does as well!
                extra_libraries.add(linkable.lib.basename.removeprefix("lib").removesuffix(linkable.lib.extension))
            else:
                fail("Unimplemented linkable for package {}: {}".format(pkgname, linkable))

    return PkgConfLinkFields(
        library_dirs = library_dirs,
        extra_libraries = extra_libraries,
        cc_options = cc_options,
        ld_options = ld_options,
    )

def append_pkg_conf_link_fields(*, pkg_conf: cmd_args, link_fields: PkgConfLinkFields) -> None:
    pkg_conf.add(cmd_args(cmd_args(link_fields.library_dirs, delimiter = ","), format = "library-dirs: {}"))
    pkg_conf.add(cmd_args(cmd_args(link_fields.extra_libraries, delimiter = ","), format = "extra-libraries: {}"))

def append_pkg_conf_link_fields_for_link_infos(*, pkgname: str, pkg_conf: cmd_args, link_infos: list[LinkInfo]) -> None:
    append_pkg_conf_link_fields(
        pkg_conf = pkg_conf,
        link_fields = get_pkg_conf_link_fields(pkgname = pkgname, link_infos = link_infos),
    )
