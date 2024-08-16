#!/usr/bin/env python3

"""Helper script to generate relevant metadata about Haskell targets.

* The mapping from module source file to actual module name.
* The intra-package module dependency graph.
* The cross-package module dependencies.
* Which modules require Template Haskell.

The result is a JSON object with the following fields:
* `th_modules`: List of modules that require Template Haskell.
* `module_mapping`: Mapping from source inferred module name to actual module name, if different.
* `module_graph`: Intra-package module dependencies, `dict[modname, list[modname]]`.
* `boot_deps`: Intra-package dependencies on boot-modules, `dict[modname, list[modname]]`.
* `package_deps`": Cross-package module dependencies, `dict[modname, dict[pkgname, list[modname]]`.
"""

import argparse
import sys
import json
import os
from pathlib import Path
import shlex
import subprocess
import tempfile


def main():
    parser = argparse.ArgumentParser(
        description=__doc__,
        fromfile_prefix_chars="@")
    parser.add_argument(
        "--output",
        required=True,
        type=argparse.FileType("w"),
        help="Write package metadata to this file in JSON format.")
    parser.add_argument(
        "--ghc",
        required=True,
        type=str,
        help="Path to the Haskell compiler GHC.")
    parser.add_argument(
        "--ghc-arg",
        required=False,
        type=str,
        action="append",
        help="GHC compiler argument to forward to `ghc -M`, including package flags.")
    parser.add_argument(
        "--source-prefix",
        required=True,
        type=str,
        help="The path prefix to strip of module sources to extract module names.")
    parser.add_argument(
        "--source",
        required=True,
        type=str,
        action="append",
        help="Haskell module source files of the current package.")
    parser.add_argument(
        "--package",
        required=False,
        type=str,
        action="append",
        default=[],
        help="Package dependencies formated as `NAME:PREFIX_PATH`.")
    parser.add_argument(
        "--bin-path",
        type=Path,
        action="append",
        default=[],
        help="Add given path to PATH.",
    )
    args = parser.parse_args()

    result = obtain_target_metadata(args)

    json.dump(result, args.output, indent=4, default=json_default_handler)


def json_default_handler(o):
    if isinstance(o, set):
        return sorted(o)
    raise TypeError(f'Object of type {o.__class__.__name__} is not JSON serializable')


def obtain_target_metadata(args):
    paths = [str(binpath) for binpath in args.bin_path if binpath.is_dir()]
    ghc_depends = run_ghc_depends(args.ghc, args.ghc_arg, args.source, paths)
    th_modules = determine_th_modules(ghc_depends)
    module_mapping = determine_module_mapping(ghc_depends, args.source_prefix)
    # TODO(ah) handle .hi-boot dependencies
    module_graph, boot_deps = determine_module_graph(ghc_depends)
    package_deps = determine_package_deps(ghc_depends)
    return {
        "th_modules": th_modules,
        "module_mapping": module_mapping,
        "module_graph": module_graph,
        "boot_deps": boot_deps,
        "package_deps": package_deps,
    }


def load_toolchain_packages(filepath):
    with open(filepath, "r") as f:
        return json.load(f)


def determine_th_modules(ghc_depends):
    return [
        modname
        for modname, properties in ghc_depends.items()
        if uses_th(properties.get("options", []))
    ]


__TH_EXTENSIONS = ["TemplateHaskell", "TemplateHaskellQuotes", "QuasiQuotes"]


def uses_th(opts):
    """Determine if a Template Haskell extension is enabled."""
    return any([f"-X{ext}" in opts for ext in __TH_EXTENSIONS])


def determine_module_mapping(ghc_depends, source_prefix):
    result = {}

    for modname, properties in ghc_depends.items():
        sources = list(filter(is_haskell_src, properties.get("sources", [])))

        if len(sources) != 1:
            raise RuntimeError(f"Expected exactly one Haskell source for module '{modname}' but got '{sources}'.")

        apparent_name = src_to_module_name(strip_prefix_(source_prefix, sources[0]).lstrip("/"))

        if apparent_name != modname:
            result[apparent_name] = modname

    return result


def determine_module_graph(ghc_depends):
    module_deps = {
        modname: description.get("modules", [])
        for modname, description in ghc_depends.items()
    }
    boot_deps = {
        modname: description.get("modules-boot", [])
        for modname, description in ghc_depends.items()
    }
    return module_deps, boot_deps


def determine_package_deps(ghc_depends):
    package_deps = {}

    for modname, description in ghc_depends.items():
        for pkgdep in description.get("packages", {}):
            pkgname = pkgdep.get("name")
            package_deps.setdefault(modname, {})[pkgname] = pkgdep.get("modules", [])

    return package_deps


def run_ghc_depends(ghc, ghc_args, sources, aux_paths):
    with tempfile.TemporaryDirectory() as dname:
        json_fname = os.path.join(dname, "depends.json")
        make_fname = os.path.join(dname, "depends.make")
        args = [
            ghc, "-M", "-include-pkg-deps",
            # Note: `-outputdir '.'` removes the prefix of all targets:
            #       backend/src/Foo/Util.<ext> => Foo/Util.<ext>
            "-outputdir", ".",
            "-dep-json", json_fname,
            "-dep-makefile", make_fname,
        ] + ghc_args + sources

        env = os.environ.copy()
        path = env.get("PATH", "")
        env["PATH"] = os.pathsep.join([path] + aux_paths)

        res = subprocess.run(args, env=env, capture_output=True)
        if res.returncode != 0:
            # Write the GHC command on failure.
            print(shlex.join(args), file=sys.stderr)

        # Always forward stdout/stderr.
        sys.stdout.buffer.write(res.stdout)
        sys.stderr.buffer.write(res.stderr)

        if res.returncode != 0:
            # Fail if GHC failed.
            sys.exit(res.returncode)

        with open(json_fname) as f:
            return json.load(f)


def src_to_module_name(x):
    base, _ = os.path.splitext(x)
    return base.replace("/", ".")


def is_haskell_src(x):
    _, ext = os.path.splitext(x)
    return ext in HASKELL_EXTENSIONS


HASKELL_EXTENSIONS = [
    ".hs",
    ".lhs",
    ".hsc",
    ".chs",
    ".x",
    ".y",
]


def strip_prefix_(prefix, s):
    stripped = strip_prefix(prefix, s)

    if stripped == None:
        return s

    return stripped


def strip_prefix(prefix, s):
    if s.startswith(prefix):
        return s[len(prefix):]

    return None


if __name__ == "__main__":
    main()
