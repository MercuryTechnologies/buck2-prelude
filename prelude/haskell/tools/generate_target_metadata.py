#!/usr/bin/env python3

"""Helper script to generate relevant metadata about Haskell targets.

* The mapping from module source file to actual module name.
* The intra-package module dependency graph.
* The transitive cross-package module dependency graph.
* Which modules require Template Haskell.

The result is a JSON object with the following fields:
* `th_modules`: List of modules that require Template Haskell.
* `module_mapping`: Mapping from source inferred module name to actual module name, if different.
* `module_graph`: Intra-package module dependencies, `dict[modname, list[modname]]`.
* `transitive_deps`: Cross-package module dependencies in topological order starting at the leafs, `dict[modname, dict[pkgname, list[modname]]]`.
"""

import argparse
import graphlib
import json
import os
from pathlib import Path
import subprocess
import tempfile


def main():
    parser = argparse.ArgumentParser(
        description=__doc__,
        fromfile_prefix_chars="@")
    parser.add_argument(
        "--pkgname",
        required=True,
        type=str,
        help="The name of the current package.")
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
        "--dependency-metadata",
        required=False,
        default=[],
        type=str,
        action="append",
        help="Path to the JSON metadata file of a package dependency.")
    args = parser.parse_args()

    result = obtain_target_metadata(args)

    json.dump(result, args.output, indent=4)


def obtain_target_metadata(args):
    output_prefix = os.path.dirname(args.output.name)
    ghc_depends, ghc_options = run_ghc_depends(args.ghc, args.ghc_arg, args.source)
    th_modules = determine_th_modules(ghc_options, args.source_prefix)
    deps_md = load_dependencies_metadata(args.dependency_metadata)
    package_prefixes = calc_package_prefixes(deps_md)
    module_mapping, module_graph, package_deps = interpret_ghc_depends(
        ghc_depends, args.source_prefix, package_prefixes)
    transitive_deps = calc_transitive_deps(
        args.pkgname, module_graph, package_deps, deps_md)
    return {
        "pkgname": args.pkgname,
        "output_prefix": output_prefix,
        "th_modules": th_modules,
        "module_mapping": module_mapping,
        "module_graph": module_graph,
        "transitive_deps": transitive_deps,
    }


def determine_th_modules(ghc_options, source_prefix):
    return [
        src_to_module_name(strip_prefix_(source_prefix, fname).lstrip("/"))
        for fname, opts in ghc_options.items()
        if uses_th(opts)
    ]


__TH_EXTENSIONS = ["TemplateHaskell", "TemplateHaskellQuotes", "QuasiQuotes"]


def uses_th(opts):
    """Determine if a Template Haskell extension is enabled."""
    return any([f"-X{ext}" in opts for ext in __TH_EXTENSIONS])


def run_ghc_depends(ghc, ghc_args, sources):
    with tempfile.TemporaryDirectory() as dname:
        json_fname = os.path.join(dname, "depends.json")
        opt_json_fname = os.path.join(dname, "options.json")
        make_fname = os.path.join(dname, "depends.make")
        args = [
            ghc, "-M", "-include-pkg-deps",
            # Note: `-outputdir '.'` removes the prefix of all targets:
            #       backend/src/Foo/Util.<ext> => Foo/Util.<ext>
            "-outputdir", ".",
            "-dep-json", json_fname,
            "-opt-json", opt_json_fname,
            "-dep-makefile", make_fname,
        ] + ghc_args + sources
        subprocess.run(args, check=True)

        with open(json_fname) as f, open(opt_json_fname) as o:
            return json.load(f), json.load(o)


def load_dependencies_metadata(fnames):
    result = {}

    for fname in fnames:
        with open(fname) as f:
            md = json.load(f)
            result[md["pkgname"]] = md

    return result


def calc_package_prefixes(dependencies_metadata):
    """Creates a trie to look up modules in dependency packages.

    Package names are stored under the marker key `//pkgname`. This is
    unambiguous since path components may not contain `/` characters.
    """
    result = {}
    for pkgname, md in dependencies_metadata.items():
        path = Path(md["output_prefix"])
        layer = result
        for part in path.parts:
            layer = layer.setdefault(part, {})
        layer["//pkgname"] = pkgname
    return result


def lookup_package_dep(module_dep, package_prefixes):
    """Look up a cross-packge module dependency.

    Assumes that `module_dep` is a relative path to an interface file of the form
    `buck-out/.../__my_package__/mod-shared/Some/Package.hi`.
    """
    module_path = Path(module_dep)
    layer = package_prefixes
    for offset, part in enumerate(module_path.parts):
        if (layer := layer.get(part)) is None:
            return None

        if (pkgname := layer.get("//pkgname")) is not None:
            modname = src_to_module_name("/".join(module_path.parts[offset+2:]))
            return pkgname, modname


def interpret_ghc_depends(ghc_depends, source_prefix, package_prefixes):
    mapping = {}
    graph = {}
    extgraph = {}

    for k, vs in ghc_depends.items():
        module_name = src_to_module_name(k)
        intdeps, extdeps = parse_module_deps(vs, package_prefixes)

        graph.setdefault(module_name, []).extend(intdeps)
        for pkg, mods in extdeps.items():
            extgraph.setdefault(module_name, {}).setdefault(pkg, []).extend(mods)

        ext = os.path.splitext(k)[1]

        if ext != ".o":
            continue

        sources = list(filter(is_haskell_src, vs))

        if not sources:
            continue

        assert len(sources) == 1, "one object file must correspond to exactly one haskell source "

        hs_file = sources[0]

        hs_module_name = src_to_module_name(
            strip_prefix_(source_prefix, hs_file).lstrip("/"))

        if hs_module_name != module_name:
            mapping[hs_module_name] = module_name

    return mapping, graph, extgraph


def parse_module_deps(module_deps, package_prefixes):
    internal_deps = []
    external_deps = {}

    for module_dep in module_deps:
        if is_haskell_src(module_dep):
            continue

        if os.path.isabs(module_dep):
            continue

        if (pkgdep := lookup_package_dep(module_dep, package_prefixes)) is not None:
            pkgname, modname = pkgdep
            external_deps.setdefault(pkgname, []).append(modname)
            continue

        internal_deps.append(src_to_module_name(module_dep))

    return internal_deps, external_deps


def calc_transitive_deps(pkgname, module_graph, package_deps, deps_md):
    result = {}

    topo_modules = graphlib.TopologicalSorter(module_graph).static_order()

    for modname in topo_modules:
        result[modname] = {}

        for dep_pkg, dep_pkg_mods in package_deps.get(modname, {}).items():
            dep_pkg_trans_deps = deps_md[dep_pkg]["transitive_deps"]
            for dep_pkg_mod in dep_pkg_mods:
                for trans_pkg, trans_mods in dep_pkg_trans_deps[dep_pkg_mod].items():
                    if trans_mods:
                        result[modname].setdefault(trans_pkg, {}).update((m, None) for m in trans_mods)

        for dep_pkg, dep_pkg_mods in package_deps.get(modname, {}).items():
            if dep_pkg_mods:
                result[modname].setdefault(dep_pkg, {}).update((m, None) for m in dep_pkg_mods)

        for dep_mod in module_graph[modname]:
            for trans_pkg, trans_mods in result[dep_mod].items():
                if trans_mods:
                    result[modname].setdefault(trans_pkg, {}).update((m, None) for m in trans_mods)

        if module_graph[modname]:
            result[modname].setdefault(pkgname, {}).update((m, None) for m in module_graph[modname])

    for modname in result:
        for dep_pkg in result[modname]:
            dep_mods = list(result[modname][dep_pkg].keys())
            result[modname][dep_pkg] = dep_mods

    return result


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
