#!/usr/bin/env python3

"""Helper script to generate relevant metadata about Haskell targets.

* The mapping from module source file to actual module name.
* The intra-package module dependency graph.
* The cross-package module dependencies.
* Which modules require Template Haskell.

Note, boot files will be represented by a `-boot` suffix in the module name.

The result is a JSON object with the following fields:
* `th_modules`: List of modules that require Template Haskell.
* `module_mapping`: Mapping from source inferred module name to actual module name, if different.
* `module_graph`: Intra-package module dependencies, `dict[modname, list[modname]]`.
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
        "--cwd",
        required=False,
        type=Path,
        help="Path to ghc's working directory."
    )
    parser.add_argument(
        "--output",
        required=True,
        type=argparse.FileType("w"),
        help="Write package metadata to this file in JSON format.")
    parser.add_argument(
        "--worker-target-id",
        required=False,
        type=str,
        help="Worker id")
    parser.add_argument(
        "--ghc",
        required=True,
        type=Path,
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
    parser.add_argument(
        "--bin-exe",
        type=Path,
        action="append",
        default=[],
        help="Add given exe (more specific than bin-path)",
    )
    parser.add_argument(
        "--build-plan",
        type=str,
        help="Previously obtained build plan",
    )
    args = parser.parse_args()

    result = obtain_target_metadata(args)

    json.dump(
        result, args.output, indent=4, sort_keys=True, default=json_default_handler
    )


def json_default_handler(o):
    if isinstance(o, set):
        return sorted(o)
    raise TypeError(f'Object of type {o.__class__.__name__} is not JSON serializable')


def obtain_buildplan(args, paths):
    result = run_ghc_buildplan(args.ghc, args.ghc_arg, args.source, paths)

    return result

def obtain_target_metadata(args):
    aux_paths = [str(binpath) for binpath in args.bin_path if binpath.is_dir()] + [str(binexepath.parent) for binexepath in args.bin_exe]
    if args.build_plan == None:
        buildplan = obtain_buildplan(args, aux_paths)
    else:
        # FIXME
        sys.exit("FIXME")
        ghc_depends = load_toolchain_packages(args.build_plan)
    th_modules = determine_th_modules(buildplan)

    module_mapping = determine_module_mapping(buildplan, args.source_prefix)
    module_graph = determine_module_graph(buildplan)
    package_deps = determine_package_deps(buildplan)
    return {
        "th_modules": th_modules,
        "module_mapping": module_mapping,
        "module_graph": module_graph,
        "package_deps": package_deps,
    }


def load_toolchain_packages(filepath):
    with open(filepath, "r") as f:
        return json.load(f)


def module_name(node):
    name = node["module_name"]

    return name + "-boot" if node["is_boot"] else name


def apparent_name(node, source_prefix):
    name = src_to_module_name(
        strip_prefix_(source_prefix, node["hs_path"]).lstrip("/")
    )

    return name + "-boot" if node["is_boot"] else name


def determine_th_modules(buildplan):
    result = []

    def handle_node(node):
        if "compile-or-link" not in node:
            return
        if node["compile-or-link"] == "link":
            return
        if node["uses_th"]:
            result.append(module_name(node))

    for module in buildplan:
        module_type = module["type"]
        if module_type == "single-module":
            handle_node(module["node"])
        elif module_type == "resolved-cycle":
            for node in module["nodes"]:
                handle_node(node)
        else:
            raise Error("unknown module type: " + module_type)

    return set(result)


def determine_module_mapping(buildplan, source_prefix):
    result = {}

    def handle_node(node):
        if "compile-or-link" not in node:
            return
        if node["compile-or-link"] == "link":
            return
        if node.get("external", False):
            return
        modname = module_name(node)
        appname = apparent_name(node, source_prefix)
        if appname != modname:
            result[appname] = modname

    for module in buildplan:
        module_type = module["type"]

        if module_type == "single-module":
            handle_node(module["node"])
        elif module_type == "resolved-cycle":
            for node in module["nodes"]:
                handle_node(node)
        else:
            raise Error("unknown module type: " + module_type)

    return result


def determine_module_graph(buildplan):
    module_deps = {}

    modules = [
        module_name(mod["node"]) for mod in buildplan
        if mod["type"] == "single-module"
        if "module_name" in mod["node"]
    ]
    reexports = {
        modname : mod["node"]["reexports"]
        for mod in buildplan
        if mod["type"] == "single-module" and (modname := mod["node"].get("module_name"))
    }
    def handle_node(node):
        if "compile-or-link" not in node:
            return
        if node["compile-or-link"] == "link":
            return
        if node.get("external", False):
            return
        deps = set(
            module_name(dep)
            for dep in node["dependencies"]
            if not dep.get("external", False)
        )
        deps |= set(
             reexport
             for dep in deps
             for reexport in reexports.get(dep, [])
             if reexport in modules
        )
        module_deps[module_name(node)] = sorted(deps)

    for module in buildplan:
        module_type = module["type"]

        if module_type == "single-module":
            handle_node(module["node"])
        elif module_type == "resolved-cycle":
            for node in module["nodes"]:
                handle_node(node)
        else:
            raise Error("unknown module type: " + module_type)

    return module_deps


def determine_package_deps(buildplan):
    package_deps = {}

    def unit_id_to_package(unit_id):
        return unit_id.split("-", 1)[0]

    def handle_node(node):
        if "compile-or-link" not in node:
            return
        if node["compile-or-link"] == "link":
            return
        if node.get("external", False):
            return
        modname = module_name(node)
        package_deps[modname] = {
            unit_id_to_package(dep["unit_id"]): []
            for dep in node["dependencies"]
            if dep.get("external", False)
        }

    for module in buildplan:
        module_type = module["type"]

        if module_type == "single-module":
            handle_node(module["node"])
        elif module_type == "resolved-cycle":
            for node in module["nodes"]:
                handle_node(node)
        else:
            raise Error("unknown module type: " + module_type)

    return package_deps


def run_ghc_buildplan(ghc, ghc_args, sources, aux_paths):
    with tempfile.TemporaryDirectory() as dname:
        json_fname = os.path.join(dname, "buildplan.json")
        haskell_sources = list(filter(is_haskell_src, sources))

        args = [
            ghc,
            "-include-pkg-deps", # FIXME does have no effect currently
            "--buildplan", json_fname,
        ] + ghc_args + haskell_sources

        env = os.environ.copy()
        path = env.get("PATH", "")
        env["PATH"] = os.pathsep.join([path] + aux_paths)

        res = subprocess.run(args, env=env, capture_output=True)
        if res.returncode != 0:
            # Write the GHC command on failure.
            print(shlex.join(args), file=sys.stderr)

        # Always forward stdout/stderr.
        # Note, Buck2 swallows stdout on successful builds.
        # Redirect to stderr to avoid this.
        sys.stderr.buffer.write(res.stdout)
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


def is_haskell_boot(x):
    _, ext = os.path.splitext(x)
    return ext in HASKELL_BOOT_EXTENSIONS


HASKELL_EXTENSIONS = [
    ".hs",
    ".lhs",
    ".hsc",
    ".chs",
    ".x",
    ".y",
]


HASKELL_BOOT_EXTENSIONS = [
    ".hs-boot",
    ".lhs-boot",
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
