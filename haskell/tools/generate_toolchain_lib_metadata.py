#!/usr/bin/env python3

"""Helper script to generate metadata about Haskell targets provided by the Haskell toolchain.

The result is a JSON object with the following fields:
* `exposed_modules`: List of modules exposed by the package.
"""

import argparse
import sys
import json
import shlex
import subprocess

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--package-name", required=True, type=str, help="Read info about this package."
    )
    parser.add_argument(
        "--package-dir",
        required=False,
        type=str,
        help="Read package info from this directory.",
    )
    parser.add_argument(
        "--output",
        required=True,
        type=argparse.FileType("w"),
        help="Write package metadata to this file in JSON format.",
    )

    args = parser.parse_args()
    result = obtain_lib_metadata(args.package_name, args.package_dir)

    json.dump(result, args.output, indent=4, default=json_default_handler)


def json_default_handler(o):
    if isinstance(o, set):
        return sorted(o)
    raise TypeError(f"Object of type {o.__class__.__name__} is not JSON serializable")


def obtain_lib_metadata(package_name, pkgdb):
    exposed_modules = determine_exposed_modules(package_name, pkgdb)
    return {"exposed_modules": exposed_modules}


def determine_exposed_modules(package_name, pkgdb):
    package_data = run_ghc_pkg(
        "field", pkgdb, args=[package_name, "exposed-modules", "--simple-output"]
    )
    return package_data.split()


def run_ghc_pkg(cmd, pkgdb=None, args=[]):
    outer_args = ["ghc-pkg", cmd] + args
    if pkgdb:
        outer_args += [f"--package-db={pkgdb}"]

    res = subprocess.run(outer_args, capture_output=True)

    if res.returncode != 0:
        print(shlex.join(args), file=sys.stderr)

    if res.returncode != 0:
        # Fail if ghc-pkg failed.
        sys.exit(res.returncode)

    return res.stdout.decode("utf-8")


if __name__ == "__main__":
    main()
