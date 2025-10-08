#!/usr/bin/env python3

"""Wrapper for ghc-pkg register
"""

import argparse
import shlex
import subprocess
import sys

def main():
    parser = argparse.ArgumentParser(description=__doc__)

    parser.add_argument(
        "--ghc-pkg",
        required=True,
        type=str,
        help="Path to ghc-pkg",
    )
    parser.add_argument(
        "--output",
        required=True,
        type=str,
        help="Output DB path",
    )
    parser.add_argument(
        "--package-conf",
        required=True,
        type=str,
        help="package.conf source",
    )

    args = parser.parse_args()
    ghc_pkg = args.ghc_pkg
    output = args.output
    package_conf = args.package_conf

    init_cmd = [ghc_pkg, "init", output]

    res = subprocess.run(init_cmd, stderr=sys.stderr.buffer)
    if res.returncode != 0:
        # Fail if ghc-pkg failed.
        sys.exit(res.returncode)

    register_cmd = [
        ghc_pkg,
        "register",
        "--package-conf",
        output,
        "--no-expand-pkgroot",
        package_conf,
        "--force",
        "-v0",
    ]

    res2 = subprocess.run(
        register_cmd,
        stderr=sys.stderr.buffer,
    )
    if res2.returncode != 0:
        # Fail if ghc-pkg failed.
        sys.exit(res2.returncode)

if __name__ == "__main__":
    main()
