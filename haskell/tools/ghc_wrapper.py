#!/usr/bin/env python3

"""Wrapper script to call ghc.

It accepts a dep file where all used inputs are written to. For any passed ABI
hash file, the corresponding interface is marked as unused, so these can change
without triggering compilation actions.

"""

import argparse
import os
from pathlib import Path
import subprocess
import sys
import json
import tempfile


def main():
    parser = argparse.ArgumentParser(
        description=__doc__, add_help=False, fromfile_prefix_chars="@"
    )
    parser.add_argument(
        "--buck2-dep",
        required=True,
        help="Path to the dep file.",
    )
    parser.add_argument(
        "--buck2-non-hs-dep",
        required=False,
        type=Path,
        help="Path to dep file for non-haskell sources")
    parser.add_argument(
        "--buck2-packagedb-dep",
        required=True,
        help="Path to the dep file.",
    )
    parser.add_argument(
        "--usagefiles",
        required=True,
        help="Path to the usagefiles.hs script",
    )
    parser.add_argument(
        "--buck2-package-db",
        required=False,
        nargs="*",
        default=[],
        help="Path to a package db that is used during the module compilation",
    )
    parser.add_argument(
        "--ghc", required=True, type=str, help="Path to the Haskell compiler GHC."
    )
    parser.add_argument(
        "--abi-out",
        required=True,
        type=Path,
        help="Output path of the abi file to create.",
    )
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
        "--extra-env-key",
        type=str,
        action="append",
        default=[],
        help="Extra environment variable name",
    )
    parser.add_argument(
        "--extra-env-value",
        type=str,
        action="append",
        default=[],
        help="Extra environment variable value",
    )

    args, ghc_args = parser.parse_known_args()

    cmd = [args.ghc] + ghc_args

    aux_paths = [str(binpath) for binpath in args.bin_path if binpath.is_dir()] + [str(os.path.dirname(binexepath)) for binexepath in args.bin_exe]
    env = os.environ.copy()
    path = env.get("PATH", "")
    env["PATH"] = os.pathsep.join([path] + aux_paths)

    extra_env_keys = [str(k) for k in args.extra_env_key]
    extra_env_values = [str(v) for v in args.extra_env_value]
    assert len(extra_env_keys) == len(extra_env_values), "number of --extra-env-key and --extra-env-value flags must match"
    n_extra_env = len(extra_env_keys)
    if n_extra_env > 0:
        for i in range(0, n_extra_env):
            k = extra_env_keys[i]
            v = extra_env_values[i]
            env[k] = v

    # Note, Buck2 swallows stdout on successful builds.
    # Redirect to stderr to avoid this.
    returncode = subprocess.call(cmd, env=env, stdout=sys.stderr.buffer)
    if returncode != 0:
        return returncode

    # after compilation mark non-haskell sources as used
    if args.buck2_non_hs_dep:
        if "-ohi" in cmd:
            ohi_index = cmd.index("-ohi")
            hi_file = cmd[ohi_index+1]
        elif "-dynohi" in cmd:
            dynohi_index = cmd.index("-dynohi")
            hi_file = cmd[dynohi_index+1]

        usage_files = subprocess.check_output([
            args.ghc,
            "-package-env", "-",
            "-package", "ghc",
            "--run",
            args.usagefiles,
            "--",
            hi_file
        ], env=env, text=True)

        try:
            with open(args.buck2_non_hs_dep, "w") as f:
                used_files = [used for used in usage_files.splitlines() if not Path(used).is_absolute()]
                f.write("\n".join(used_files))
        except Exception as e:
            # remove incomplete dep file
            os.remove(args.buck2_non_hs_dep)
            raise e


    recompute_abi_hash(args.ghc, args.abi_out)

    # write an empty dep file, to signal that all tagged files are unused
    try:
        with open(args.buck2_dep, "w") as f:
            f.write("\n")

    except Exception as e:
        # remove incomplete dep file
        os.remove(args.buck2_dep)
        raise e

    # write an empty dep file, to signal that all tagged files are unused
    try:
        with open(args.buck2_packagedb_dep, "w") as f:
            for db in args.buck2_package_db:
                f.write(db + "\n")
            if not args.buck2_package_db:
                f.write("\n")

    except Exception as e:
        # remove incomplete dep file
        os.remove(args.buck2_packagedb_dep)
        raise e

    return 0


def recompute_abi_hash(ghc, abi_out):
    """Call ghc on the hi file and write the ABI hash to abi_out."""
    hi_file = abi_out.with_suffix("")

    cmd = [ghc, "-v0", "-package-env=-", "--show-iface-abi-hash", hi_file]

    hash = subprocess.check_output(cmd, text=True).split(maxsplit=1)[0]

    abi_out.write_text(hash)


if __name__ == "__main__":
    main()
