#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-unsafe

import sys

from subprocess import PIPE, run

import dep_file_utils


# output_path -> path to write the dep file to
# cmd_args -> command to be run to get dependencies from compiler
# input_file -> Path to the file we're generating the dep file for. We need this since
# when generating dependencies for a file using show_headers, the output does not include
# the file itself, so we need the path to add it manually
def process_show_headers_dep_file(output_path, cmd_args, input_file):
    ret = run(cmd_args, stderr=PIPE, encoding="utf-8")
    if ret.returncode == 0:
        parse_into_dep_file(ret.stderr, output_path, input_file)
    sys.exit(ret.returncode)


def parse_into_dep_file(output, dst_path, input_file):
    """
    Convert stderr generated by clang to dep file. This will be a mix of output like:

    warning: this is a warning!
    .path/to/dep1.h
    ..path/to/dep2.h
    ...path/to/dep3.h

    and we want to get:

    path/to/dep1.h
    path/to/dep2.h
    path/to/dep3.h

    """

    lines = output.splitlines()

    deps = []
    for line in lines:
        if line.startswith("."):
            path = remove_leading_dots(line.replace(" ", ""))
            if len(path) > 0:
                deps.append(path.strip())
                continue
        print(line, file=sys.stderr)  # This was a warning/error

    deps.append(input_file)
    dep_file_utils.normalize_and_write_deps(deps, dst_path)


def remove_leading_dots(s):
    while s.startswith("."):
        s = s[1:]
    return s
