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

def _srcs_arg():
    return {
        "srcs": attrs.named_set(attrs.source(allow_directory = True), sorted = False, default = [], doc = """
    Either a list or a map of the source files which Buck makes available to the shell
     command at the path in the `SRCDIR` environment variable.
     If you specify a list, the source files are the names in the list.
     If you specify a map, the source files are made available as the names in
     the keys of the map, where the values of the map are the original source
     file names.
"""),
    }

def _cmd_arg():
    return {
        "cmd": attrs.option(attrs.arg(), default = None, doc = """
    The shell command to run to generate the output file. It is the
     fallback for `bash` and `cmd_exe` arguments. The following environment variables are populated by
     Buck and available to the shell command. They are accessed using
     the syntax:


    ```
    ${<variable>}
    ```


     Example:


    ```
    ${SRCS}
    ```

    `${SRCS}`


     A string expansion of the `srcs` argument delimited
     by the `environment_expansion_separator` argument
     where each element of `srcs` will be translated
     into a relative path.


    `${SRCDIR}`


     The relative path to a directory to which sources are copied
     prior to running the command.


    `${OUT}`


     The output file or directory for the `genrule()`.
     This variable will have whatever value is specified by
     the `out` argument if not using named outputs. If
     using named outputs, this variable will be the output directory.


     The value should be a valid filepath. The semantics of the shell
     command determine whether this filepath is treated as a file or a
     directory. If the filepath is a directory, then the shell command
     needs to create it if not using named outputs. Otherwise, it will
     be automatically created. All outputs (directories and files) must
     be readable, writable, and (in the case of directories) executable
     by the current user.


     The file or directory specified by this variable must always
     be written by this command. If not, the execution of this
     rule will be considered a failure, halting the build process.


    `${TMP}`


     A temporary directory which can be used for intermediate
     results and will not be bundled into the output.

"""),
    }

def _bash_arg():
    return {
        "bash": attrs.option(attrs.arg(), default = None, doc = """
    A platform-specific version of the shell command parameter `cmd`.
     It runs on Linux and UNIX systems—including OSX—on which `bash` is installed.
     It has a higher priority than `cmd`. The `bash` argument is run with `/usr/bin/env bash -c`.
     It has access to the same set of macros and variables as the `cmd` argument.
"""),
    }

def _cmd_exe_arg():
    return {
        "cmd_exe": attrs.option(attrs.arg(), default = None, doc = """
    A platform-specific version of the shell command parameter `cmd`. It runs on Windows and has a higher
     priority than `cmd`. The `cmd_exe` argument is run with `cmd.exe /v:off /c`.
     It has access to the same set of macros and variables as the `cmd` argument.
"""),
    }

def _weight_arg():
    return {
        "weight": attrs.option(attrs.int(), default = None, doc = """
    How many local slots these genrule should take when executing locally.
"""),
    }

def _out_arg():
    return {
        "out": attrs.option(attrs.string(), default = None, doc = """
    The name of the output file or directory. The complete path to this
     argument is provided to the shell command through
     the `OUT` environment variable.
"""),
    }

def _type_arg():
    return {
        "type": attrs.option(attrs.string(), default = None, doc = """
    Specifies the *type* of this genrule. This is used for logging
     and is particularly useful for grouping genrules that share an
     underlying logical "type".


     For example, if you have the following `cxx_genrule` defined
     in the root directory of your Buck project



    ```


    cxx_genrule(
      name = 'cxx_gen',
      type = 'epilog',
      cmd  = 'touch finish.txt; cp finish.txt $OUT',
      out  = 'finish.txt'
    )

    ```



     then the following `buck query` command



    ```


    buck query "attrfilter( type, 'epilog', '//...' )"

    ```



     returns



    ```


    //:cxx_gen

    ```
"""),
    }

def _environment_expansion_separator():
    return {
        "environment_expansion_separator": attrs.option(attrs.string(), default = None, doc = """
    The delimiter between paths in environment variables, such as SRCS, that can contain multiple paths.
     It can be useful to specify this parameter if the paths could contain spaces.
"""),
    }

genrule_common = struct(
    srcs_arg = _srcs_arg,
    cmd_arg = _cmd_arg,
    bash_arg = _bash_arg,
    cmd_exe_arg = _cmd_exe_arg,
    out_arg = _out_arg,
    type_arg = _type_arg,
    weight_arg = _weight_arg,
    environment_expansion_separator = _environment_expansion_separator,
)
