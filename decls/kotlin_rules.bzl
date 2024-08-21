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

load(":common.bzl", "AbiGenerationMode", "AnnotationProcessingTool", "LogLevel", "SourceAbiVerificationMode", "TestType", "UnusedDependenciesAction", "buck", "prelude_rule")
load(":jvm_common.bzl", "jvm_common")
load(":re_test_common.bzl", "re_test_common")

kotlin_library = prelude_rule(
    name = "kotlin_library",
    docs = """
        A `kotlin_library()` rule is used to define a set of Kotlin
        files that can be compiled together. The main output of a
         `kotlin_library()` rule is a single JAR file containing all
        of the compiled class files, as well as the static files specified in
        the `resources` argument.
    """,
    examples = """
        ```

        # A rule that compiles a single .kt file.
        kotlin_library(
          name = 'JsonUtil',
          srcs = ['JsonUtil.kt'],
          deps = [
            '//third_party/guava:guava',
            '//third_party/jackson:jackson',
          ],
        )

        # A rule that compiles all of the .kt files under the directory in
        # which the rule is defined using glob(). It also excludes an
        # individual file that may have additional dependencies, so it is
        # compiled by a separate rule.
        kotlin_library(
          name = 'messenger',
          srcs = glob(['**/*.kt'], excludes = ['MessengerModule.kt']),
          deps = [
            '//src/com/facebook/base:base',
            '//third_party/guava:guava',
          ],
        )

        kotlin_library(
          name = 'MessengerModule',
          srcs = ['MessengerModule.kt'],
          deps = [
            '//src/com/facebook/base:base',
            '//src/com/google/inject:inject',
            '//third_party/guava:guava',
            '//third_party/jsr-330:jsr-330',
          ],
        )

        # A rule that builds a library with both relative and
        # fully-qualified deps.
        kotlin_library(
          name = 'testutil',
          srcs = glob(['tests/**/*.kt'], excludes = 'tests/**/*Test.kt'),
          deps = [
            ':lib-fb4a',
            '//java/com/facebook/base:base',
          ],
        )

        ```
    """,
    further = None,
    attrs = (
        # @unsorted-dict-items
        {
            "srcs": attrs.list(attrs.source(), default = [], doc = """
                The set of `.kt`, `.java` or `.kts` files to compile for this rule.
                 If any of the files in this list end in `.src.zip`,
                 then the entries in the ZIP file that end in `.java` or `.kt` will be
                 included as ordinary inputs to compilation.
            """),
        } |
        jvm_common.resources_arg() |
        {
            "deps": attrs.list(attrs.dep(), default = [], doc = """
                Rules (usually other `kotlin_library` rules) that are used to
                 generate the classpath required to compile this `kotlin_library`.
            """),
            "kotlin_compiler_plugins": attrs.dict(key = attrs.source(), value = attrs.dict(key = attrs.string(), value = attrs.string(), sorted = False), sorted = False, default = {}, doc = """
                Use this to specify [Kotlin compiler plugins](https://kotlinlang.org/docs/reference/compiler-plugins.html) to use when compiling this library.
                 This takes a map, with each entry specify one plugin. Entry's key is plugin source path,
                 and value is a map of plugin option key value pair. Unlike `extra_kotlinc_arguments`,
                 these can be *source paths*, not just strings.

                 A special option value is
                 `__codegen_dir__`, in which case Buck will provide a default codegen folder's path as
                 option value instead.
                 E.g.

                ```

                kotlin_compiler_plugins = {
                    "somePluginSourcePath": {
                        "plugin:somePluginId:somePluginOptionKey": "somePluginOptionValue",
                        "plugin:somePluginId:someDirectoryRelatedOptionKey": "__codegen_dir__",
                    },
                },

                ```
                Each plugin source path will be prefixed with `-Xplugin=` and passed as extra
                 arguments to the compiler. Plugin options will be appended after its plugin with `-P`.

                 A specific example is, if you want to use [kotlinx.serialization](https://github.com/Kotlin/kotlinx.serialization)
                 with `kotlin_library()`, you need to specify `kotlinx-serialization-compiler-plugin.jar` under `kotlin_compiler_plugins` and `kotlinx-serialization-runtime.jar` (which you may have to fetch from Maven) in your `deps`:

                ```

                kotlin_library(
                    name = "example",
                    srcs = glob(["*.kt"]),
                    deps = [
                        ":kotlinx-serialization-runtime",
                    ],
                    kotlin_compiler_plugins = {
                        # Likely copied from your $KOTLIN_HOME directory.
                        "kotlinx-serialization-compiler-plugin.jar": {},
                    },
                )

                prebuilt_jar(
                    name = "kotlinx-serialization-runtime",
                    binary_jar = ":kotlinx-serialization-runtime-0.10.0",
                )

                # Note you probably want to set
                # maven_repo=http://jcenter.bintray.com/ in your .buckconfig until
                # https://github.com/Kotlin/kotlinx.serialization/issues/64
                # is closed.
                remote_file(
                    name = "kotlinx-serialization-runtime-0.10.0",
                    out = "kotlinx-serialization-runtime-0.10.0.jar",
                    url = "mvn:org.jetbrains.kotlinx:kotlinx-serialization-runtime:jar:0.10.0",
                    sha1 = "23d777a5282c1957c7ce35946374fff0adab114c"
                )

                ```
            """),
            "extra_kotlinc_arguments": attrs.list(attrs.string(), default = [], doc = """
                List of additional arguments to pass into the Kotlin compiler.
            """),
            "friend_paths": attrs.list(attrs.dep(), default = [], doc = """
                List of source paths to pass into the Kotlin compiler as friend-paths, that is, modules
                 you can have access to internal methods.
            """),
            "annotation_processing_tool": attrs.option(attrs.enum(AnnotationProcessingTool), default = None, doc = """
                Specifies the tool to use for annotation processing. Possible values: "kapt" or "javac".
                 "kapt" allows running Java annotation processors against Kotlin sources while backporting
                 it for Java sources too.
                 "javac" works only against Java sources, Kotlin sources won't have access to generated
                 classes at compile time.
            """),
        } |
        jvm_common.remove_classes_arg() |
        jvm_common.exported_deps() |
        jvm_common.provided_deps() |
        jvm_common.exported_provided_deps() |
        jvm_common.k2() |
        jvm_common.incremental() |
        buck.labels_arg() |
        {
            "abi_generation_mode": attrs.option(attrs.enum(AbiGenerationMode), default = None),
            "annotation_processor_deps": attrs.list(attrs.dep(), default = []),
            "annotation_processor_params": attrs.list(attrs.string(), default = []),
            "annotation_processors": attrs.list(attrs.string(), default = []),
            "contacts": attrs.list(attrs.string(), default = []),
            "default_host_platform": attrs.option(attrs.configuration_label(), default = None),
            "extra_arguments": attrs.list(attrs.string(), default = []),
            "java_version": attrs.option(attrs.string(), default = None),
            "javac": attrs.option(attrs.source(), default = None),
            "jar_postprocessor": attrs.option(attrs.exec_dep(), default = None),
            "licenses": attrs.list(attrs.source(), default = []),
            "manifest_file": attrs.option(attrs.source(), default = None),
            "maven_coords": attrs.option(attrs.string(), default = None),
            "never_mark_as_unused_dependency": attrs.option(attrs.bool(), default = None),
            "on_unused_dependencies": attrs.option(attrs.enum(UnusedDependenciesAction), default = None),
            "plugins": attrs.list(attrs.dep(), default = []),
            "proguard_config": attrs.option(attrs.source(), default = None),
            "required_for_source_only_abi": attrs.bool(default = False),
            "runtime_deps": attrs.list(attrs.dep(), default = []),
            "source": attrs.option(attrs.string(), default = None),
            "source_abi_verification_mode": attrs.option(attrs.enum(SourceAbiVerificationMode), default = None),
            "source_only_abi_deps": attrs.list(attrs.dep(), default = []),
            "target": attrs.option(attrs.string(), default = None),
            "use_jvm_abi_gen": attrs.option(attrs.bool(), default = None),
            "_wip_java_plugin_arguments": attrs.dict(attrs.label(), attrs.list(attrs.string()), default = {}),
        }
    ),
)

kotlin_test = prelude_rule(
    name = "kotlin_test",
    docs = """
        A `kotlin_test()` rule is used to define a set of
         `.kt` files that contain tests to run via JUnit.
    """,
    examples = None,
    further = None,
    attrs = (
        # @unsorted-dict-items
        {
            "srcs": attrs.list(attrs.source(), default = [], doc = """
                Like ``kotlin_library()``,
                 all of the `.kt` files specified by the
                 `srcs` argument will be compiled when this rule is
                 built. In addition, all of the corresponding `.class` files that are built by this rule will be passed as arguments to
                 JUnit when this rule is run as a test. `.class` files
                 that are passed to JUnit that do not have any methods annotated with
                 `@Test` are considered failed tests, so make sure that
                 only test case classes are specified as `srcs`. This is
                 frequently done by specifying `srcs` as
                 `glob(['**/*Test.kt'])`.
            """),
            "resources": attrs.list(attrs.source(), default = [], doc = """
                Same as `kotlin_library()`.
            """),
        } |
        buck.test_label_arg() |
        {
            "deps": attrs.list(attrs.dep(), default = [], doc = """
                Same as `kotlin_library()`.
                 // org.junit.rules.Timeout was not introduced until 4.7.
                 Must include JUnit (version 4.7 or later) as a dependency for JUnit tests.
                 Must include TestNG (version 6.2 or later) and hamcrest as a dependencies for TestNG tests.
            """),
            "test_type": attrs.option(attrs.enum(TestType), default = None, doc = """
                Specifies which test framework to use.
                 The currently supported options are 'junit' and 'testng'.
            """),
        } |
        buck.run_test_separately_arg(run_test_separately_type = attrs.bool(default = False)) |
        buck.fork_mode() |
        re_test_common.test_args() |
        buck.test_rule_timeout_ms() |
        {
            "std_out_log_level": attrs.option(attrs.one_of(attrs.enum(LogLevel), attrs.int()), default = None, doc = """
                Log level for messages from the source under test that buck will output to
                 std out.
                 Value must be a valid `java.util.logging.Level` value.
            """),
            "std_err_log_level": attrs.option(attrs.one_of(attrs.enum(LogLevel), attrs.int()), default = None, doc = """
                Same as `std_out_log_level`, but for std err.
            """),
            "vm_args": attrs.list(attrs.arg(), default = [], doc = """
                Runtime arguments to the JVM running the tests.
            """),
        } |
        jvm_common.k2() |
        jvm_common.incremental() |
        jvm_common.test_env() |
        {
            "abi_generation_mode": attrs.option(attrs.enum(AbiGenerationMode), default = None),
            "annotation_processing_tool": attrs.option(attrs.enum(AnnotationProcessingTool), default = None),
            "annotation_processor_deps": attrs.list(attrs.dep(), default = []),
            "annotation_processor_params": attrs.list(attrs.string(), default = []),
            "annotation_processors": attrs.list(attrs.string(), default = []),
            "contacts": attrs.list(attrs.string(), default = []),
            "cxx_library_whitelist": attrs.list(attrs.dep(), default = []),
            "default_cxx_platform": attrs.option(attrs.string(), default = None),
            "default_host_platform": attrs.option(attrs.configuration_label(), default = None),
            "deps_query": attrs.option(attrs.query(), default = None),
            "exported_deps": attrs.list(attrs.dep(), default = []),
            "exported_provided_deps": attrs.list(attrs.dep(), default = []),
            "extra_arguments": attrs.list(attrs.string(), default = []),
            "extra_kotlinc_arguments": attrs.list(attrs.string(), default = []),
            "friend_paths": attrs.list(attrs.dep(), default = []),
            "java_version": attrs.option(attrs.string(), default = None),
            "java": attrs.option(attrs.dep(), default = None),
            "javac": attrs.option(attrs.source(), default = None),
            "kotlin_compiler_plugins": attrs.dict(key = attrs.source(), value = attrs.dict(key = attrs.string(), value = attrs.string(), sorted = False), sorted = False, default = {}),
            "licenses": attrs.list(attrs.source(), default = []),
            "manifest_file": attrs.option(attrs.source(), default = None),
            "maven_coords": attrs.option(attrs.string(), default = None),
            "never_mark_as_unused_dependency": attrs.option(attrs.bool(), default = None),
            "on_unused_dependencies": attrs.option(attrs.enum(UnusedDependenciesAction), default = None),
            "plugins": attrs.list(attrs.dep(), default = []),
            "proguard_config": attrs.option(attrs.source(), default = None),
            "provided_deps": attrs.list(attrs.dep(), default = []),
            "remove_classes": attrs.list(attrs.regex(), default = []),
            "required_for_source_only_abi": attrs.bool(default = False),
            "resources_root": attrs.option(attrs.source(), default = None),
            "runtime_deps": attrs.list(attrs.dep(), default = []),
            "source": attrs.option(attrs.string(), default = None),
            "source_abi_verification_mode": attrs.option(attrs.enum(SourceAbiVerificationMode), default = None),
            "source_only_abi_deps": attrs.list(attrs.dep(), default = []),
            "target": attrs.option(attrs.string(), default = None),
            "test_case_timeout_ms": attrs.option(attrs.int(), default = None),
            "unbundled_resources_root": attrs.option(attrs.source(allow_directory = True), default = None),
            "use_cxx_libraries": attrs.option(attrs.bool(), default = None),
            "use_dependency_order_classpath": attrs.option(attrs.bool(), default = None),
            "use_jvm_abi_gen": attrs.option(attrs.bool(), default = None),
            "_wip_java_plugin_arguments": attrs.dict(attrs.label(), attrs.list(attrs.string()), default = {}),
        }
    ),
)

kotlin_rules = struct(
    kotlin_library = kotlin_library,
    kotlin_test = kotlin_test,
)
