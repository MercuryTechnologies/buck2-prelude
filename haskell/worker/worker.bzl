worker_libs = [
    "base",
    "bytestring",
    "containers",
    "deepseq",
    "exceptions",
    "filepath",
    "ghc",
    "grpc-haskell",
    "proto3-suite",
    "proto3-wire",
    "text",
    "vector",
    "unix",
]

worker_srcs = [
    "@prelude//haskell/worker/impl/plugin/src:Internal/AbiHash.hs",
    "@prelude//haskell/worker/impl/plugin/src:Internal/Args.hs",
    "@prelude//haskell/worker/impl/plugin/src:Internal/Cache.hs",
    "@prelude//haskell/worker/impl/plugin/src:Internal/Compile.hs",
    "@prelude//haskell/worker/impl/plugin/src:Internal/Error.hs",
    "@prelude//haskell/worker/impl/plugin/src:Internal/Log.hs",
    "@prelude//haskell/worker/impl/plugin/src:Internal/Session.hs",
    "@prelude//haskell/worker/impl/buck-worker:Args.hs",
    "@prelude//haskell/worker/impl/buck-worker:Main.hs",
    "@prelude//haskell/worker/impl/buck-worker:BuckWorker.hs",
]

worker_flags = [
    "-Wall",
    "-XGHC2021",
    "-XBlockArguments",
    "-XDerivingStrategies",
    "-XRecordWildCards",
    "-XDuplicateRecordFields",
    "-XOverloadedRecordDot",
    "-XStrictData",
    "-XNoFieldSelectors",
    "-XLambdaCase",
]
