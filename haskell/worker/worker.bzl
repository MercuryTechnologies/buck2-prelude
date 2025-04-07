worker_libs = [
    "async",
    "base",
    "binary",
    "bytestring",
    "containers",
    "deepseq",
    "directory",
    "exceptions",
    "extra",
    "filepath",
    "ghc",
    "grapesy",
    "network",
    "process",
    "proto-lens-runtime",
    "stm",
    "text",
    "transformers",
    "vector",
    "unix",
]

worker_srcs_shared = [
    "@prelude//haskell/worker/impl/plugin/src:Internal/AbiHash.hs",
    "@prelude//haskell/worker/impl/plugin/src:Internal/Args.hs",
    "@prelude//haskell/worker/impl/plugin/src:Internal/Cache.hs",
    "@prelude//haskell/worker/impl/plugin/src:Internal/Compile.hs",
    "@prelude//haskell/worker/impl/plugin/src:Internal/CompileHpt.hs",
    "@prelude//haskell/worker/impl/plugin/src:Internal/Debug.hs",
    "@prelude//haskell/worker/impl/plugin/src:Internal/Error.hs",
    "@prelude//haskell/worker/impl/plugin/src:Internal/Log.hs",
    "@prelude//haskell/worker/impl/plugin/src:Internal/MakeFile.hs",
    "@prelude//haskell/worker/impl/plugin/src:Internal/MakeFile/JSON.hs",
    "@prelude//haskell/worker/impl/plugin/src:Internal/Metadata.hs",
    "@prelude//haskell/worker/impl/plugin/src:Internal/Session.hs",
    "@prelude//haskell/worker/impl/buck-worker/lib:BuckArgs.hs",
    "@prelude//haskell/worker/impl/buck-worker/lib:BuckWorker.hs",
    "@prelude//haskell/worker/impl/buck-worker/lib:Proto/Instrument.hs",
]

worker_srcs = worker_srcs_shared + [
    "@prelude//haskell/worker/impl/buck-worker:Main.hs",
]

worker_srcs_multiplexer = worker_srcs_shared + [
    "@prelude//haskell/worker/impl/comm/src:Message.hs",
    "@prelude//haskell/worker/impl/server/lib:Server.hs",
    "@prelude//haskell/worker/impl/server/lib:Pool.hs",
    "@prelude//haskell/worker/impl/server/lib:Worker.hs",
    "@prelude//haskell/worker/impl/buck-multiplex-worker:Main.hs",
]

worker_flags = [
    "-Wall",
    "-XBlockArguments",
    "-XDerivingStrategies",
    "-XDuplicateRecordFields",
    "-XGHC2021",
    "-XLambdaCase",
    "-XOverloadedLists",
    "-XOverloadedRecordDot",
    "-XOverloadedStrings",
    "-XRecordWildCards",
    "-XStrictData",
]
