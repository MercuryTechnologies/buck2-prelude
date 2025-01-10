worker_libs = [
    "base",
    "binary",
    "bytestring",
    "containers",
    "deepseq",
    "directory",
    "exceptions",
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
    "@prelude//haskell/worker/impl/plugin/src:Internal/Error.hs",
    "@prelude//haskell/worker/impl/plugin/src:Internal/Log.hs",
    "@prelude//haskell/worker/impl/plugin/src:Internal/Session.hs",
    "@prelude//haskell/worker/impl/buck-worker/lib:BuckArgs.hs",
    "@prelude//haskell/worker/impl/buck-worker/lib:Proto/Worker.hs",
    "@prelude//haskell/worker/impl/buck-worker/lib:Proto/Worker_Fields.hs",
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
