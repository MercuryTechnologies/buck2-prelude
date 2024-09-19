{-# options_ghc -Wall -Werror -Wno-error=unused-local-binds -Wno-error=unused-matches -Wno-error=unused-imports -fno-show-error-context #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

import Control.Exception (AsyncException (..), Exception (..), IOException, throwIO)
import qualified Control.Monad.Catch as MC
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.List (isPrefixOf, partition)
import Data.String (fromString)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Foldable (for_)
import qualified Data.Vector as V
import qualified GHC
import GHC (DynFlags, Ghc, GhcException (..), Phase, parseTargetFiles)
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Config.Logger (initLogFlags)
import GHC.Driver.Env (HscEnv (..))
import GHC.Driver.Errors (printOrThrowDiagnostics)
import GHC.Driver.Errors.Types (DriverMessages, GhcMessage (GhcDriverMessage))
import GHC.Driver.Main (initHscEnv)
import GHC.Driver.Monad (Session (..))
import GHC.Driver.Phases (StopPhase (NoStop))
import GHC.Driver.Pipeline (oneShot)
import GHC.Driver.Session (
  FlushOut (..),
  defaultFatalMessager,
  defaultFlushOut,
  initialUnique,
  targetProfile,
  uniqueIncrement,
  )
import GHC.Iface.Binary (CheckHiWay (IgnoreHiWay), TraceBinIFace (QuietBinIFace), readBinIface)
import GHC.Runtime.Loader (initializeSessionPlugins)
import GHC.Types.SourceError (SourceError)
import GHC.Types.SrcLoc (Located, mkGeneralLocated, unLoc)
import GHC.Types.Unique.Supply (initUniqSupply)
import GHC.Unit.Module.ModIface (mi_final_exts, mi_mod_hash)
import GHC.Utils.Logger (Logger, getLogger, log_default_dump_context, setLogFlags)
import GHC.Utils.Outputable (ppr, renderWithContext)
import Network.GRPC.HighLevel.Generated
import System.Environment (getProgName, lookupEnv)
import System.Exit (ExitCode)
import System.FilePath (dropExtension, takeDirectory)
import System.IO (BufferMode (LineBuffering), hPutStrLn, hSetBuffering, stderr, stdout)
import Worker

options :: ServiceOptions
options = defaultServiceOptions

handlers :: IORef (Maybe Session) -> Worker ServerRequest ServerResponse
handlers session =
  Worker
    { workerExecute = executeHandler session,
      workerExec = execHandler
    }

data Args =
  Args {
    abiOut :: Maybe String,
    binPaths :: [String],
    buck2Dep :: Maybe String,
    buck2PackageDb :: [String],
    buck2PackageDbDep :: Maybe String,
    ghcDir :: Maybe String,
    ghcOptions :: [String]
  }
  deriving stock (Eq, Show)

emptyArgs :: Args
emptyArgs =
  Args {
    abiOut = Nothing,
    binPaths = [],
    buck2Dep = Nothing,
    buck2PackageDb = [],
    buck2PackageDbDep = Nothing,
    ghcDir = Nothing,
    ghcOptions = []
  }

parseBuckArgs :: V.Vector ByteString -> Either String Args
parseBuckArgs =
  spin emptyArgs .
  fmap (T.unpack . decodeUtf8Lenient) .
  V.toList
  where
    spin Args {..} = \case
      "--abi-out" : rest -> takeArg "--abi-out" rest \ v -> Args {abiOut = Just v, ..}
      "--buck2-dep" : rest -> takeArg "--buck2-dep" rest \ v -> Args {buck2Dep = Just v, ..}
      "--buck2-packagedb" : rest -> takeArg "--buck2-packagedb" rest \ v -> Args {buck2PackageDb = v : buck2PackageDb, ..}
      "--buck2-packagedb-dep" : rest -> takeArg "--buck2-packagedb-dep" rest \ v -> Args {buck2PackageDbDep = Just v, ..}
      "--ghc" : rest -> takeArg "--ghc" rest \ ghc -> Args {ghcOptions = [], ghcDir = Just (takeDirectory (takeDirectory ghc)), ..}
      "--bin-path" : rest -> takeArg "--bin-path" rest \ path -> Args {binPaths = path : binPaths, ..}
      "-c" : rest -> spin Args {ghcOptions = "-no-link" : ghcOptions, ..} rest
      arg : rest -> spin Args {ghcOptions = arg : ghcOptions, ..} rest
      [] -> Right Args {ghcOptions = reverse ghcOptions, ..}

    takeArg name argv store = case argv of
      [] -> Left (name ++ " needs an argument")
      arg : rest -> spin (store arg) rest

handleExceptions :: ∀ a . a -> Ghc a -> Ghc a
handleExceptions errResult =
  MC.handle \ e -> do
    liftIO flushOut
    handler e
    pure errResult
  where
    handler exception
      | Just (se :: SourceError) <- fromException exception
      = GHC.printException se

      | Just (ioe :: IOException) <- fromException exception
      = fm (show ioe)

      | Just UserInterrupt <- fromException exception
      = liftIO $ throwIO UserInterrupt

      | Just StackOverflow <- fromException exception
      = fm "stack overflow: use +RTS -K<size> to increase it"

      | Just (ex :: ExitCode) <- fromException exception
      = liftIO $ throwIO ex

      | Just ge <- fromException exception
      = case ge of
        Signal _ -> pure ()
        ProgramError _ -> fm (show ge)
        CmdLineError _ -> fm ("<command line>: " ++ show ge)
        _ -> do
          progName <- liftIO getProgName
          fm (progName ++ ": " ++ show ge)

      | otherwise
      = fm (show (Panic (show exception)))

    fm = liftIO . defaultFatalMessager
    FlushOut flushOut = defaultFlushOut

data CompileResult =
  CompileResult {
    abiHash :: Maybe (String, String)
  }
  deriving stock (Eq, Show)

runSession :: Args -> ([Located String] -> Ghc (Maybe a)) -> IO (Maybe a)
runSession args prog =
  GHC.runGhc mbMinusB (handleExceptions Nothing (prog argv2))
  where
    argv0 = foldMap (\ d -> ["-B" ++ d ++ "/lib/ghc-9.8.2/lib"]) args.ghcDir ++ args.ghcOptions
    (minusB_args, argv1) = partition ("-B" `isPrefixOf`) argv0
    mbMinusB | null minusB_args = Nothing
             | otherwise = Just (drop 2 (last minusB_args))
    argv2 = map (mkGeneralLocated "on the commandline") argv1

parseFlags :: [Located String] -> Ghc (DynFlags, Logger, [Located String], DriverMessages)
parseFlags argv = do
  dflags0 <- GHC.getSessionDynFlags
  logger1 <- getLogger
  let logger2 = setLogFlags logger1 (initLogFlags dflags0)
  (dflags, fileish_args, dynamicFlagWarnings) <- GHC.parseDynamicFlags logger2 dflags0 argv
  pure (dflags, setLogFlags logger2 (initLogFlags dflags), fileish_args, dynamicFlagWarnings)

withGhc :: Args -> ([(String, Maybe Phase)] -> Ghc (Maybe a)) -> IO (Maybe a)
withGhc args prog =
  runSession args \ argv -> do
    (dflags0, logger, fileish_args, dynamicFlagWarnings) <- parseFlags argv
    GHC.prettyPrintGhcErrors logger do
      let flagWarnings' = GhcDriverMessage <$> dynamicFlagWarnings
      liftIO $ printOrThrowDiagnostics logger (initPrintConfig dflags0) (initDiagOpts dflags0) flagWarnings'
      let (dflags1, srcs, _objs) = parseTargetFiles dflags0 (map unLoc fileish_args)
      GHC.setSessionDynFlags dflags1
      dflags <- GHC.getSessionDynFlags
      liftIO $ initUniqSupply (initialUnique dflags) (uniqueIncrement dflags)
      initializeSessionPlugins
      prog srcs

compile :: Args -> [(String, Maybe Phase)] -> Ghc (Maybe CompileResult)
compile args srcs = do
  hsc_env <- GHC.getSession
  liftIO (oneShot hsc_env NoStop srcs)
  abiHash <- readAbiHash hsc_env args.abiOut
  pure (Just CompileResult {abiHash})
  where
    readAbiHash HscEnv {hsc_dflags, hsc_NC} (Just out) = do
      let hi_file = dropExtension out
      iface <- liftIO $ readBinIface (targetProfile hsc_dflags) hsc_NC IgnoreHiWay QuietBinIFace hi_file
      pure (Just (out, dump hsc_dflags (mi_mod_hash (mi_final_exts iface))))

    readAbiHash _ _ = pure Nothing

    dump dflags = renderWithContext (log_default_dump_context (initLogFlags dflags)) . ppr

writeResult :: Args -> Maybe CompileResult -> IO Int32
writeResult args = \case
  Nothing -> pure 1
  Just CompileResult {abiHash} -> do
    for_ abiHash \ (path, hash) -> writeFile path hash
    for_ args.buck2Dep \ path -> writeFile path "\n"
    for_ args.buck2PackageDbDep \ path ->
      case args.buck2PackageDb of
        [] -> writeFile path "\n"
        dbs -> writeFile path (unlines dbs)
    pure 0

executeHandler ::
  IORef (Maybe Session) ->
  ServerRequest Normal ExecuteCommand ExecuteResponse ->
  IO (ServerResponse Normal ExecuteResponse)
executeHandler state (ServerNormalRequest _metadata (ExecuteCommand {executeCommandArgv, executeCommandEnv})) = do
  hPutStrLn stderr (show executeCommandArgv)
  print executeCommandArgv
  print executeCommandEnv
  -- session <- ensureSession
  args <- either (throwIO . userError) pure (parseBuckArgs executeCommandArgv)
  result <- withGhc args (compile args)
  hPutStrLn stderr ("compiled: " ++ show result)
  exitCode <- writeResult args result
  pure (ServerNormalResponse (ExecuteResponse exitCode "") [] StatusOk "")
  where
    _ensureSession =
      readIORef state >>= \case
        Just s -> pure s
        Nothing -> do
          env0 <- initHscEnv Nothing
          session <- Session <$> newIORef env0
          writeIORef state (Just session)
          pure session

execHandler ::
  ServerRequest ClientStreaming ExecuteEvent ExecuteResponse ->
  IO (ServerResponse ClientStreaming ExecuteResponse)
execHandler (ServerReaderRequest _metadata _recv) = do
  hPutStrLn stderr "Received Exec"
  error "not implemented"

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  socket <- lookupEnv "WORKER_SOCKET"
  hPutStrLn stderr $ "using worker socket: " <> show socket
  state <- newIORef Nothing
  let activeOptions =
        maybe
          options
          ( \s ->
              options {serverHost = fromString $ "unix://" <> s <> "\x00", serverPort = 0}
          )
          socket
  workerServer (handlers state) activeOptions
