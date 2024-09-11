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

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (AsyncException (..), Exception (..), IOException, throwIO)
import qualified Control.Monad.Catch as MC
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_, traverse_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.List (dropWhileEnd)
import Data.String (fromString)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8Lenient)
import qualified Data.Text.Lazy as LazyText
import qualified Data.Vector as Vector
import qualified GHC
import GHC (
  DynFlags (..),
  Ghc,
  GhcException (..),
  GhcLink (LinkBinary),
  GhcMode (OneShot),
  Phase,
  Severity (SevIgnore),
  getSessionDynFlags,
  parseTargetFiles,
  prettyPrintGhcErrors,
  pushLogHookM,
  setSessionDynFlags,
  )
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Config.Logger (initLogFlags)
import GHC.Driver.Env (HscEnv (..))
import GHC.Driver.Errors (printOrThrowDiagnostics)
import GHC.Driver.Errors.Types (DriverMessages, GhcMessage (GhcDriverMessage))
import GHC.Driver.Monad (modifySession, withSession)
import GHC.Driver.Phases (StopPhase (NoStop))
import GHC.Driver.Pipeline (oneShot)
import GHC.Driver.Session (FlushOut (..), defaultFatalMessager, defaultFlushOut, targetProfile)
import GHC.Iface.Binary (CheckHiWay (IgnoreHiWay), TraceBinIFace (QuietBinIFace), readBinIface)
import GHC.Runtime.Interpreter (Interp)
import GHC.Runtime.Loader (initializeSessionPlugins)
import GHC.Types.Error (MessageClass (..), getCaretDiagnostic, mkLocMessageWarningGroups)
import GHC.Types.SourceError (SourceError)
import GHC.Types.SrcLoc (Located, mkGeneralLocated, unLoc)
import GHC.Types.Unique.Supply (initUniqSupply)
import GHC.Unit.Module.ModIface (mi_final_exts, mi_mod_hash)
import GHC.Utils.Logger (LogAction, LogFlags (..), Logger, getLogger, log_default_dump_context, setLogFlags)
import GHC.Utils.Outputable (
  blankLine,
  empty,
  getPprStyle,
  ppr,
  renderWithContext,
  setStyleColoured,
  text,
  withPprStyle,
  ($$),
  ($+$),
  )
import Network.GRPC.HighLevel.Generated
import Prelude hiding (log)
import System.Environment (getProgName, lookupEnv)
import System.Exit (ExitCode)
import System.FilePath (dropExtension)
import System.IO (BufferMode (LineBuffering), hPutStrLn, hSetBuffering, stderr, stdout)
import Worker

data Args =
  Args {
    abiOut :: Maybe String,
    buck2Dep :: Maybe String,
    buck2PackageDb :: [String],
    buck2PackageDbDep :: Maybe String,
    ghcDirFile :: Maybe String,
    ghcOptions :: [String]
  }
  deriving stock (Eq, Show)

emptyArgs :: Args
emptyArgs =
  Args {
    abiOut = Nothing,
    buck2Dep = Nothing,
    buck2PackageDb = [],
    buck2PackageDbDep = Nothing,
    ghcDirFile = Nothing,
    ghcOptions = []
  }

parseBuckArgs :: [String] -> Either String Args
parseBuckArgs =
  spin emptyArgs
  where
    spin Args {..} = \case
      "--abi-out" : rest -> takeArg "--abi-out" rest \ v -> Args {abiOut = Just v, ..}
      "--buck2-dep" : rest -> takeArg "--buck2-dep" rest \ v -> Args {buck2Dep = Just v, ..}
      "--buck2-packagedb" : rest -> takeArg "--buck2-packagedb" rest \ v -> Args {buck2PackageDb = v : buck2PackageDb, ..}
      "--buck2-packagedb-dep" : rest -> takeArg "--buck2-packagedb-dep" rest \ v -> Args {buck2PackageDbDep = Just v, ..}
      "--ghc" : rest -> takeArg "--ghc" rest \ _ -> Args {ghcOptions = [], ..}
      "--ghc-dir" : rest -> takeArg "--ghc-dir" rest \ f -> Args {ghcOptions = [], ghcDirFile = Just f, ..}
      "-c" : rest -> spin Args {ghcOptions = ghcOptions, ..} rest
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

data AbiHash =
  AbiHash {
    path :: String,
    hash :: String
  }
  deriving stock (Eq, Show)

data CompileResult =
  CompileResult {
    abiHash :: Maybe AbiHash
  }
  deriving stock (Eq, Show)

data Cache =
  Cache {
    interp :: Interp
  }

type CacheRef = IORef (Maybe Cache)

withCache :: CacheRef -> Ghc a -> Ghc a
withCache cache prog = do
  restoreCache
  prog <* updateCache
  where
    restoreCache =
      liftIO (readIORef cache) >>= traverse_ \ Cache {interp} ->
        modifySession \ env -> env {hsc_interp = Just interp}

    updateCache =
      withSession \ HscEnv {hsc_interp} ->
        for_ hsc_interp \ interp ->
          liftIO $ writeIORef cache (Just Cache {interp})

runSession :: Args -> ([Located String] -> Ghc (Maybe a)) -> IO (Maybe a)
runSession args prog = do
  topdir <- readPath args.ghcDirFile
  GHC.runGhc topdir do
    handleExceptions Nothing (prog (map loc args.ghcOptions))
  where
    readPath = fmap (fmap (dropWhileEnd ('\n' ==))) . traverse readFile
    loc = mkGeneralLocated "by Buck2"

parseFlags :: [Located String] -> Ghc (DynFlags, Logger, [Located String], DriverMessages)
parseFlags argv = do
  dflags0 <- GHC.getSessionDynFlags
  let dflags1 = dflags0 {ghcMode = OneShot, ghcLink = LinkBinary, verbosity = 1}
  logger1 <- getLogger
  let logger2 = setLogFlags logger1 (initLogFlags dflags1)
  (dflags, fileish_args, dynamicFlagWarnings) <- GHC.parseDynamicFlags logger2 dflags1 argv
  pure (dflags, setLogFlags logger2 (initLogFlags dflags), fileish_args, dynamicFlagWarnings)

data Log =
  Log {
    diagnostics :: [String],
    other :: [String]
  }
  deriving stock (Eq, Show)

data Env =
  Env {
    log :: MVar Log,
    cache :: CacheRef,
    args :: Args
  }

logToState :: MVar Log -> LogAction
logToState logVar logflags msg_class srcSpan msg = case msg_class of
  MCOutput -> logOther msg
  MCDump -> logOther (msg $$ blankLine)
  MCInteractive -> logOther msg
  MCInfo -> logError msg
  MCFatal -> logError msg
  MCDiagnostic SevIgnore _ _ -> pure ()
  MCDiagnostic _sev _rea _code -> printDiagnostics
  where
    message = mkLocMessageWarningGroups (log_show_warn_groups logflags) msg_class srcSpan msg

    printDiagnostics = do
      caretDiagnostic <-
        if log_show_caret logflags
        then getCaretDiagnostic msg_class srcSpan
        else pure empty
      logError $ getPprStyle $ \style ->
        withPprStyle (setStyleColoured True style) (message $+$ caretDiagnostic $+$ blankLine)

    logError =
      logWith \ Log {diagnostics, other} new ->
        Log {diagnostics = new : diagnostics, other}

    logOther =
      logWith \ Log {diagnostics, other} new ->
        Log {diagnostics = diagnostics, other = new : other}

    logWith f d =
      modifyMVar_ logVar \ log ->
        let new = renderWithContext (log_default_user_context logflags) (d $$ text "")
        in pure (f log new)

withGhc :: Env -> ([(String, Maybe Phase)] -> Ghc (Maybe a)) -> IO (Maybe a)
withGhc Env {log, cache, args} prog =
  runSession args \ argv -> do
    pushLogHookM (const (logToState log))
    (dflags0, logger, fileish_args, dynamicFlagWarnings) <- parseFlags argv
    prettyPrintGhcErrors logger do
      let flagWarnings' = GhcDriverMessage <$> dynamicFlagWarnings
      liftIO $ printOrThrowDiagnostics logger (initPrintConfig dflags0) (initDiagOpts dflags0) flagWarnings'
      let (dflags1, srcs, _objs) = parseTargetFiles dflags0 (map unLoc fileish_args)
      setSessionDynFlags dflags1
      dflags <- getSessionDynFlags
      liftIO $ initUniqSupply (initialUnique dflags) (uniqueIncrement dflags)
      initializeSessionPlugins
      withCache cache do
        prog srcs

compile :: Args -> [(String, Maybe Phase)] -> Ghc (Maybe CompileResult)
compile args srcs = do
  hsc_env <- GHC.getSession
  liftIO (oneShot hsc_env NoStop srcs)
  abiHash <- readAbiHash hsc_env args.abiOut
  pure (Just CompileResult {abiHash})
  where
    readAbiHash HscEnv {hsc_dflags, hsc_NC} (Just path) = do
      let hi_file = dropExtension path
      iface <- liftIO $ readBinIface (targetProfile hsc_dflags) hsc_NC IgnoreHiWay QuietBinIFace hi_file
      pure (Just (AbiHash {path, hash = dump hsc_dflags (mi_mod_hash (mi_final_exts iface))}))

    readAbiHash _ _ = pure Nothing

    dump dflags = renderWithContext (log_default_dump_context (initLogFlags dflags)) . ppr

writeResult :: Args -> Maybe CompileResult -> IO Int32
writeResult args = \case
  Nothing -> pure 1
  Just CompileResult {abiHash} -> do
    for_ abiHash \ AbiHash {path, hash} -> writeFile path hash
    for_ args.buck2Dep \ path -> writeFile path "\n"
    for_ args.buck2PackageDbDep \ path ->
      case args.buck2PackageDb of
        [] -> writeFile path "\n"
        dbs -> writeFile path (unlines dbs)
    pure 0

executeHandler ::
  CacheRef ->
  ServerRequest Normal ExecuteCommand ExecuteResponse ->
  IO (ServerResponse Normal ExecuteResponse)
executeHandler cache (ServerNormalRequest _ ExecuteCommand {executeCommandArgv}) = do
  hPutStrLn stderr (unlines argv)
  args <- either (throwIO . userError) pure (parseBuckArgs argv)
  log <- newMVar Log {diagnostics = [], other = []}
  result <- withGhc Env {log, cache, args} (compile args)
  executeResponseExitCode <- writeResult args result
  Log {diagnostics, other} <- readMVar log
  traverse_ (hPutStrLn stderr) (reverse other)
  let
    response = ExecuteResponse {
      executeResponseExitCode,
      executeResponseStderr = mconcat (LazyText.pack <$> reverse diagnostics)
      }
  pure (ServerNormalResponse response [] StatusOk "")
  where
    argv = Text.unpack . decodeUtf8Lenient <$> Vector.toList executeCommandArgv

execHandler ::
  ServerRequest ClientStreaming ExecuteEvent ExecuteResponse ->
  IO (ServerResponse ClientStreaming ExecuteResponse)
execHandler (ServerReaderRequest _metadata _recv) = do
  hPutStrLn stderr "Received Exec"
  error "not implemented"

handlers :: CacheRef -> Worker ServerRequest ServerResponse
handlers cache =
  Worker
    { workerExecute = executeHandler cache,
      workerExec = execHandler
    }

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  socket <- lookupEnv "WORKER_SOCKET"
  hPutStrLn stderr $ "using worker socket: " <> show socket
  cache <- newIORef Nothing
  workerServer (handlers cache) (maybe id setSocket socket defaultServiceOptions)
  where
    setSocket s options = options {serverHost = fromString ("unix://" <> s <> "\x00"), serverPort = 0}
