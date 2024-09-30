{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations -Wno-orphans #-}

module Cardano.Benchmarking.Command
(
  runCommand
, commandParser -- for tests
)
where

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

import           Cardano.Benchmarking.Compiler (compileOptions)
import           Cardano.Benchmarking.LogTypes (AsyncBenchmarkControl (..), BenchTracers (..),
                   EnvConsts (..), TraceBenchTxSubmit (..))
import           Cardano.Benchmarking.Script (parseScriptFileAeson, runScript)
import           Cardano.Benchmarking.Script.Aeson (parseJSONFile, prettyPrint)
import           Cardano.Benchmarking.Script.Env as Env (emptyEnv, newEnvConsts)
import           Cardano.Benchmarking.Script.Selftest (runSelftest)
import           Cardano.Benchmarking.Version as Version
import           Cardano.TxGenerator.PlutusContext (readScriptData)
import           Cardano.TxGenerator.Setup.NixService
import           Cardano.TxGenerator.Types (TxGenPlutusParams (..))
import           Data.Aeson (fromJSON)
import           Data.ByteString.Lazy as BSL
import           Data.Foldable (for_)
import           Data.Maybe (catMaybes)
import           Data.Text as T
import           Data.Text.IO as T
import           Options.Applicative as Opt
import           Ouroboros.Network.NodeToClient (IOManager, withIOManager)

import           System.Exit

#ifdef UNIX
import           Cardano.Logging as Tracer (traceWith)
import           Control.Concurrent as Conc (killThread, myThreadId)
import           Control.Concurrent as Weak (mkWeakThreadId)
import           Control.Concurrent.Async as Async (cancelWith)
import           Control.Concurrent.STM as STM (readTVar)
import           Control.Monad.STM as STM (atomically)
import           Data.Foldable as Fold (forM_)
import           Data.List as List (unwords)
import           Data.Time.Format as Time (defaultTimeLocale, formatTime)
import           Data.Time.Clock.System as Time (getSystemTime, systemToUTCTime)
import           Foreign.C (Errno(..))
import           GHC.Weak as Weak (deRefWeak)

import           System.Posix.Signals as Sig (Handler (CatchInfo),
                   SignalInfo (..), SignalSpecificInfo (..), installHandler,
                   sigINT, sigTERM)
#if MIN_VERSION_base(4,18,0)
import           Data.Maybe as Maybe (fromMaybe)
import           GHC.Conc.Sync as Conc (threadLabel)
#endif
#endif

#ifdef UNIX
deriving instance Show Errno
deriving instance Show SignalInfo
deriving instance Show SignalSpecificInfo
#endif

data Command
  = Json FilePath
  | JsonHL FilePath (Maybe FilePath) (Maybe FilePath)
  | Compile FilePath
  | Selftest (Maybe FilePath)
  | VersionCmd

runCommand :: IO ()
runCommand = withIOManager runCommand'

runCommand' :: IOManager -> IO ()
runCommand' iocp = do
  envConsts <- installSignalHandler
  cmd <- customExecParser
           (prefs showHelpOnEmpty)
           (info commandParser mempty)
  case cmd of
    Json actionFile -> do
      script <- parseScriptFileAeson actionFile
      runScript emptyEnv script envConsts >>= handleError . fst
    JsonHL nixSvcOptsFile nodeConfigOverwrite cardanoTracerOverwrite -> do
      opts <- parseJSONFile fromJSON nixSvcOptsFile
      finalOpts <- mangleTracerConfig cardanoTracerOverwrite <$> mangleNodeConfig nodeConfigOverwrite opts
      let consts = envConsts { envNixSvcOpts = Just finalOpts }

      Prelude.putStrLn $
          "--> initial options:\n" ++ show opts ++
        "\n--> final options:\n" ++ show finalOpts

      quickTestPlutusDataOrDie finalOpts

      case compileOptions finalOpts of
        Right script -> runScript emptyEnv script consts >>= handleError . fst
        err -> die $ "tx-generator:Cardano.Command.runCommand JsonHL: " ++ show err
    Compile file -> do
      o <- parseJSONFile fromJSON file
      case compileOptions o of
        Right script -> BSL.putStr $ prettyPrint script
        Left err -> die $ "tx-generator:Cardano.Command.runCommand Compile: " ++ show err
    Selftest outFile -> runSelftest emptyEnv envConsts outFile >>= handleError
    VersionCmd -> runVersionCommand
  where
  handleError :: Show a => Either a b -> IO ()
  handleError = \case
    Right _  -> exitSuccess
    Left err -> die $ "tx-generator:Cardano.Command.runCommand handleError: " ++ show err
  installSignalHandler :: IO EnvConsts
  installSignalHandler = do
    -- The main thread does not appear in the set of asyncs.
    wkMainTID <- Weak.mkWeakThreadId =<< myThreadId
    envConsts@EnvConsts { .. } <- STM.atomically $ newEnvConsts iocp Nothing
    abc <- STM.atomically $ STM.readTVar envThreads
    _ <- pure (abc, wkMainTID)
#ifdef UNIX
    let signalHandler = Sig.CatchInfo signalHandler'
        signalHandler' sigInfo = do
          tid <- Conc.myThreadId
          utcTime <- Time.systemToUTCTime <$> Time.getSystemTime
          -- It's meant to match Cardano.Tracers.Handlers.Logs.Utils
          -- The hope was to avoid the package dependency.
          let formatTimeStamp = formatTime' "%Y-%m-%dT%H-%M-%S"
              formatTime' = Time.formatTime Time.defaultTimeLocale
              timeStamp = formatTimeStamp utcTime
#if MIN_VERSION_base(4,18,0)
          maybeLabel <- Conc.threadLabel tid
          let labelStr' :: String
              labelStr' = fromMaybe "(thread label unset)" maybeLabel
#else
              labelStr' = "(base version insufficient to read thread label)"
#endif
              labelStr  :: String
              labelStr  = List.unwords [ timeStamp
                                       , labelStr'
                                       , show tid
                                       , "received signal"
                                       , show sigInfo ]
              errorToThrow :: IOError
              errorToThrow = userError labelStr
              tag = TraceBenchTxSubError . T.pack
              traceWith' msg = do
                mBenchTracer <- STM.atomically do readTVar benchTracers
                case mBenchTracer of
                  Nothing -> pure ()
                  Just tracers -> do
                    let wrappedMsg = tag msg
                        submittedTracers = btTxSubmit_ tracers
                    Tracer.traceWith submittedTracers wrappedMsg

          Prelude.putStrLn labelStr
          traceWith' labelStr
          mABC <- STM.atomically $ STM.readTVar envThreads
          case mABC of
            Nothing -> do
              -- Catching a signal at this point makes it a higher than
              -- average risk of the tracer not being initialized, so
              -- this pursues some alternatives.
              let errMsg = "Signal received before AsyncBenchmarkControl creation."
              Prelude.putStrLn errMsg
              traceWith' errMsg
            Just AsyncBenchmarkControl { .. } -> do
              abcFeeder `Async.cancelWith` errorToThrow
              Fold.forM_ abcWorkers \work -> do
                work `Async.cancelWith` errorToThrow
              -- The main thread does __NOT__ appear in the above list.
              -- In order to kill that off, this, or some equivalent,
              -- absolutely /must/ be done separately.
          mapM_ Conc.killThread =<< Weak.deRefWeak wkMainTID
    Fold.forM_ [Sig.sigINT, Sig.sigTERM] $ \sig ->
           Sig.installHandler sig signalHandler Nothing
#endif
    pure envConsts

  mangleNodeConfig :: Maybe FilePath -> NixServiceOptions -> IO NixServiceOptions
  mangleNodeConfig fp opts = case (getNodeConfigFile opts, fp) of
    (_      , Just newFilePath) -> return $ setNodeConfigFile opts newFilePath
    (Just _ , Nothing) -> return opts
    (Nothing, Nothing) -> die "No node-configFile set"

  mangleTracerConfig ::  Maybe FilePath -> NixServiceOptions -> NixServiceOptions
  mangleTracerConfig traceSocket opts
    = opts { _nix_cardanoTracerSocket = traceSocket <> _nix_cardanoTracerSocket opts}

-- if there's a parsing error wrt. ScriptData, we want to fail early, before the splitting phase
quickTestPlutusDataOrDie :: NixServiceOptions -> IO ()
quickTestPlutusDataOrDie NixServiceOptions{_nix_plutus} = do
  for_ files test
  Prelude.putStrLn $
    "--> success: quickTestPlutusDataOrDie " ++ show files
  where
    test file =
      readScriptData file >>= \case
        Left err -> die $ "quickTestPlutusDataOrDie (" ++ file ++ "): " ++ show err
        Right{}  -> pure ()

    files = case _nix_plutus of
      Just PlutusOn{plutusDatum, plutusRedeemer}  -> catMaybes [plutusDatum, plutusRedeemer]
      _ -> []

commandParser :: Parser Command
commandParser
  = subparser (
       cmdParser "json" jsonCmd "Run a generic benchmarking script."
    <> cmdParser "json_highlevel" jsonHLCmd "Run the tx-generator using a flat config."
    <> cmdParser "compile" compileCmd "Compile flat-options to benchmarking script."
    <> cmdParser "selftest" selfTestCmd "Run a build-in selftest."
    <> cmdParser "version" versionCmd "Show the tx-generator version"
       )
 where
  cmdParser cmd parser description = command cmd $ info parser $ progDesc description

  filePath :: String -> Parser String
  filePath helpMsg = strArgument (metavar "FILEPATH" <> help helpMsg)

  jsonCmd :: Parser Command
  jsonCmd = Json <$> filePath "low-level benchmarking script"

  jsonHLCmd :: Parser Command
  jsonHLCmd = JsonHL <$> filePath "benchmarking options"
                     <*> nodeConfigOpt
                     <*> tracerConfigOpt
  compileCmd :: Parser Command
  compileCmd = Compile <$> filePath "benchmarking options"

  selfTestCmd = Selftest <$> optional (filePath "output file")

  nodeConfigOpt :: Parser (Maybe FilePath)
  nodeConfigOpt = option (Just <$> str)
    ( long "nodeConfig"
      <> short 'n'
      <> metavar "FILENAME"
      <> value Nothing
      <> help "the node configfile"
    )

  tracerConfigOpt :: Parser (Maybe FilePath)
  tracerConfigOpt = option (Just <$> str)
    ( long "cardano-tracer"
      <> short 'n'
      <> metavar "SOCKET"
      <> value Nothing
      <> help "the cardano-tracer socket"
    )

  versionCmd :: Parser Command
  versionCmd = pure VersionCmd

runVersionCommand :: IO ()
runVersionCommand = T.putStrLn $ multilineVersionMsg txGeneratorVersion
