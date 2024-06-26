{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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
import           Cardano.Benchmarking.Script (parseScriptFileAeson, runScript)
import           Cardano.Benchmarking.Script.Aeson (parseJSONFile, prettyPrint)
import           Cardano.Benchmarking.Script.Env as Env (Env (Env, envThreads), mkNewEnv)
import           Cardano.Benchmarking.Script.Selftest (runSelftest)
import           Cardano.Benchmarking.Version as Version
import           Cardano.TxGenerator.PlutusContext (readScriptData)
import           Cardano.TxGenerator.Setup.NixService
import           Cardano.TxGenerator.Types (TxGenPlutusParams (..))
import           Ouroboros.Network.NodeToClient (withIOManager)

import           Prelude

import           Data.Aeson (fromJSON)
import           Data.ByteString.Lazy as BSL
import           Data.Foldable (for_)
import           Data.Maybe (catMaybes)
import           Data.Text.IO as T
import           Options.Applicative as Opt
import           System.Exit

#ifdef UNIX
import           Control.Concurrent as Conc (myThreadId)
import           Control.Concurrent.Async as Async (cancelWith)
import           Control.Concurrent.STM as STM (readTVar)
import           Control.Monad.STM as STM (atomically)

import           Data.Foldable as Fold (forM_)
import           Data.List as List (unwords)
import           Data.Map as Map (lookup)
import           Data.Time.Format as Time (defaultTimeLocale, formatTime)
import           Data.Time.Clock.System as Time (getSystemTime, systemToUTCTime)
import           System.Posix.Signals as Sig (Handler (CatchInfoOnce), SignalInfo (..), SignalSpecificInfo (..), installHandler, sigINT, sigTERM)
#if MIN_VERSION_base(4,18,0)
import           Data.Maybe as Maybe (fromMaybe)
import           GHC.Conc.Sync as Conc (threadLabel)
#endif
#endif

#ifdef UNIX
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
runCommand = withIOManager $ \iocp -> do
  env <- installSignalHandler
  cmd <- customExecParser
           (prefs showHelpOnEmpty)
           (info commandParser mempty)
  case cmd of
    Json file -> do
      script <- parseScriptFileAeson file
      runScript env script iocp >>= handleError . fst
    JsonHL file nodeConfigOverwrite cardanoTracerOverwrite -> do
      opts <- parseJSONFile fromJSON file
      finalOpts <- mangleTracerConfig cardanoTracerOverwrite <$> mangleNodeConfig nodeConfigOverwrite opts

      Prelude.putStrLn $
          "--> initial options:\n" ++ show opts ++
        "\n--> final options:\n" ++ show finalOpts

      quickTestPlutusDataOrDie finalOpts

      case compileOptions finalOpts of
        Right script -> runScript env script iocp >>= handleError . fst
        err -> die $ "tx-generator:Cardano.Command.runCommand JsonHL: " ++ show err
    Compile file -> do
      o <- parseJSONFile fromJSON file
      case compileOptions o of
        Right script -> BSL.putStr $ prettyPrint script
        Left err -> die $ "tx-generator:Cardano.Command.runCommand Compile: " ++ show err
    Selftest outFile -> runSelftest env iocp outFile >>= handleError
    VersionCmd -> runVersionCommand
  where
  handleError :: Show a => Either a b -> IO ()
  handleError = \case
    Right _  -> exitSuccess
    Left err -> die $ "tx-generator:Cardano.Command.runCommand handleError: " ++ show err
  installSignalHandler :: IO Env
  installSignalHandler = do
    env@Env { .. } <- STM.atomically mkNewEnv
    Just abcTVar <- pure $ "tx-submit-benchmark" `Map.lookup` envThreads
    abc <- STM.atomically $ STM.readTVar abcTVar
    _ <- pure abc
#ifdef UNIX
    let signalHandler = Sig.CatchInfoOnce signalHandler'
        signalHandler' sigInfo = do
          tid <- Conc.myThreadId
          Just (throttler, workers, _, _) <- STM.atomically $ STM.readTVar abcTVar
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

          Prelude.putStrLn labelStr
          Async.cancelWith throttler errorToThrow
          Fold.forM_ workers \work -> do
            Async.cancelWith work errorToThrow
    Fold.forM_ [Sig.sigINT, Sig.sigTERM] $ \sig ->
           Sig.installHandler sig signalHandler Nothing
#endif
    pure env

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
