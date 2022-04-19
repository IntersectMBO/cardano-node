{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations -Wno-orphans #-}

module Cardano.Benchmarking.Command
(
  runCommand
, commandParser -- for tests
)
where

import           Prelude
import           System.Exit

import           Data.ByteString.Lazy as BSL
import           Options.Applicative as Opt

import           Ouroboros.Network.NodeToClient (withIOManager)

import           Cardano.Benchmarking.Compiler (compileOptions)
import           Cardano.Benchmarking.NixOptions (NixServiceOptions, parseNixServiceOptions, setNodeConfigFile, _nix_nodeConfigFile)
import           Cardano.Benchmarking.Script (runScript, parseScriptFileAeson)
import           Cardano.Benchmarking.Script.Aeson (prettyPrint)
import           Cardano.Benchmarking.Script.Selftest (runSelftest)

data Command
  = Json FilePath
  | JsonHL FilePath (Maybe FilePath)
  | Compile FilePath
  | Selftest FilePath

runCommand :: IO ()
runCommand = withIOManager $ \iocp -> do
  cmd <- customExecParser
           (prefs showHelpOnEmpty)
           (info commandParser mempty)
  case cmd of
    Json file -> do
      script <- parseScriptFileAeson file
      runScript script iocp >>= handleError
    JsonHL file nodeConfigOverwrite -> do
      opts <- parseNixServiceOptions file
      finalOpts <- mangleNodeConfig opts nodeConfigOverwrite
      case compileOptions finalOpts of
        Right script -> runScript script iocp >>= handleError
        err -> handleError err
    Compile file -> do
      o <- parseNixServiceOptions file
      case compileOptions o of
        Right script -> BSL.putStr $ prettyPrint script
        err -> handleError err
    Selftest outFile -> runSelftest iocp (Just outFile) >>= handleError
 where
  handleError :: Show a => Either a b -> IO ()
  handleError = \case
    Right _  -> exitSuccess
    Left err -> die $ show err

  mangleNodeConfig :: NixServiceOptions -> Maybe FilePath -> IO NixServiceOptions
  mangleNodeConfig opts fp = case (_nix_nodeConfigFile opts, fp) of
    (_      , Just newFilePath) -> return $ setNodeConfigFile opts newFilePath
    (Just _ , Nothing) -> return opts
    (Nothing, Nothing) -> die "No node-configFile set"

commandParser :: Parser Command
commandParser
  = subparser (
       cmdParser "json" jsonCmd "Run a generic benchmarking script."
    <> cmdParser "json_highlevel" jsonHLCmd "Run the tx-generator using a flat config."
    <> cmdParser "compile" compileCmd "Compile flat-options to benchmarking script."
    <> cmdParser "selftest" selfTestCmd "Run a build-in selftest."
       )
 where
  cmdParser cmd parser description = command cmd $ info parser $ progDesc description
  jsonCmd = Json <$> filePath "low-level benchmarking script"

  jsonHLCmd = JsonHL <$> filePath "benchmarking options"
                     <*> nodeConfigOpt

  compileCmd = Compile <$> filePath "benchmarking options"

  selfTestCmd = Selftest <$> filePath "output file"

  filePath helpMsg = strArgument (metavar "FILEPATH" <> help helpMsg)

  nodeConfigOpt :: Parser (Maybe FilePath)
  nodeConfigOpt = option (Just <$> str)
    ( long "nodeConfig"
      <> short 'n'
      <> metavar "FILENAME"
      <> value Nothing
      <> help "the node configfile"
    )

