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

import           Data.Aeson (fromJSON)
import           Data.ByteString.Lazy as BSL
import           Data.Text.IO as T
import           Options.Applicative as Opt

import           Ouroboros.Network.NodeToClient (withIOManager)

import           Cardano.Benchmarking.Compiler (compileOptions)
import           Cardano.Benchmarking.Script (parseScriptFileAeson, runScript)
import           Cardano.Benchmarking.Script.Aeson (parseJSONFile, prettyPrint)
import           Cardano.Benchmarking.Script.Selftest (runSelftest)
import           Cardano.Benchmarking.Version as Version
import           Cardano.TxGenerator.Setup.NixService


data Command
  = Json FilePath
  | JsonHL FilePath (Maybe FilePath) (Maybe FilePath)
  | Compile FilePath
  | Selftest (Maybe FilePath)
  | VersionCmd

runCommand :: IO ()
runCommand = withIOManager $ \iocp -> do
  cmd <- customExecParser
           (prefs showHelpOnEmpty)
           (info commandParser mempty)
  case cmd of
    Json file -> do
      script <- parseScriptFileAeson file
      runScript script iocp >>= handleError
    JsonHL file nodeConfigOverwrite cardanoTracerOverwrite -> do
      opts <- parseJSONFile fromJSON file
      finalOpts <- mangleTracerConfig cardanoTracerOverwrite <$> mangleNodeConfig nodeConfigOverwrite opts

      Prelude.putStrLn $
          "--> initial options:\n" ++ show opts ++
        "\n--> final options:\n" ++ show finalOpts

      case compileOptions finalOpts of
        Right script -> runScript script iocp >>= handleError
        err -> handleError err
    Compile file -> do
      o <- parseJSONFile fromJSON file
      case compileOptions o of
        Right script -> BSL.putStr $ prettyPrint script
        err -> handleError err
    Selftest outFile -> runSelftest iocp outFile >>= handleError
    VersionCmd -> runVersionCommand
  where
  handleError :: Show a => Either a b -> IO ()
  handleError = \case
    Right _  -> exitSuccess
    Left err -> die $ show err

  mangleNodeConfig :: Maybe FilePath -> NixServiceOptions -> IO NixServiceOptions
  mangleNodeConfig fp opts = case (getNodeConfigFile opts, fp) of
    (_      , Just newFilePath) -> return $ setNodeConfigFile opts newFilePath
    (Just _ , Nothing) -> return opts
    (Nothing, Nothing) -> die "No node-configFile set"

  mangleTracerConfig ::  Maybe FilePath -> NixServiceOptions -> NixServiceOptions
  mangleTracerConfig traceSocket opts
    = opts { _nix_cardanoTracerSocket = traceSocket <> _nix_cardanoTracerSocket opts}

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
