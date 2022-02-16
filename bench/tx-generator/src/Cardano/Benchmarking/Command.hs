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
import           Cardano.Benchmarking.NixOptions (parseNixServiceOptions)
import           Cardano.Benchmarking.Script (runScript, parseScriptFileAeson)
import           Cardano.Benchmarking.Script.Aeson (prettyPrint)

data Command
  = Json FilePath
  | JsonHL FilePath
  | Compile FilePath

runCommand :: IO ()
runCommand = withIOManager $ \iocp -> do
  cmd <- customExecParser
           (prefs showHelpOnEmpty)
           (info commandParser mempty)
  case cmd of
    Json file -> do
      script <- parseScriptFileAeson file
      runScript script iocp >>= handleError
    JsonHL file -> do
      o <- parseNixServiceOptions file
      case compileOptions o of
        Right script -> runScript script iocp >>= handleError
        err -> handleError err
    Compile file -> do
      o <- parseNixServiceOptions file
      case compileOptions o of
        Right script -> BSL.putStr $ prettyPrint script
        err -> handleError err
  where
  handleError :: Show a => Either a b -> IO ()
  handleError = \case
    Right _  -> exitSuccess
    Left err -> die $ show err

commandParser :: Parser Command
commandParser
  = subparser (jsonCmd <> jsonHLCmd <> compileCmd)
 where
  jsonCmd = command "json"
    (Json <$> info (strArgument (metavar "FILEPATH"))
      (  progDesc "tx-generator run JsonScript"
      <> fullDesc
      <> header "tx-generator - run a generic benchmarking script"
      )
    )
  jsonHLCmd = command "json_highlevel"
    (JsonHL <$> info (strArgument (metavar "FILEPATH"))
      (  progDesc "tx-generator run Options"
      <> fullDesc
      <> header "tx-generator - run flat-options"
      )
    )
  compileCmd = command "compile"
    (Compile <$> info (strArgument (metavar "FILEPATH"))
      (  progDesc "tx-generator compile Options"
      <> fullDesc
      <> header "tx-generator - compile flat-options to benchmarking script"
      )
    )
