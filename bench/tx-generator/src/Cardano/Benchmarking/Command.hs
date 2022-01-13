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

import           Options.Applicative as Opt

import           Ouroboros.Network.NodeToClient (withIOManager)

import           Cardano.Benchmarking.Script (runScript, parseScriptFileAeson)

newtype Command = Json FilePath

runCommand :: IO ()
runCommand = withIOManager $ \iocp -> do
  cmd <- customExecParser
           (prefs showHelpOnEmpty)
           (info commandParser mempty)
  case cmd of
    Json file     -> do
      script <- parseScriptFileAeson file
      runScript script iocp >>= handleError
  where
  handleError :: Show a => Either a b -> IO ()
  handleError = \case
    Right _  -> exitSuccess
    Left err -> die $ show err

commandParser :: Parser Command
commandParser
  = subparser jsonCmd
 where
  jsonCmd = command "json"
    (Json <$> info (strArgument (metavar "FILEPATH"))
      (  progDesc "tx-generator run JsonScript"
      <> fullDesc
      <> header "tx-generator - run a generic benchmarking script"
      )
    )
