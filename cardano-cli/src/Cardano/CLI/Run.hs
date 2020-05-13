
-- | Dispatch for running all the CLI commands
module Cardano.CLI.Run
  ( ClientCommand(..)
  , runClientCommand
  ) where

import           Cardano.Prelude

import qualified Data.Text as Text
import           Control.Monad.Trans.Except (ExceptT)

import           Cardano.CLI.Errors (CliError(..))
import           Cardano.CLI.Commands (ClientCommand(..))
import           Cardano.CLI.Byron.Run (runByronClientCommand)
import           Cardano.CLI.Shelley.Run (runShelleyClientCommand)

import           Data.Version (showVersion)
import           Paths_cardano_cli (version)
import           System.Info (arch, compilerName, compilerVersion, os)
import           Cardano.Config.GitRev (gitRev)


runClientCommand :: ClientCommand -> ExceptT CliError IO ()
runClientCommand (ByronCommand   c) = runByronClientCommand c
runClientCommand (ShelleyCommand c) = runShelleyClientCommand c
runClientCommand DisplayVersion     = runDisplayVersion


runDisplayVersion :: ExceptT CliError IO ()
runDisplayVersion = do
    liftIO . putTextLn $ mconcat
                [ "cardano-cli ", renderVersion version
                , " - ", Text.pack os, "-", Text.pack arch
                , " - ", Text.pack compilerName, "-", renderVersion compilerVersion
                , "\ngit rev ", gitRev
                ]
  where
    renderVersion = Text.pack . showVersion

