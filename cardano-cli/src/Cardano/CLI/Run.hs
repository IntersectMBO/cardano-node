
-- | Dispatch for running all the CLI commands
module Cardano.CLI.Run
  ( ClientCommand(..)
  , ClientCommandErrors
  , renderClientCommandError
  , runClientCommand
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra (firstExceptT)
import qualified Data.Text as Text

import           Cardano.CLI.Byron.Commands (ByronCommand)
import           Cardano.CLI.Byron.Run (ByronClientCmdError, renderByronClientCmdError,
                     runByronClientCommand)
import           Cardano.CLI.Shelley.Commands (ShelleyCommand)
import           Cardano.CLI.Shelley.Run (ShelleyClientCmdError, renderShelleyClientCmdError,
                     runShelleyClientCommand)

import           Cardano.Config.Git.Rev (gitRev)
import           Data.Version (showVersion)
import           Paths_cardano_cli (version)
import           System.Info (arch, compilerName, compilerVersion, os)

-- | Sub-commands of 'cardano-cli'.
data ClientCommand =

    -- | Byron Related Commands
    ByronCommand ByronCommand

    -- | Shelley Related Commands
  | ShelleyCommand ShelleyCommand

  | DisplayVersion
  deriving Show

data ClientCommandErrors
  = ByronClientError ByronClientCmdError
  | ShelleyClientError ShelleyCommand ShelleyClientCmdError
  deriving Show
  --TODO: We should include an AgnosticClientError

runClientCommand :: ClientCommand -> ExceptT ClientCommandErrors IO ()
runClientCommand (ByronCommand c) = firstExceptT ByronClientError $ runByronClientCommand c
runClientCommand (ShelleyCommand c) = firstExceptT (ShelleyClientError c) $ runShelleyClientCommand c
runClientCommand DisplayVersion = runDisplayVersion

renderClientCommandError :: ClientCommandErrors -> Text
renderClientCommandError (ByronClientError err) =
  renderByronClientCmdError err
renderClientCommandError (ShelleyClientError cmd err) =
  renderShelleyClientCmdError cmd err

runDisplayVersion :: ExceptT ClientCommandErrors IO ()
runDisplayVersion = do
    liftIO . putTextLn $ mconcat
                [ "cardano-cli ", renderVersion version
                , " - ", Text.pack os, "-", Text.pack arch
                , " - ", Text.pack compilerName, "-", renderVersion compilerVersion
                , "\ngit rev ", gitRev
                ]
  where
    renderVersion = Text.pack . showVersion
