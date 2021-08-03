{-# LANGUAGE GADTs #-}

-- | Dispatch for running all the CLI commands
module Cardano.CLI.Run
  ( ClientCommand(..)
  , ClientCommandErrors
  , renderClientCommandError
  , runClientCommand
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra (firstExceptT)
import           Data.String
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Cardano.CLI.Byron.Commands (ByronCommand)
import           Cardano.CLI.Byron.Run (ByronClientCmdError, renderByronClientCmdError,
                   runByronClientCommand)
import           Cardano.CLI.Shelley.Commands (ShelleyCommand)
import           Cardano.CLI.Shelley.Run (ShelleyClientCmdError, renderShelleyClientCmdError,
                   runShelleyClientCommand)

import           Cardano.CLI.Render (customRenderHelp)

import           Cardano.Config.Git.Rev (gitRev)
import           Data.Version (showVersion)
import           Paths_cardano_cli (version)
import           System.Info (arch, compilerName, compilerVersion, os)
import           Options.Applicative.Types (Option (..), OptReader (..), Parser (..), ParserInfo (..), ParserPrefs (..))
import           Options.Applicative.Help.Core

import qualified Data.List as L
import qualified System.IO as IO

-- | Sub-commands of 'cardano-cli'.
data ClientCommand =

    -- | Byron Related Commands
    ByronCommand ByronCommand

    -- | Shelley Related Commands
  | ShelleyCommand ShelleyCommand

    -- | Shelley-related commands that have been parsed under the
    -- now-deprecated \"shelley\" subcommand.
  | DeprecatedShelleySubcommand ShelleyCommand

  | forall a. Help ParserPrefs (ParserInfo a)
  | DisplayVersion

data ClientCommandErrors
  = ByronClientError ByronClientCmdError
  | ShelleyClientError ShelleyCommand ShelleyClientCmdError
  deriving Show

runClientCommand :: ClientCommand -> ExceptT ClientCommandErrors IO ()
runClientCommand (ByronCommand c) = firstExceptT ByronClientError $ runByronClientCommand c
runClientCommand (ShelleyCommand c) = firstExceptT (ShelleyClientError c) $ runShelleyClientCommand c
runClientCommand (DeprecatedShelleySubcommand c) =
  firstExceptT (ShelleyClientError c)
    $ runShelleyClientCommandWithDeprecationWarning
    $ runShelleyClientCommand c
runClientCommand (Help pprefs allParserInfo) = runHelp pprefs allParserInfo
runClientCommand DisplayVersion = runDisplayVersion

renderClientCommandError :: ClientCommandErrors -> Text
renderClientCommandError (ByronClientError err) =
  renderByronClientCmdError err
renderClientCommandError (ShelleyClientError cmd err) =
  renderShelleyClientCmdError cmd err

-- | Combine an 'ExceptT' that will write a warning message to @stderr@ with
-- the provided 'ExceptT'.
ioExceptTWithWarning :: MonadIO m => Text -> ExceptT e m () -> ExceptT e m ()
ioExceptTWithWarning warningMsg e =
  liftIO (Text.hPutStrLn stderr warningMsg) >> e

-- | Used in the event that Shelley-related commands are run using the
-- now-deprecated \"shelley\" subcommand.
runShelleyClientCommandWithDeprecationWarning
  :: MonadIO m
  => ExceptT e m ()
  -> ExceptT e m ()
runShelleyClientCommandWithDeprecationWarning =
    ioExceptTWithWarning warningMsg
  where
    warningMsg :: Text
    warningMsg =
      "WARNING: The \"shelley\" subcommand is now deprecated and will be "
        <> "removed in the future. Please use the top-level commands instead."

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


helpAll :: ParserPrefs -> String -> [String] -> ParserInfo a -> IO ()
helpAll pprefs progn rnames parserInfo = do
  IO.putStrLn $ customRenderHelp 80 (usage_help parserInfo)
  IO.putStrLn ""
  go (infoParser parserInfo)
  where go :: Parser a -> IO ()
        go p = case p of
          NilP _ -> return ()
          OptP optP -> case optMain optP of
            CmdReader _ cs f -> do
              forM_ cs $ \c ->
                forM_ (f c) $ \subParserInfo -> 
                  helpAll pprefs progn (c:rnames) subParserInfo
            _ -> return ()
          AltP pa pb -> go pa >> go pb
          MultP pf px -> go pf >> go px
          BindP pa _ -> go pa
        usage_help i =
              mconcat
              [ usageHelp (pure . parserUsage pprefs (infoParser i) . L.unwords $ progn : reverse rnames)
              , descriptionHelp (infoProgDesc i)
              ]

runHelp :: ParserPrefs -> ParserInfo a -> ExceptT ClientCommandErrors IO ()
runHelp pprefs allParserInfo = liftIO $ helpAll pprefs "cardano-cli" [] allParserInfo
