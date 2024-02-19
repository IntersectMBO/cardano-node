{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Parsers.Run
  ( commands
  , runTestnetCmd
  , pref
  , opts
  ) where

import           Cardano.CLI.Environment

import           Data.Foldable
import           Options.Applicative
import qualified Options.Applicative as Opt

import           Testnet.Property.Run
import           Testnet.Start.Cardano

import           Parsers.Cardano
import           Parsers.Help
import           Parsers.Version

pref :: ParserPrefs
pref = Opt.prefs $ showHelpOnEmpty <> showHelpOnError

opts :: EnvCli -> ParserInfo CardanoTestnetCommands
opts envCli = Opt.info (commands envCli <**> helper) idm

-- TODO: Remove StartBabbageTestnet and StartShelleyTestnet
-- by allowing the user to start testnets in any era (excluding Byron)
-- via StartCardanoTestnet
data CardanoTestnetCommands
  = StartCardanoTestnet CardanoTestnetOptions
  | GetVersion VersionOptions
  | Help ParserPrefs (ParserInfo CardanoTestnetCommands) HelpOptions

commands :: EnvCli -> Parser CardanoTestnetCommands
commands envCli =
  asum
    [ fmap StartCardanoTestnet (subparser (cmdCardano envCli))
    , fmap GetVersion (subparser cmdVersion)
    , fmap (Help pref (opts envCli)) (subparser cmdHelp)
    ]


runTestnetCmd :: CardanoTestnetCommands -> IO ()
runTestnetCmd = \case
  StartCardanoTestnet cmdOpts -> runCardanoOptions cmdOpts
  GetVersion cmdOpts -> runVersionOptions cmdOpts
  Help pPrefs pInfo cmdOpts -> runHelpOptions pPrefs pInfo cmdOpts


runCardanoOptions :: CardanoTestnetOptions -> IO ()
runCardanoOptions options =
  runTestnet $ cardanoTestnetDefault options
