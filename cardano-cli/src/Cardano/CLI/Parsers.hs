{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Monoid law, left identity" -}

module Cardano.CLI.Parsers
  ( opts
  , pref
  ) where

import           Cardano.CLI.Byron.Parsers (backwardsCompatibilityCommands, parseByronCommands)
import           Cardano.CLI.Common.Parsers
import           Cardano.CLI.Environment (EnvCli)
import           Cardano.CLI.Ping (parsePingCmd)
import           Cardano.CLI.Render (customRenderHelp)
import           Cardano.CLI.Run (ClientCommand (..))
import           Cardano.CLI.Shelley.Parsers (parseShelleyCommands)

import           Data.Foldable
import           Options.Applicative

import qualified Options.Applicative as Opt


opts :: EnvCli -> ParserInfo ClientCommand
opts envCli =
  Opt.info (parseClientCommand envCli <**> Opt.helper) $ mconcat
    [ Opt.fullDesc
    , Opt.header $ mconcat
      [ "cardano-cli - General purpose command-line utility to interact with cardano-node."
      , " Provides specific commands to manage keys, addresses, build & submit transactions,"
      , " certificates, etc."
      ]
    ]

pref :: ParserPrefs
pref = Opt.prefs $ mconcat
         [ showHelpOnEmpty
         , helpHangUsageOverflow 10
         , helpRenderHelp customRenderHelp
         ]

parseClientCommand :: EnvCli -> Parser ClientCommand
parseClientCommand envCli =
  asum
    -- There are name clashes between Shelley commands and the Byron backwards
    -- compat commands (e.g. "genesis"), and we need to prefer the Shelley ones
    -- so we list it first.
    [ parseShelley envCli
    , parseByron envCli
    , parsePing
    , backwardsCompatibilityCommands envCli
    , parseDisplayVersion (opts envCli)
    ]

parseByron :: EnvCli -> Parser ClientCommand
parseByron mNetworkId =
  fmap ByronCommand $
  subparser $ mconcat
    [ commandGroup "Byron specific commands"
    , metavar "Byron specific commands"
    , command' "byron" "Byron specific commands" $ parseByronCommands mNetworkId
    ]

parsePing :: Parser ClientCommand
parsePing = CliPingCommand <$> parsePingCmd

-- | Parse Shelley-related commands at the top level of the CLI.
parseShelley :: EnvCli -> Parser ClientCommand
parseShelley envCli = ShelleyCommand <$> parseShelleyCommands envCli

-- Yes! A --version flag or version command. Either guess is right!
parseDisplayVersion :: ParserInfo a -> Parser ClientCommand
parseDisplayVersion allParserInfo =
      subparser
        (mconcat
         [ commandGroup "Miscellaneous commands"
         , metavar "Miscellaneous commands"
         , command'
           "help"
           "Show all help"
           (pure (Help pref allParserInfo))
         , command'
           "version"
           "Show the cardano-cli version"
           (pure DisplayVersion)
         ]
        )
  <|> flag' DisplayVersion
        (  long "version"
        <> help "Show the cardano-cli version"
        <> hidden
        )
