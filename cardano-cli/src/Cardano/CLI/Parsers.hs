module Cardano.CLI.Parsers
  ( ClientCommand
  , parseClientCommand
  ) where

import           Cardano.Prelude

import           Options.Applicative

import           Cardano.CLI.Commands
import           Cardano.Common.Parsers
import           Cardano.CLI.Byron.Parsers   (parseByronCommands)
import           Cardano.CLI.Shelley.Parsers (parseShelleyCommands)


parseClientCommand :: Parser ClientCommand
parseClientCommand =
  asum
    [ parseByron
    , parseShelley
    , parseDisplayVersion
    ]

parseByron :: Parser ClientCommand
parseByron =
  fmap ByronCommand $
  subparser $ mconcat
    [ commandGroup "Byron specific commands"
    , metavar "Byron specific commands"
    , parseByronCommands
    ]

parseShelley :: Parser ClientCommand
parseShelley =
  subparser $ mconcat
    [ commandGroup "Shelley specific commands"
    , metavar "Shelley specific commands"
    , command'
        "shelley"
        "Shelley specific commands"
        $ ShelleyCommand <$> parseShelleyCommands
    ]

parseDisplayVersion :: Parser ClientCommand
parseDisplayVersion =
  subparser $ mconcat
    [ commandGroup "Miscellaneous commands"
    , metavar "Miscellaneous commands"
    , command'
      "version"
      "Show cardano-cli version"
      $ pure DisplayVersion
    ]


