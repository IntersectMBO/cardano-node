module Cardano.CLI.Parsers
  ( parseClientCommand
  ) where

import           Cardano.Prelude

import           Options.Applicative

import           Cardano.Config.Parsers (command')
import           Cardano.CLI.Byron.Parsers   (parseByronCommands)
import           Cardano.CLI.Run (ClientCommand(..))
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
        (ShelleyCommand <$> parseShelleyCommands)
    ]

-- Yes! A --version flag or version command. Either guess is right!
parseDisplayVersion :: Parser ClientCommand
parseDisplayVersion =
      subparser
        (mconcat
         [ commandGroup "Miscellaneous commands"
         , metavar "Miscellaneous commands"
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
