module Cardano.Unlog.Parsers
  ( opts
  , pref
  ) where

import           Cardano.Prelude
import           Prelude (String)

import           Options.Applicative
import qualified Options.Applicative as Opt

import           Cardano.Unlog.Commands
import           Cardano.Unlog.Run (Command (..))

command' :: String -> String -> Parser a -> Mod CommandFields a
command' c descr p =
    command c $ info (p <**> helper)
              $ mconcat [ progDesc descr ]

opts :: ParserInfo Command
opts =
  Opt.info (parseCommand <**> Opt.helper)
    ( Opt.fullDesc
      <> Opt.header
      "locli - parse JSON log files, as emitted by cardano-node."
    )

pref :: ParserPrefs
pref = Opt.prefs showHelpOnEmpty

parseCommand :: Parser Command
parseCommand =
  asum
    [ parseAnalysis
    , parseDisplayVersion
    ]

parseAnalysis :: Parser Command
parseAnalysis =
  fmap AnalysisCommand $
  subparser $ mconcat
    [ commandGroup "Log analysis"
    , metavar "Log analysis"
    , command'
        "analyse"
        "Log analysis"
         parseAnalysisCommands
    ]

parseDisplayVersion :: Parser Command
parseDisplayVersion =
      subparser
        (mconcat
         [ commandGroup "Miscellaneous commands"
         , metavar "Miscellaneous commands"
         , command'
           "version"
           "Show the locli version"
           (pure DisplayVersion)
         ]
        )
  <|> flag' DisplayVersion
        (  long "version"
        <> help "Show the locli version"
        <> hidden
        )
