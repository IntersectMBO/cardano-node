{-# LANGUAGE ImportQualifiedPost #-}
module Cardano.Unlog.Parsers
  ( opts
  , pref
  ) where

import Cardano.Prelude
import Prelude (String)

import Options.Applicative
import Options.Applicative qualified as Opt

import Cardano.Unlog.Commands
import Cardano.Unlog.Run (Command (..))

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
