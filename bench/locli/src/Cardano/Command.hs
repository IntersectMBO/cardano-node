module Cardano.Command (module Cardano.Command) where

import Prelude (String)
import Cardano.Prelude

import Control.Monad.Trans.Except.Extra (firstExceptT)
import Data.Text                        (pack)
import Options.Applicative
import Options.Applicative              qualified as Opt

import Cardano.Analysis.ChainFilter
import Cardano.Analysis.Driver
import Cardano.Analysis.Ground
import Cardano.Analysis.Run
import Cardano.Analysis.Version
import Cardano.Util


-- | Sub-commands
data Command =
  -- | Analysis commands
    AnalysisCommand
      (Maybe JsonGenesisFile)
      (Maybe JsonRunMetafile)
      [JsonFilterFile]
      AnalysisCommand
      [JsonLogfile]
  deriving Show

data CommandError
  = AnalysisError AnalysisCommand AnalysisCmdError
  deriving Show

renderCommandError :: CommandError -> Text
renderCommandError (AnalysisError cmd err) =
  renderAnalysisCmdError cmd err

runCommand :: Command -> ExceptT CommandError IO ()
runCommand (AnalysisCommand shelleyGenesis runmeta chainFilters c logfiles) = do
  progress "locli-version" (J getVersion)

  maybeRun <- firstExceptT (AnalysisError c) $
              -- Discharge the optionality:
              sequence $
               readRun
               <$> shelleyGenesis
               <*> runmeta

  filters <- forM chainFilters $ \f ->
               firstExceptT (AnalysisError c . ChainFiltersParseError f . pack)
                 (readChainFilter f)
  unless (null filters) $
    progress "filters" (J $ filters <&> unFilterName . snd)

  firstExceptT (AnalysisError c) $
    runAnalysisCommand maybeRun filters logfiles c

parseCommand :: Parser Command
parseCommand =
  asum
  [ AnalysisCommand
    <$> optional
          (argJsonGenesisFile "genesis"      "Genesis file of the run")
    <*> optional
          (argJsonRunMetafile "run-metafile" "The meta.json file from the benchmark run")
    <*> many
          (argChainFilterset "filter"
            "List of block/slot selection criteria, as JSON file")
    <*> subparser
        (mconcat
         [ commandGroup "Log analysis"
         , metavar "COMMAND"
         , command'
           "analyse"
           "Log analysis"
           parseAnalysisCommand
         ])
    <*> many
          argJsonLogfile
  ]
  where
    command' :: String -> String -> Parser a -> Mod CommandFields a
    command' c descr p =
      command c $ info (p <**> helper) $
        mconcat [ progDesc descr ]

opts :: ParserInfo Command
opts =
  Opt.info (parseCommand <**> Opt.helper)
    ( Opt.fullDesc
      <> Opt.header
      "locli - parse JSON log files, as emitted by cardano-node."
    )

pref :: ParserPrefs
pref = Opt.prefs showHelpOnEmpty
