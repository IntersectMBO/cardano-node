{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE UndecidableInstances #-}

-- | CLI command types
module Cardano.Unlog.Commands (module Cardano.Unlog.Commands) where

import Prelude

import Data.Text (Text)

import Options.Applicative
import Options.Applicative qualified as Opt

import Ouroboros.Network.Block (SlotNo(..))

import Cardano.Analysis.ChainFilter
import Cardano.Unlog.LogObject hiding (Text)

--
-- Analysis CLI command data types
--

-- | All the CLI subcommands under \"analysis\".
--
data AnalysisCommand
  = MachineTimelineCmd
      JsonGenesisFile
      JsonRunMetafile
      [JsonFilterFile]
      [JsonLogfile]
      MachineTimelineOutputFiles
  | BlockPropagationCmd
      JsonGenesisFile
      JsonRunMetafile
      [JsonFilterFile]
      [JsonLogfile]
      BlockPropagationOutputFiles
  | SubstringKeysCmd
  | RunInfoCmd
      JsonGenesisFile
      JsonRunMetafile
  deriving (Show)

data MachineTimelineOutputFiles
  = MachineTimelineOutputFiles
  { mtofLogObjects         :: Maybe JsonOutputFile
  , mtofSlotStats          :: Maybe JsonOutputFile
  , mtofTimelinePretty     :: Maybe TextOutputFile
  , mtofTimelineCsv        :: Maybe  CsvOutputFile
  , mtofStatsCsv           :: Maybe  CsvOutputFile
  , mtofHistogram          :: Maybe     OutputFile
  , mtofAnalysis           :: Maybe JsonOutputFile
  , mtofDerivedVectors0Csv :: Maybe  CsvOutputFile
  , mtofDerivedVectors1Csv :: Maybe  CsvOutputFile
  }
  deriving (Show)

data BlockPropagationOutputFiles
  = BlockPropagationOutputFiles
  { bpofLogObjects         :: Maybe JsonOutputFile
  , bpofTimelinePretty     :: Maybe TextOutputFile
  , bpofAnalysis           :: Maybe JsonOutputFile
  }
  deriving (Show)

renderAnalysisCommand :: AnalysisCommand -> Text
renderAnalysisCommand sc =
  case sc of
    MachineTimelineCmd {}  -> "analyse machine-timeline"
    BlockPropagationCmd {} -> "analyse block-propagation"
    SubstringKeysCmd {}    -> "analyse substring-keys"
    RunInfoCmd {}        -> "print extracted Run information"

parseMachineTimelineOutputFiles :: Parser MachineTimelineOutputFiles
parseMachineTimelineOutputFiles =
  MachineTimelineOutputFiles
    <$> optional
        (argJsonOutputFile "logobjects-json"
           "Dump the entire input LogObject stream")
    <*> optional
        (argJsonOutputFile "slotstats-json"
           "Dump extracted per-slot summaries, as a side-effect of log analysis")
    <*> optional
        (argTextOutputFile "timeline-pretty"
           "Dump pretty timeline of extracted slot leadership summaries, as a side-effect of log analysis")
    <*> optional
        (argCsvOutputFile "timeline-csv"
           "Dump CSV of the timeline")
    <*> optional
        (argCsvOutputFile "stats-csv"
           "Dump CSV of the timeline statistics")
    <*> optional
        (argOutputFile "cpu-spans-histogram-png"
           "Write a PNG file with the CPU spans histogram")
    <*> optional
        (argJsonOutputFile "analysis-json"
           "Write analysis JSON to this file, if specified -- otherwise print to stdout.")
    <*> optional
        (argCsvOutputFile "derived-vectors-0-csv"
           "Dump CSV of vectors derived from the timeline")
    <*> optional
        (argCsvOutputFile "derived-vectors-1-csv"
           "Dump CSV of vectors derived from the timeline")

parseBlockPropagationOutputFiles :: Parser BlockPropagationOutputFiles
parseBlockPropagationOutputFiles =
  BlockPropagationOutputFiles
    <$> optional
        (argJsonOutputFile "logobjects-json"
           "Dump the entire input LogObject stream")
    <*> optional
        (argTextOutputFile "timeline-pretty"
           "Dump pretty timeline of extracted slot leadership summaries, as a side-effect of log analysis")
    <*> optional
        (argJsonOutputFile "analysis-json"
           "Write analysis JSON to this file, if specified -- otherwise print to stdout.")

pSlotNo :: String -> String -> Parser SlotNo
pSlotNo name desc =
  SlotNo <$>
    Opt.option Opt.auto
     (  Opt.long name
     <> Opt.metavar "SLOT"
     <> Opt.help desc
     )

parseAnalysisCommands :: Parser AnalysisCommand
parseAnalysisCommands =
  Opt.subparser $
    mconcat
      [ Opt.command "machine-timeline"
          (Opt.info (MachineTimelineCmd
                       <$> argJsonGenesisFile "genesis"
                              "Genesis file of the run"
                       <*> argJsonRunMetafile "run-metafile"
                              "The meta.json file from the benchmark run"
                       <*> many
                             (argJsonBlockSelectors "filter"
                               "List of block selection criteria, as JSON file")
                       <*> some argJsonLogfile
                       <*> parseMachineTimelineOutputFiles) $
            Opt.progDesc "Machine performance timeline")
      , Opt.command "block-propagation"
          (Opt.info (BlockPropagationCmd
                       <$> argJsonGenesisFile "genesis"
                              "Genesis file of the run"
                       <*> argJsonRunMetafile "run-metafile"
                              "The meta.json file from the benchmark run"
                       <*> many
                             (argJsonBlockSelectors "filter"
                               "List of block selection criteria, as JSON file")
                       <*> many argJsonLogfile
                       <*> parseBlockPropagationOutputFiles) $
            Opt.progDesc "Block propagation")
      , Opt.command "substring-keys"
          (Opt.info (pure SubstringKeysCmd) $
            Opt.progDesc "Dump substrings that narrow logs to relevant subset")
      , Opt.command "chaininfo"
          (Opt.info (RunInfoCmd
                       <$> argJsonGenesisFile "genesis"
                              "Genesis file of the run"
                       <*> argJsonRunMetafile "run-metafile"
                              "The meta.json file from the benchmark run") $
            Opt.progDesc "Dump substrings that narrow logs to relevant subset")
      ]

--
-- Analysis CLI flag/option data types
--
argJsonGenesisFile :: String -> String -> Parser JsonGenesisFile
argJsonGenesisFile optname desc =
  fmap JsonGenesisFile $
    Opt.option Opt.str
      $ long optname
      <> metavar "GENESIS-FILE"
      <> help desc

argJsonRunMetafile :: String -> String -> Parser JsonRunMetafile
argJsonRunMetafile optname desc =
  fmap JsonRunMetafile $
    Opt.option Opt.str
      $ long optname
      <> metavar "RUN-METAFILE"
      <> help desc

argJsonBlockSelectors :: String -> String -> Parser JsonFilterFile
argJsonBlockSelectors optname desc =
  fmap JsonFilterFile $
    Opt.option Opt.str
      $ long optname
      <> metavar "BLOCK-SELECTOR-FILE"
      <> help desc

argJsonLogfile :: Parser JsonLogfile
argJsonLogfile =
  JsonLogfile <$>
    Opt.argument Opt.str (Opt.metavar "LOGFILE")

argJsonOutputFile :: String -> String -> Parser JsonOutputFile
argJsonOutputFile optname desc =
  fmap JsonOutputFile $
    Opt.option Opt.str
      $ long optname
      <> metavar "JSON-OUTFILE"
      <> help desc

argTextOutputFile :: String -> String -> Parser TextOutputFile
argTextOutputFile optname desc =
  fmap TextOutputFile $
    Opt.option Opt.str
      $ long optname
      <> metavar "TEXT-OUTFILE"
      <> help desc

argCsvOutputFile :: String -> String -> Parser CsvOutputFile
argCsvOutputFile optname desc =
  fmap CsvOutputFile $
    Opt.option Opt.str
      $ long optname
      <> metavar "CSV-OUTFILE"
      <> help desc

argOutputFile :: String -> String -> Parser OutputFile
argOutputFile optname desc =
  fmap OutputFile $
    Opt.option Opt.str
      $ long optname
      <> metavar "OUTFILE"
      <> help desc
