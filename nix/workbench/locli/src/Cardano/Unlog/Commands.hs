{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- | CLI command types
module Cardano.Unlog.Commands (module Cardano.Unlog.Commands) where

import           Prelude

import           Data.Text (Text)

import           Options.Applicative
import qualified Options.Applicative as Opt

import           Cardano.Unlog.LogObject hiding (Text)

--
-- Analysis CLI command data types
--

-- | All the CLI subcommands under \"analysis\".
--
data AnalysisCommand
  = PerfTimeline
      JsonGenesisFile
      JsonRunMetafile
      [JsonLogfile]
      AnalysisOutputFiles
  | SubstringKeys
  deriving (Show)

data AnalysisOutputFiles
  = AnalysisOutputFiles
  { ofLogObjects         :: Maybe JsonOutputFile
  , ofSlotStats          :: Maybe JsonOutputFile
  , ofTimelinePretty     :: Maybe TextOutputFile
  , ofTimelineCsv        :: Maybe  CsvOutputFile
  , ofStatsCsv           :: Maybe  CsvOutputFile
  , ofHistogram          :: Maybe     OutputFile
  , ofAnalysis           :: Maybe JsonOutputFile
  , ofDerivedVectors0Csv :: Maybe  CsvOutputFile
  , ofDerivedVectors1Csv :: Maybe  CsvOutputFile
  }
  deriving (Show)

renderAnalysisCommand :: AnalysisCommand -> Text
renderAnalysisCommand sc =
  case sc of
    PerfTimeline {}  -> "analyse perf-timeline"
    SubstringKeys {} -> "analyse substring-keys"

parseAnalysisOutputFiles :: Parser AnalysisOutputFiles
parseAnalysisOutputFiles =
  AnalysisOutputFiles
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

parseAnalysisCommands :: Parser AnalysisCommand
parseAnalysisCommands =
  Opt.subparser $
    mconcat
      [ Opt.command "perf-timeline"
          (Opt.info (PerfTimeline
                       <$> argJsonGenesisFile "genesis"
                              "Genesis file of the run"
                       <*> argJsonRunMetafile "run-metafile"
                              "The meta.json file from the benchmark run"
                       <*> some argJsonLogfile
                       <*> parseAnalysisOutputFiles) $
            Opt.progDesc "Analyse leadership checks")
      , Opt.command "substring-keys"
          (Opt.info (pure SubstringKeys) $
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
      <> metavar "JSON-GENESIS-FILE"
      <> help desc

argJsonRunMetafile :: String -> String -> Parser JsonRunMetafile
argJsonRunMetafile optname desc =
  fmap JsonRunMetafile $
    Opt.option Opt.str
      $ long optname
      <> metavar "JSON-RUN-METAFILE"
      <> help desc

argJsonLogfile :: Parser JsonLogfile
argJsonLogfile =
  JsonLogfile <$>
    Opt.argument Opt.str (Opt.metavar "JSON-LOGFILE")

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
