{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Cardano.Analysis.Ground
  ( module Cardano.Analysis.Ground
  , BlockNo (..), EpochNo (..), SlotNo (..)
  )
where

import Prelude (String)
import Cardano.Prelude hiding (head)

import Data.Aeson (FromJSON, ToJSON)
import Data.Attoparsec.Text             qualified as Atto
import Data.Attoparsec.Time             qualified as Iso8601
import Data.Text                        qualified as T
import Data.Time.Clock                              (UTCTime, NominalDiffTime)
import Options.Applicative
import Options.Applicative              qualified as Opt

import Cardano.Slotting.Slot    (EpochNo(..), SlotNo(..))
import Ouroboros.Network.Block  (BlockNo(..))


newtype EpochSlot = EpochSlot { unEpochSlot :: Word64 }
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (FromJSON, NFData, ToJSON, Num)

newtype EpochSafeInt = EpochSafeInt { unEpochSafeInt :: Word64 }
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (FromJSON, NFData, ToJSON, Num)


---
--- Files
---
newtype JsonGenesisFile
  = JsonGenesisFile { unJsonGenesisFile :: FilePath }
  deriving (Show, Eq)

newtype JsonRunMetafile
  = JsonRunMetafile { unJsonRunMetafile :: FilePath }
  deriving (Show, Eq)

newtype JsonLogfile
  = JsonLogfile { unJsonLogfile :: FilePath }
  deriving (NFData, Show, Eq)

newtype JsonOutputFile
  = JsonOutputFile { unJsonOutputFile :: FilePath }
  deriving (Show, Eq)

newtype TextOutputFile
  = TextOutputFile { unTextOutputFile :: FilePath }
  deriving (Show, Eq)

newtype CsvOutputFile
  = CsvOutputFile { unCsvOutputFile :: FilePath }
  deriving (Show, Eq)

newtype OutputFile
  = OutputFile { unOutputFile :: FilePath }
  deriving (Show, Eq)

data MachineTimelineOutputFiles
  = MachineTimelineOutputFiles
  { mtofSlotStats          :: Maybe JsonOutputFile
  , mtofAnalysis           :: Maybe JsonOutputFile
  , mtofFullStatsPretty    :: Maybe TextOutputFile
  , mtofReportStatsPretty  :: Maybe TextOutputFile
  , mtofTimelinePretty     :: Maybe TextOutputFile
  , mtofTimelineCsv        :: Maybe  CsvOutputFile
  , mtofFullStatsCsv       :: Maybe  CsvOutputFile
  , mtofDerivedVectors0Csv :: Maybe  CsvOutputFile
  , mtofDerivedVectors1Csv :: Maybe  CsvOutputFile
  }
  deriving (Show)

data BlockPropagationOutputFiles
  = BlockPropagationOutputFiles
  { bpofForgerPretty       :: Maybe TextOutputFile
  , bpofPeersPretty        :: Maybe TextOutputFile
  , bpofPropagationPretty  :: Maybe TextOutputFile
  , bpofFullStatsPretty    :: Maybe TextOutputFile
  , bpofMachViews          :: Maybe JsonOutputFile
  , bpofChainPretty        :: Maybe TextOutputFile
  , bpofChain              :: Maybe JsonOutputFile
  , bpofChainRaw           :: Maybe JsonOutputFile
  , bpofAnalysis           :: Maybe JsonOutputFile
  }
  deriving (Show)

---
--- Parsers
---
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

pSlotNo :: String -> String -> Parser SlotNo
pSlotNo name desc =
  SlotNo <$>
    Opt.option Opt.auto
     (  Opt.long name
     <> Opt.metavar "SLOT"
     <> Opt.help desc
     )

optUTCTime :: String -> String -> Parser UTCTime
optUTCTime optname desc =
  Opt.option (readerFromAttoParser Iso8601.utcTime)
    $ long optname
    <> metavar "ISO8601-TIME"
    <> help desc
 where
   -- Stolen from: cardano-cli/src/Cardano/CLI/Shelley/Parsers.hs
   readerFromAttoParser :: Atto.Parser a -> Opt.ReadM a
   readerFromAttoParser p =
     Opt.eitherReader (Atto.parseOnly (p <* Atto.endOfInput) . T.pack)

optDuration :: String -> String -> NominalDiffTime -> Parser NominalDiffTime
optDuration optname desc def=
  Opt.option ((realToFrac :: Double -> NominalDiffTime) <$> Opt.auto)
    $ long optname
    <> metavar "SEC"
    <> help desc
    <> value def

optWord :: String -> String -> Word64 -> Parser Word64
optWord optname desc def =
  Opt.option auto
    $ long optname
    <> metavar "INT"
    <> help desc
    <> value def

parseMachineTimelineOutputFiles :: Parser MachineTimelineOutputFiles
parseMachineTimelineOutputFiles =
  MachineTimelineOutputFiles
    <$> optional
        (argJsonOutputFile "slotstats-json"
           "Per-slot performance summaries")
    <*> optional
        (argJsonOutputFile "analysis-json"
           "Write analysis JSON to this file, if specified -- otherwise print to stdout.")
    <*> optional
        (argTextOutputFile "fullstats-text"
           "Full performance statistics breakdown")
    <*> optional
        (argTextOutputFile "reportstats-text"
           "Report performance statistics breakdown")
    <*> optional
        (argTextOutputFile "timeline-text"
           "Dump pretty timeline of extracted slot leadership summaries, as a side-effect of log analysis")
    <*> optional
        (argCsvOutputFile "timeline-csv"
           "Dump CSV of the timeline")
    <*> optional
        (argCsvOutputFile "stats-csv"
           "Dump CSV of the timeline statistics")
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
        (argTextOutputFile "forger-text"       "Forger stats")
    <*> optional
        (argTextOutputFile "peers-text"        "Peers stats")
    <*> optional
        (argTextOutputFile "propagation-text"  "Propagation stats")
    <*> optional
        (argTextOutputFile "fullstats-text"    "Full (forger+peers+propagation) stats")
    <*> optional
        (argJsonOutputFile "mach-views-json"   "Machine chain views as JSON")
    <*> optional
        (argTextOutputFile "chain-text"        "Timeline of chain evolution, one line per block")
    <*> optional
        (argJsonOutputFile "chain-raw-json"    "Unfiltered chain as JSON")
    <*> optional
        (argJsonOutputFile "chain-json"        "Chain as JSON")
    <*> optional
        (argJsonOutputFile "analysis-json"     "Analysis as JSON")
