{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Cardano.Analysis.Ground
  ( module Cardano.Analysis.Ground
  , BlockNo (..), EpochNo (..), SlotNo (..)
  )
where

import Prelude                          (String, show)
import Cardano.Prelude                  hiding (head)

import Data.Aeson--                       (FromJSON (..), ToJSON (..), ToJSONKey (..), FromJSONKey (..))
import Data.Aeson.Types                 (toJSONKeyText)
import Data.Attoparsec.Text             qualified as Atto
import Data.Attoparsec.Time             qualified as Iso8601
import Data.Text                        qualified as T
import Data.Text.Short                  qualified as SText
import Data.Text.Short                  (ShortText, fromText, toText)
import Data.Time.Clock                              (UTCTime, NominalDiffTime)
import Options.Applicative
import Options.Applicative              qualified as Opt
import Quiet                            (Quiet (..))

import Cardano.Slotting.Slot            (EpochNo(..), SlotNo(..))
import Ouroboros.Network.Block          (BlockNo(..))


newtype TId = TId { unTId :: ShortText }
  deriving (Eq, Generic, Ord)
  deriving newtype (FromJSON, ToJSON)
  deriving anyclass NFData
  deriving Show via Quiet TId

newtype Hash = Hash { unHash :: ShortText }
  deriving (Eq, Generic, Ord)
  deriving newtype (FromJSON, ToJSON)
  deriving anyclass NFData

shortHash :: Hash -> T.Text
shortHash = toText . SText.take 6 . unHash

instance Show Hash where show = T.unpack . toText . unHash

instance ToJSONKey Hash where
  toJSONKey = toJSONKeyText (toText . unHash)
instance FromJSONKey Hash where
  fromJSONKey = FromJSONKeyText (Hash . fromText)

newtype Host = Host { unHost :: ShortText }
  deriving (Eq, Generic, Ord)
  deriving newtype (IsString, FromJSON, ToJSON)
  deriving anyclass NFData
  deriving Show via Quiet Host

instance FromJSON BlockNo where
  parseJSON o = BlockNo <$> parseJSON o
instance ToJSON BlockNo where
  toJSON (BlockNo x) = toJSON x

newtype EpochSlot = EpochSlot { unEpochSlot :: Word64 }
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (FromJSON, NFData, ToJSON, Num)

newtype EpochSafeInt = EpochSafeInt { unEpochSafeInt :: Word64 }
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (FromJSON, NFData, ToJSON, Num)

data HostDeduction
  = HostFromLogfilename
  deriving stock (Eq, Ord, Show)

---
--- Files
---
newtype JsonGenesisFile
  = JsonGenesisFile { unJsonGenesisFile :: FilePath }
  deriving (Show, Eq)

newtype JsonRunMetafile
  = JsonRunMetafile { unJsonRunMetafile :: FilePath }
  deriving (Show, Eq)

newtype JsonDomainFile
  = JsonDomainFile { unJsonDomainFile :: FilePath }
  deriving (Show, Eq)

newtype JsonLogfile
  = JsonLogfile { unJsonLogfile :: FilePath }
  deriving (Show, Eq)
  deriving newtype (NFData)

newtype JsonInputFile
  = JsonInputFile { unJsonInputFile :: FilePath }
  deriving (Show, Eq)

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
optJsonGenesisFile :: String -> String -> Parser JsonGenesisFile
optJsonGenesisFile optname desc =
  fmap JsonGenesisFile $
    Opt.option Opt.str
      $ long optname
      <> metavar "GENESIS-FILE"
      <> help desc

optJsonRunMetafile :: String -> String -> Parser JsonRunMetafile
optJsonRunMetafile optname desc =
  fmap JsonRunMetafile $
    Opt.option Opt.str
      $ long optname
      <> metavar "RUN-METAFILE"
      <> help desc

optJsonDomainFile :: String -> String -> Parser JsonDomainFile
optJsonDomainFile optname desc =
  fmap JsonDomainFile $
    Opt.option Opt.str
      $ long optname
      <> metavar "DOMAINFILE"
      <> help desc

optJsonLogfile :: String -> String -> Parser JsonLogfile
optJsonLogfile optname desc =
  fmap JsonLogfile $
    Opt.option Opt.str
      $ long optname
      <> metavar "JSONLOGFILE"
      <> help desc

argJsonLogfile :: Parser JsonLogfile
argJsonLogfile =
  JsonLogfile <$>
    Opt.argument Opt.str (Opt.metavar "LOGFILE")

optJsonInputFile :: String -> String -> Parser JsonInputFile
optJsonInputFile optname desc =
  fmap JsonInputFile $
    Opt.option Opt.str
      $ long optname
      <> metavar "JSON-FILE"
      <> help desc

optJsonOutputFile :: String -> String -> Parser JsonOutputFile
optJsonOutputFile optname desc =
  fmap JsonOutputFile $
    Opt.option Opt.str
      $ long optname
      <> metavar "JSON-OUTFILE"
      <> help desc

optTextOutputFile :: String -> String -> Parser TextOutputFile
optTextOutputFile optname desc =
  fmap TextOutputFile $
    Opt.option Opt.str
      $ long optname
      <> metavar "TEXT-OUTFILE"
      <> help desc

optCsvOutputFile :: String -> String -> Parser CsvOutputFile
optCsvOutputFile optname desc =
  fmap CsvOutputFile $
    Opt.option Opt.str
      $ long optname
      <> metavar "CSV-OUTFILE"
      <> help desc

optOutputFile :: String -> String -> Parser OutputFile
optOutputFile optname desc =
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
        (optJsonOutputFile "slotstats-json"
           "Per-slot performance summaries")
    <*> optional
        (optJsonOutputFile "analysis-json"
           "Write analysis JSON to this file, if specified -- otherwise print to stdout.")
    <*> optional
        (optTextOutputFile "fullstats-text"
           "Full performance statistics breakdown")
    <*> optional
        (optTextOutputFile "reportstats-text"
           "Report performance statistics breakdown")
    <*> optional
        (optTextOutputFile "timeline-text"
           "Dump pretty timeline of extracted slot leadership summaries, as a side-effect of log analysis")
    <*> optional
        (optCsvOutputFile "timeline-csv"
           "Dump CSV of the timeline")
    <*> optional
        (optCsvOutputFile "stats-csv"
           "Dump CSV of the timeline statistics")
    <*> optional
        (optCsvOutputFile "derived-vectors-0-csv"
           "Dump CSV of vectors derived from the timeline")
    <*> optional
        (optCsvOutputFile "derived-vectors-1-csv"
           "Dump CSV of vectors derived from the timeline")

parseBlockPropagationOutputFiles :: Parser BlockPropagationOutputFiles
parseBlockPropagationOutputFiles =
  BlockPropagationOutputFiles
    <$> optional
        (optTextOutputFile "forger-text"       "Forger stats")
    <*> optional
        (optTextOutputFile "peers-text"        "Peers stats")
    <*> optional
        (optTextOutputFile "propagation-text"  "Propagation stats")
    <*> optional
        (optTextOutputFile "fullstats-text"    "Full (forger+peers+propagation) stats")
    <*> optional
        (optJsonOutputFile "mach-views-json"   "Machine chain views as JSON")
    <*> optional
        (optTextOutputFile "chain-text"        "Timeline of chain evolution, one line per block")
    <*> optional
        (optJsonOutputFile "chain-raw-json"    "Unfiltered chain as JSON")
    <*> optional
        (optJsonOutputFile "chain-json"        "Chain as JSON")
    <*> optional
        (optJsonOutputFile "analysis-json"     "Analysis as JSON")
