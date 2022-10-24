{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Cardano.Analysis.Ground
  ( module Cardano.Analysis.Ground
  , module Data.DataDomain
  , BlockNo (..), EpochNo (..), SlotNo (..)
  )
where

import Prelude                          (String, fail, show)
import Cardano.Prelude                  hiding (head)

import Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)
import Data.Aeson
import Data.Aeson.Types                 (toJSONKeyText)
import Data.Attoparsec.Text             qualified as Atto
import Data.Attoparsec.Time             qualified as Iso8601
import Data.ByteString.Lazy.Char8       qualified as LBS
import Data.Text                        qualified as T
import Data.Text.Short                  qualified as SText
import Data.Text.Short                  (ShortText, fromText, toText)
import Data.Time.Clock                              (UTCTime, NominalDiffTime)
import Options.Applicative
import Options.Applicative              qualified as Opt
import Quiet                            (Quiet (..))

import Cardano.Slotting.Slot            (EpochNo(..), SlotNo(..))
import Ouroboros.Network.Block          (BlockNo(..))

import Data.DataDomain


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
  parseJSON o = case o of
    Number{}  -> BlockNo <$> parseJSON o
    Object o' -> BlockNo <$> o' .: "unBlockNo"
    _         -> fail "illegal type for BlockNo"
    -- FIXME: this workaround catches a faulty JSON serialisation of BlockNo
    --   is:         {"unBlockNo":1790}
    --   should be:  1790
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
newtype InputDir
  = InputDir { unInputDir :: FilePath }
  deriving (Show, Eq)
  deriving newtype (NFData)

newtype JsonLogfile
  = JsonLogfile { unJsonLogfile :: FilePath }
  deriving (Show, Eq)
  deriving newtype (NFData)

newtype JsonInputFile (a :: Type)
  = JsonInputFile { unJsonInputFile :: FilePath }
  deriving (Show, Eq)

newtype JsonOutputFile (a :: Type)
  = JsonOutputFile { unJsonOutputFile :: FilePath }
  deriving (Show, Eq)

newtype GnuplotOutputFile
  = GnuplotOutputFile { unGnuplotOutputFile :: FilePath }
  deriving (Show, Eq)

newtype OrgOutputFile
  = OrgOutputFile { unOrgOutputFile :: FilePath }
  deriving (Show, Eq)

newtype TextInputFile
  = TextInputFile { unTextInputFile :: FilePath }
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

---
--- Readers
---
readJsonData :: FromJSON a => JsonInputFile a -> (Text -> b) -> ExceptT b IO a
readJsonData f err =
  unJsonInputFile f
  & LBS.readFile
  & fmap eitherDecode
  & newExceptT
  & firstExceptT (err . T.pack)

readJsonDataIO :: FromJSON a => JsonInputFile a -> IO (Either String a)
readJsonDataIO f =
  unJsonInputFile f
  & LBS.readFile
  & fmap eitherDecode

---
--- Parsers
---
optInputDir :: String -> String -> Parser InputDir
optInputDir optname desc =
  fmap InputDir $
    Opt.option Opt.str
      $ long optname
      <> metavar "DIR"
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

optJsonInputFile :: String -> String -> Parser (JsonInputFile a)
optJsonInputFile optname desc =
  fmap JsonInputFile $
    Opt.option Opt.str
      $ long optname
      <> metavar "JSON-FILE"
      <> help desc

optJsonOutputFile :: String -> String -> Parser (JsonOutputFile a)
optJsonOutputFile optname desc =
  fmap JsonOutputFile $
    Opt.option Opt.str
      $ long optname
      <> metavar "JSON-OUTFILE"
      <> help desc

optGnuplotOutputFile :: String -> String -> Parser GnuplotOutputFile
optGnuplotOutputFile optname desc =
  fmap GnuplotOutputFile $
    Opt.option Opt.str
      $ long optname
      <> metavar "CDF-OUTFILE"
      <> help desc

optTextInputFile :: String -> String -> Parser TextInputFile
optTextInputFile optname desc =
  fmap TextInputFile $
    Opt.option Opt.str
      $ long optname
      <> metavar "TEXT-INFILE"
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
