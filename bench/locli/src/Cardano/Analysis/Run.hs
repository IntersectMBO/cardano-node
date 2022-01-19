{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-name-shadowing #-}
module Cardano.Analysis.Run (module Cardano.Analysis.Run) where

import Prelude (String)
import Cardano.Prelude

import Data.Aeson.Types qualified as Aeson
import Data.Aeson (FromJSON(..), Object, ToJSON(..), withObject, (.:))
import Data.Attoparsec.Text qualified as Atto
import Data.Attoparsec.Time qualified as Iso8601
import Data.Text (intercalate, pack)
import Data.Time.Clock (UTCTime, NominalDiffTime)
import Data.Time.Clock.POSIX qualified as Time
import Options.Applicative
import Options.Applicative qualified as Opt

--
-- This is difficult: we have two different genesis-related structures:
--  1. the real ShelleyGenesis
--  2. the profile-supplied genesis specification used by the workbench & bench-on-AWS.
--
data GenesisSpec
  = GenesisSpec
  { delegators          :: Word64
  , dense_pool_density  :: Word64
  , n_pools             :: Word64
  , utxo                :: Word64
  }
  deriving (Generic, Show, ToJSON)

-- | Partial 'Cardano.Ledger.Shelley.Genesis.ShelleyGenesis'
data Genesis
  = Genesis
  { activeSlotsCoeff   :: Double
  , protocolParams     :: PParams
  , networkMagic       :: Word64
  , epochLength        :: Word64
  , systemStart        :: UTCTime
  , slotsPerKESPeriod  :: Word64
  , slotLength         :: NominalDiffTime
  , maxKESEvolutions   :: Word64
  , securityParam      :: Word64
  }
  deriving (Generic, Show, ToJSON)

-- | Partial 'Cardano.Ledger.Shelley.PParams.PParams'
data PParams
  = PParams
  { maxTxSize         :: Word64
  , maxBlockBodySize  :: Word64
  }
  deriving (Generic, Show, ToJSON)

data GeneratorProfile
  = GeneratorProfile
  { add_tx_size     :: Word64
  , inputs_per_tx   :: Word64
  , outputs_per_tx  :: Word64
  , tps             :: Word64
  , tx_count        :: Word64
  }
  deriving (Generic, Show, ToJSON)

data Metadata
  = Metadata
  { tag       :: Text
  , profile   :: Text
  , era       :: Text
  , timestamp :: UTCTime
  }
  deriving (Generic, Show, ToJSON)

renderRunExport :: Run -> [Text]
renderRunExport Run{metadata=Metadata{..}, ..} =
  Data.Text.intercalate "," <$>
  [[ "Profile",    profile]
  ,[ "Era",        era ]
  ,[ "Date",       show timestamp]
  ,[ "Pools",      show $ n_pools genesisSpec]
  ,[ "Density",    show $ dense_pool_density genesisSpec]
  ,[ "Delegators", show $ delegators genesisSpec]
  ,[ "UTxO",       show $ utxo genesisSpec]
  ]

data ARunWith a
  = Run
  { genesisSpec      :: GenesisSpec
  , generatorProfile :: GeneratorProfile
  , metadata         :: Metadata
  , genesis          :: a
  }
  deriving (Generic, Show, ToJSON)

type RunPartial = ARunWith ()
type Run        = ARunWith Genesis

completeRun :: RunPartial -> Genesis -> Run
completeRun Run{..} g = Run { genesis = g, .. }

instance FromJSON GenesisSpec
instance FromJSON GeneratorProfile
instance FromJSON Genesis
instance FromJSON Metadata
instance FromJSON PParams
instance FromJSON RunPartial where
  parseJSON = withObject "Run" $ \v -> do
    meta :: Object <- v .: "meta"
    profile_content <- meta .: "profile_content"
    generator <- profile_content .: "generator"
    --
    genesisSpec      <- profile_content .: "genesis"
    generatorProfile <- parseJSON $ Aeson.Object generator
    --
    tag       <- meta .: "tag"
    profile   <- meta .: "profile"
    era       <- generator .: "era"
    timestamp <- (meta .: "timestamp" :: Aeson.Parser Integer)
                  <&> Time.posixSecondsToUTCTime . realToFrac
    --
    let metadata = Metadata{..}
        genesis  = ()
    pure Run{..}

optUTCTime :: String -> String -> Parser UTCTime
optUTCTime optname desc =
  Opt.option (readerFromAttoParser Iso8601.utcTime)
    $ long optname
    <> metavar "ISO8601-TIME"
    <> help desc

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

-- Stolen from: cardano-cli/src/Cardano/CLI/Shelley/Parsers.hs
readerFromAttoParser :: Atto.Parser a -> Opt.ReadM a
readerFromAttoParser p =
    Opt.eitherReader (Atto.parseOnly (p <* Atto.endOfInput) . pack)
