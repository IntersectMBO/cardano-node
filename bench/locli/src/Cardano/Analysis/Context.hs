{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
module Cardano.Analysis.Context (module Cardano.Analysis.Context) where

import Cardano.Prelude

import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Clock (UTCTime, NominalDiffTime)


-- This is difficult: we have two different genesis-related structures:
--  1. the real ShelleyGenesis
--  2. the profile-supplied genesis specification used by the workbench & bench-on-AWS.
--
data GenesisSpec
  = GenesisSpec
  { delegators          :: Word64
  , utxo                :: Word64
  }
  deriving (Generic, Show, ToJSON, FromJSON)

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
  deriving (Generic, Show, FromJSON, ToJSON)

-- | Partial 'Cardano.Ledger.Shelley.PParams.PParams'
data PParams
  = PParams
  { maxTxSize         :: Word64
  , maxBlockBodySize  :: Word64
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data GeneratorProfile
  = GeneratorProfile
  { add_tx_size     :: Word64
  , inputs_per_tx   :: Word64
  , outputs_per_tx  :: Word64
  , tps             :: Word64
  , tx_count        :: Word64
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data Metadata
  = Metadata
  { tag       :: Text
  , profile   :: Text
  , era       :: Text
  , timestamp :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON)
