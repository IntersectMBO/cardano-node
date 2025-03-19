{-# LANGUAGE DeriveGeneric #-}
{-# Language DerivingStrategies #-}
{-# Language DeriveAnyClass #-}

module Cardano.Logging.Types.NodeStartupInfo
  ( NodeStartupInfo (..)
  )
  where

import           Control.DeepSeq (NFData)
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Text (Text)
import           Data.Time (NominalDiffTime)
import           Data.Word (Word64)
import           GHC.Generics (Generic)

-- | NodeStartupInfo

-- | This information is taken from 'BasicInfoShelleyBased'. It is required for
--   'cardano-tracer' service (particularly, for RTView).
data NodeStartupInfo = NodeStartupInfo
  { suiEra               :: Text
  , suiSlotLength        :: NominalDiffTime
  , suiEpochLength       :: Word64
  , suiSlotsPerKESPeriod :: Word64
  } 
  deriving stock 
    (Eq, Show, Generic)
  deriving anyclass
    (NFData, ToJSON, FromJSON)
