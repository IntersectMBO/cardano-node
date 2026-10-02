{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Trace.Forward.Types.NodeStartupInfo
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
  { -- | The most recent era the node supports transitioning to, not necessarily the era it currently operates in.
    suiLatestSupportedEra :: Text
  , suiSlotLength        :: NominalDiffTime
  , suiEpochLength       :: Word64
  , suiSlotsPerKESPeriod :: Word64
  }
  deriving stock
    (Eq, Show, Generic)
  deriving anyclass
    (NFData, ToJSON, FromJSON)
