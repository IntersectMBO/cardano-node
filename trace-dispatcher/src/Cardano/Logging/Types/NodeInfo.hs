{-# LANGUAGE DeriveGeneric #-}
{-# Language DerivingStrategies #-}
{-# Language DeriveAnyClass #-}

module Cardano.Logging.Types.NodeInfo
  ( NodeInfo (..)
  )
  where

import           Control.DeepSeq (NFData)
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Text (Text)
import           Data.Time (UTCTime)
import           GHC.Generics (Generic)

-- | NodeInfo

data NodeInfo = NodeInfo
  { niName            :: Text
  , niProtocol        :: Text
  , niVersion         :: Text
  , niCommit          :: Text
  , niStartTime       :: UTCTime
  , niSystemStartTime :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)
