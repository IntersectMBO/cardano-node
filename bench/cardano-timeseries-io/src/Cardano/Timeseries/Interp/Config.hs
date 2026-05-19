{- HLINT ignore "Use newtype instead of data" -}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Cardano.Timeseries.Interp.Config where
import           Data.Aeson (ToJSON)
import           Data.Word (Word64)
import           GHC.Generics

data Config = Config {
  defaultRangeSamplingRateMillis :: Word64
} deriving (Show, Eq, Generic, ToJSON)
