{- HLINT ignore "Use newtype instead of data" -}

module Cardano.Timeseries.Interp.Config where
import           Data.Word (Word64)

data Config = Config {
  defaultRangeSamplingRateMillis :: Word64
}
