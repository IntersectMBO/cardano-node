{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Examples.Aggregation (
  testAggregation
) where

import           Data.Aeson (Value (..), (.=))
import           Data.Text (pack)
import           GHC.Generics (Generic)

import           Cardano.Logging

data BaseStats = BaseStats {
    bsMeasure :: Double,
    bsMin     :: Double,
    bsMax     :: Double,
    bsCount   :: Int,
    bsSum     :: Double
    } deriving (Eq, Ord, Show, Generic)


instance LogFormatting BaseStats where
  forMachine _dtal BaseStats{..} =
      mconcat
        [ "kind" .= Data.Aeson.String "BaseStats"
        , "bsMeasure" .= String (pack $ show bsMeasure)
        , "bsMin" .= String (pack $ show bsMin)
        , "bsMax" .= String (pack $ show bsMax)
        , "bsCount" .= String (pack $ show bsCount)
        , "bsSum" .= String (pack $ show bsSum)
        ]
  asMetrics BaseStats {..} =
    [ DoubleM "measure" bsMeasure
    , DoubleM "sum" bsSum]

baseStatsDocumented :: Documented Double
baseStatsDocumented = Documented
  [
    DocMsg
      ["BaseStats"]
      [ ("measure", "This is the value of a single measurment")
      , ("sum", "This is the sum of all measurments")
      ]
      "Basic statistics"
  ]

emptyStats :: BaseStats
emptyStats = BaseStats 0.0 100000000.0 (-100000000.0) 0 0.0

calculate :: BaseStats -> LoggingContext -> Double -> BaseStats
calculate BaseStats{..} _ val =
    BaseStats
      val
      (min bsMin val)
      (max bsMax val)
      (1 + bsCount)
      (bsSum + val)

testAggregation :: IO ()
testAggregation = do
    simpleTracer <- standardTracer
    formTracer <- fmap (appendName "BaseTrace")
                      (humanFormatter True (Just "cardano") simpleTracer)
    tracer <- foldTraceM calculate emptyStats formTracer
    configureTracers emptyTraceConfig baseStatsDocumented [tracer]
    traceWith tracer 1.0
    traceWith tracer 2.0
    traceWith tracer 0.5
