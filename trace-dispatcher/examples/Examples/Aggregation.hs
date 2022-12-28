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

instance MetaTrace BaseStats where
  namespaceFor BaseStats{} = Namespace [] ["BaseStats"]
  severityFor (Namespace _ ["BaseStats"]) _ = Just Info
  privacyFor  (Namespace _ ["BaseStats"]) _ = Just Public
  documentFor (Namespace _ ["BaseStats"]) = Just "Basic statistics"
  metricsDocFor (Namespace _ ["BaseStats"]) =
    [ ("measure", "This is the value of a single measurment")
    , ("sum", "This is the sum of all measurments")]
  allNamespaces = [Namespace [] ["BaseStats"]]

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
    formTracer <- humanFormatter True (Just "cardano") simpleTracer
    tracer <- foldTraceM calculate emptyStats (contramap unfold formTracer)

    configureTracers emptyTraceConfig [formTracer]

    traceWith tracer 1.0
    traceWith tracer 2.0
    traceWith tracer 0.5
