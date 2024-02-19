{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Cardano.Logging.Test.Unit.Aggregation (
  testAggregation
, testAggResult
) where

import           Cardano.Logging
import           Cardano.Logging.Test.Tracer

import           Data.Aeson (Value (..), (.=))
import           Data.IORef
import           Data.Text (Text, pack)
import           GHC.Generics (Generic)

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

calculate :: BaseStats -> LoggingContext -> Double -> IO BaseStats
calculate BaseStats{..} _ val = pure $
    BaseStats
      val
      (min bsMin val)
      (max bsMax val)
      (1 + bsCount)
      (bsSum + val)

testAggregation :: IO [Text]
testAggregation = do
    testTracerRef <- newIORef []
    simpleTracer <- testTracer testTracerRef
    formTracer <- machineFormatter Nothing simpleTracer
    tracer <- foldTraceM calculate emptyStats (contramap unfold formTracer)
    confState <- emptyConfigReflection
    configureTracers confState emptyTraceConfig [formTracer]

    traceWith tracer 1.0
    traceWith tracer 2.0
    traceWith tracer 0.5

    msgs <- reverse <$> readIORef testTracerRef
    let res = map formattedMsgAsText msgs
    pure res

testAggResult :: [Text]
testAggResult = [
     "{\"at\":\"2023-11-23T15:55:01.255202499Z\",\"ns\":\"\",\"data\":{\"bsCount\":\"1\",\"bsMax\":\"1.0\",\"bsMeasure\":\"1.0\",\"bsMin\":\"1.0\",\"bsSum\":\"1.0\",\"kind\":\"BaseStats\"},\"sev\":\"Info\",\"thread\":\"1342\",\"host\":\"deusXmachina\"}"
    ,"{\"at\":\"2023-11-23T15:55:01.255204601Z\",\"ns\":\"\",\"data\":{\"bsCount\":\"2\",\"bsMax\":\"2.0\",\"bsMeasure\":\"2.0\",\"bsMin\":\"1.0\",\"bsSum\":\"3.0\",\"kind\":\"BaseStats\"},\"sev\":\"Info\",\"thread\":\"1342\",\"host\":\"deusXmachina\"}"
    ,"{\"at\":\"2023-11-23T15:55:01.25520585Z\",\"ns\":\"\",\"data\":{\"bsCount\":\"3\",\"bsMax\":\"2.0\",\"bsMeasure\":\"0.5\",\"bsMin\":\"0.5\",\"bsSum\":\"3.5\",\"kind\":\"BaseStats\"},\"sev\":\"Info\",\"thread\":\"1342\",\"host\":\"deusXmachina\"}"
    ]
