{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Logging.Test.Unit.DataPoint (
    testDataPoint
  , testDataPointResult
) where

import           Cardano.Logging

import           Control.DeepSeq (NFData)
import qualified Data.Aeson as A
import           Data.ByteString.Lazy.UTF8
import qualified Data.Map.Strict as M
import           GHC.Conc
import           GHC.Generics (Generic)

import           Trace.Forward.Protocol.DataPoint.Type (DataPointName)
import           Trace.Forward.Utils.DataPoint (DataPoint (..))


data BaseStats = BaseStats {
    bsMeasure :: Double,
    bsMin     :: Double,
    bsMax     :: Double,
    bsCount   :: Int,
    bsSum     :: Double
    } deriving (Eq, Ord, Show, Generic)

deriving instance (NFData BaseStats)

instance MetaTrace BaseStats where
  namespaceFor BaseStats{} = Namespace [] ["BaseStats"]
  severityFor (Namespace _ ["BaseStats"]) _ = Just Info
  privacyFor  (Namespace _ ["BaseStats"]) _ = Just Public
  documentFor (Namespace _ ["BaseStats"]) = Just "Basic statistics"
  metricsDocFor (Namespace _ ["BaseStats"]) =
    [ ("measure", "This is the value of a single measurment")
    , ("sum", "This is the sum of all measurments")]
  allNamespaces = [Namespace [] ["BaseStats"]]

instance A.ToJSON BaseStats where
  toEncoding = A.genericToEncoding A.defaultOptions

instance Show DataPoint where
  show (DataPoint a) = toString $ A.encode a

emptyStats :: BaseStats
emptyStats = BaseStats 0.0 100000000.0 (-100000000.0) 0 0.0

testDataPoint :: IO (M.Map DataPointName DataPoint)
testDataPoint = do
    dpMap <- newTVarIO M.empty
    let rawDataPointTracer = dataPointTracer dpMap
    dpTracer <- mkDataPointTracer rawDataPointTracer
    traceWith dpTracer emptyStats
    readTVarIO dpMap

testDataPointResult :: String
testDataPointResult = "fromList [(\"BaseStats\",{\"bsMeasure\":0.0,\"bsMin\":1.0e8,\"bsMax\":-1.0e8,\"bsCount\":0,\"bsSum\":0.0})]"
