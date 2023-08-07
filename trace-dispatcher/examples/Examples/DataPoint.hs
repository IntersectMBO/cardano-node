{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Examples.DataPoint (
  testDataPoint
) where

import           Control.DeepSeq (NFData)
import qualified Data.Aeson as A
import           Data.ByteString.Lazy.UTF8
import qualified Data.Map.Strict as M
import           GHC.Conc
import           GHC.Generics (Generic)
import           Trace.Forward.Utils.DataPoint (DataPoint (..))

import           Cardano.Logging


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

testDataPoint :: IO ()
testDataPoint = do
    dpMap <- newTVarIO M.empty
    let rawDataPointTracer = dataPointTracer dpMap
    dpTracer <- mkDataPointTracer rawDataPointTracer
    traceWith dpTracer emptyStats
    dps <- readTVarIO dpMap
    print dps
