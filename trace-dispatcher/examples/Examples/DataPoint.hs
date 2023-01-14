{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Examples.DataPoint (
  testDataPoint
) where

import qualified Data.Aeson as A
import           Data.ByteString.Lazy.UTF8
import qualified Data.Map.Strict as M
import           Trace.Forward.Utils.DataPoint (DataPoint (..))
import           GHC.Conc
import           GHC.Generics (Generic)

import           Cardano.Logging



data BaseStats = BaseStats {
    bsMeasure :: Double,
    bsMin     :: Double,
    bsMax     :: Double,
    bsCount   :: Int,
    bsSum     :: Double
    } deriving (Eq, Ord, Show, Generic)

instance A.ToJSON BaseStats where
  toEncoding = A.genericToEncoding A.defaultOptions

instance Show DataPoint where
  show (DataPoint a) = toString $ A.encode a

namesForBaseStats :: BaseStats -> Namespace
namesForBaseStats _ = ["BaseStats"]

emptyStats :: BaseStats
emptyStats = BaseStats 0.0 100000000.0 (-100000000.0) 0 0.0

testDataPoint :: IO ()
testDataPoint = do
    dpMap <- newTVarIO M.empty
    let rawDataPointTracer = dataPointTracer dpMap
    dpTracer <- mkDataPointTracer
                          rawDataPointTracer
                          namesForBaseStats
    traceWith dpTracer emptyStats
    dps <- readTVarIO dpMap
    print dps
