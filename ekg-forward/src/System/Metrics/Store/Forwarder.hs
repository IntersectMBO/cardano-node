{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

module System.Metrics.Store.Forwarder
  ( mkResponse
  , mkResponseDummy
  ) where

import qualified Data.HashMap.Strict               as HM
import qualified Data.HashSet                      as HS
import qualified Data.List.NonEmpty                as NE
import           Data.Maybe                        (mapMaybe)
import qualified System.Metrics                    as EKG

import           System.Metrics.Configuration      (ForwarderConfiguration (..))
import qualified System.Metrics.Protocol.Forwarder as Forwarder
import           System.Metrics.ReqResp            (MetricName,
                                                    MetricValue (..),
                                                    Request (..), Response (..))
import           System.Metrics.Store.Deltify


mkResponse
  :: ForwarderConfiguration
  -> Deltify IO
  -> EKG.Store
  -> Forwarder.EKGForwarder Request Response IO ()
mkResponse ForwarderConfiguration{..} Deltify{..} ekgStore =
  Forwarder.EKGForwarder
    { Forwarder.recvMsgReq = \request -> do
        actionOnRequest request
        sample <- EKG.sampleAll ekgStore
        case request of
          GetUpdatedMetrics -> do
            sample' <- deltaDeltify sample
            let supportedMetrics = mapMaybe filterMetrics $ HM.toList sample'
            return $ ResponseMetrics supportedMetrics
          GetAllMetrics -> do
            let supportedMetrics = mapMaybe filterMetrics $ HM.toList sample
            deltaPutAll sample
            return $ ResponseMetrics supportedMetrics
          GetMetrics (HS.fromList . NE.toList -> mNames) -> do
            let
              sample' = filterSample mNames sample
              supportedMetrics = mapMaybe filterMetrics $ HM.toList sample'
            deltaPutDelta sample'
            return $ ResponseMetrics supportedMetrics
    , Forwarder.recvMsgDone = return ()
    }

filterMetrics
  :: (MetricName, EKG.Value)
  -> Maybe (MetricName, MetricValue)
filterMetrics (mName, ekgValue) =
  case ekgValue of
    EKG.Counter c      -> Just (mName, CounterValue c)
    EKG.Gauge g        -> Just (mName, GaugeValue g)
    EKG.Label l        -> Just (mName, LabelValue l)
    EKG.Distribution _ -> Nothing  -- Distribution does not supported yet.

filterSample
  :: HS.HashSet MetricName
  -> EKG.Sample
  -> EKG.Sample
filterSample mNames = HM.filterWithKey (\k _ -> k `HS.member` mNames)

mkResponseDummy
  :: Forwarder.EKGForwarder Request Response IO ()
mkResponseDummy =
  Forwarder.EKGForwarder
    { Forwarder.recvMsgReq  = const $ return $ ResponseMetrics []
    , Forwarder.recvMsgDone = return ()
    }
