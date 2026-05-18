{-# LANGUAGE DeriveGeneric #-}

module System.Metrics.ReqResp
  ( MetricName
  , MetricValue (..)
  , Request (..)
  , Response (..)
  ) where

import           Codec.Serialise (Serialise)
import           Data.Int (Int64)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

-- | Each EKG metric has a unique name.
type MetricName = Text

-- | Metric value that will be transmitted from the forwarder to the acceptor.
-- Please note that EKG.Distribution is not supported yet.
data MetricValue
  = CounterValue !Int64  -- ^ Counter value.
  | GaugeValue   !Int64  -- ^ Gauge value.
  | LabelValue   !Text   -- ^ Text label.
  deriving (Eq, Show, Generic)

instance ShowProxy MetricValue
instance Serialise MetricValue

-- | The request for the new metrics.
-- The acceptor will send this request to the forwarder.
data Request
  = GetAllMetrics                      -- ^ Get all metrics from the forwarder's local store.
  | GetMetrics !(NonEmpty MetricName)  -- ^ Get specific metrics only.
  | GetUpdatedMetrics                  -- ^ Get all metrics from the forwarder's local store that have changed since the last request.
  deriving (Eq, Generic, Show)

-- | The response with the metrics.
-- The forwarder will send it to the acceptor as a reply for the request.
-- Please note that the list of metrics can be empty (for example, if the
-- forwarder's local store is empty).
newtype Response = ResponseMetrics [(MetricName, MetricValue)]
  deriving (Generic, Show)

instance ShowProxy Request
instance Serialise Request

instance ShowProxy Response
instance Serialise Response
