{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Cardano.Tracer.Handlers.RTView.UI.Types
  ( ChartId (..)
  , ChartSelectId (..)
  , ChartSettings (..)
  , ChartsSettings
  , Color (..)
  , Colors
  , DatasetsIndices
  , Index (..)
  , WebPageStatus
  ) where

import           Cardano.Tracer.Types

import           Control.Concurrent.STM.TBQueue (TBQueue)
import           Control.Concurrent.STM.TVar (TVar)
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Map.Strict (Map)
import           Data.Word (Word16)
import           GHC.Generics (Generic)

data ChartId
  = CPUChart
  | MemoryChart
  | GCMajorNumChart
  | GCMinorNumChart
  | GCLiveMemoryChart
  | CPUTimeGCChart
  | CPUTimeAppChart
  | ThreadsNumChart
  -- Chain
  | ChainDensityChart
  | SlotNumChart
  | BlockNumChart
  | SlotInEpochChart
  | EpochChart
  | NodeCannotForgeChart
  | ForgedSlotLastChart
  | NodeIsLeaderChart
  | NodeIsNotLeaderChart
  | ForgedInvalidSlotLastChart
  | AdoptedSlotLastChart
  | NotAdoptedSlotLastChart
  | AboutToLeadSlotLastChart
  | CouldNotForgeSlotLastChart
  -- TX
  | TxsProcessedNumChart
  | MempoolBytesChart
  | TxsInMempoolChart
  deriving (Bounded, Enum, Generic, FromJSON, ToJSON, Show)

data ChartSelectId
  = TimeRangeSelect
  | UpdatePeriodSelect
  deriving Show

newtype Index = Index Word16
  deriving Show

data ChartSettings = ChartSettings
  { csTimeRangeInS    :: !Int
  , csUpdatePeriodInS :: !Int
  } deriving (Generic, FromJSON, ToJSON)

type ChartsSettings = [(ChartId, ChartSettings)]

newtype Color = Color String

type Colors = TBQueue Color

-- | After the node is connected, we have to add a new dataset to all historical charts.
--   The metrics received from this node will be added in these datasets.
--   Since each dataset has its index, we need a map 'NodeName -> ix',
--   where 'ix' is an index of a dataset in _each_ chart.
--   We use node's name instead of node's id, because we want to keep the same color
--   for the same node.
type DatasetsIndices = TVar (Map NodeName Index)

-- | It's 'True' if the web page is opened, 'False' otherwise.
type WebPageStatus = TVar Bool
