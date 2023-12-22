{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Beacon.SlotDataPoint
       ( module Cardano.Beacon.SlotDataPoint
       -- , module SDP
       ) where

{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Data.Aeson
import           Data.Int
import           Data.Word
import           Text.Builder (Builder)

import           Cardano.Slotting.Slot (SlotNo)
-- import           Cardano.Tools.DBAnalyser.Analysis.BenchmarkLedgerOps.SlotDataPoint as SDP


instance FromJSON SlotDataPoint where
  parseJSON = withObject "SlotDataPoint" $ \o -> do
    slot            :: SlotNo   <- o .: "slot"
    slotGap         :: Word64   <- o .: "slotGap"
    totalTime       :: Int64    <- o .: "totalTime"
    mut             :: Int64    <- o .: "mut"
    gc              :: Int64    <- o .: "gc"
    majGcCount      :: Word32   <- o .: "majGcCount"
    minGcCount      :: Word32   <- o .: "minGcCount"
    allocatedBytes  :: Word64   <- o .: "allocatedBytes"
    mut_forecast    :: Int64    <- o .: "mut_forecast"
    mut_headerTick  :: Int64    <- o .: "mut_headerTick"
    mut_headerApply :: Int64    <- o .: "mut_headerApply"
    mut_blockTick   :: Int64    <- o .: "mut_blockTick"
    mut_blockApply  :: Int64    <- o .: "mut_blockApply"
    let blockStats = BlockStats []

    pure SlotDataPoint{..}



-- TODO remove types once this has been resolved:
{-
app/Cardano/Beacon/SlotDataPoint.hs:18:1: error:
    Could not load module ‘Cardano.Tools.DBAnalyser.Analysis.BenchmarkLedgerOps.SlotDataPoint’
    it is a hidden module in the package ‘ouroboros-consensus-cardano-0.12.1.0’
    Use -v (or `:set -v` in ghci) to see a list of the files searched for.
   |
18 | import           Cardano.Tools.DBAnalyser.Analysis.BenchmarkLedgerOps.SlotDataPoint as SDP
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-}

data SlotDataPoint =
    SlotDataPoint
      { -- | Slot in which the 5 ledger operations were applied.
        slot            :: !SlotNo
        -- | Gap to the previous slot.
      , slotGap         :: !Word64
        -- | Total time spent in the 5 ledger operations at 'slot'.
      , totalTime       :: !Int64
        -- | Time spent by the mutator while performing the 5 ledger operations
        -- at 'slot'.
      , mut             :: !Int64
        -- | Time spent in garbage collection while performing the 5 ledger
        -- operations at 'slot'.
      , gc              :: !Int64
        -- | Total number of __major__ garbage collections that took place while
        -- performing the 5 ledger operations at 'slot'.
      , majGcCount      :: !Word32
        -- | Total number of __minor__ garbage collections that took place while
        -- performing the 5 ledger operations at 'slot'.
      , minGcCount      :: !Word32
        -- | Allocated bytes while performing the 5 ledger operations
        -- at 'slot'.
      , allocatedBytes  :: !Word64
        -- | Difference of the GC.mutator_elapsed_ns field when computing the
        -- forecast.
      , mut_forecast    :: !Int64
      , mut_headerTick  :: !Int64
      , mut_headerApply :: !Int64
      , mut_blockTick   :: !Int64
      , mut_blockApply  :: !Int64
      -- | Free-form information about the block.
      , blockStats      :: !BlockStats
      } deriving Show

newtype BlockStats = BlockStats { unBlockStats :: [Builder] }
  deriving Show
