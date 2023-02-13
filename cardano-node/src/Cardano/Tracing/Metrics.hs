
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Tracing.Metrics
  ( ForgingStats (..)
  , ForgeThreadStats (..)
  , mapForgingCurrentThreadStats
  , mapForgingCurrentThreadStats_
  , mapForgingStatsTxsProcessed
  , mkForgingStats
  , threadStatsProjection
  ) where

import           Control.Concurrent (ThreadId, myThreadId)
import           Control.Concurrent.STM
import           Control.Monad (join)
import           Data.Functor (void)
import           Data.Int (Int64)
import           Data.IORef (IORef, atomicModifyIORef', newIORef)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map


-- | This structure stores counters of blockchain-related events,
--   per individual forge thread.
--   These counters are driven by traces.
data ForgingStats
  = ForgingStats
  { fsTxsProcessedNum :: !(IORef Int)
    -- ^ Transactions removed from mempool.
  , fsState           :: !(TVar (Map ThreadId (TVar ForgeThreadStats)))
  , fsBlocksUncoupled :: !(TVar Int64)
    -- ^ Blocks forged since last restart not on the current chain
  }

-- | Per-forging-thread statistics.
data ForgeThreadStats = ForgeThreadStats
  { ftsNodeCannotForgeNum        :: !Int
  , ftsNodeIsLeaderNum           :: !Int
  , ftsBlocksForgedNum           :: !Int
  , ftsSlotsMissedNum            :: !Int
    -- ^ Potentially missed slots.  Note that this is not the same as the number
    -- of missed blocks, since this includes all occurrences of not reaching a
    -- leadership check decision, whether or not leadership was possible or not.
    --
    -- Also note that when the aggregate total for this metric is reported in the
    -- multi-pool case, it can be much larger than the actual number of slots
    -- occurring since node start, for it is a sum total for all threads.
  , ftsLastSlot                  :: !Int
  }

mkForgingStats :: IO ForgingStats
mkForgingStats =
  ForgingStats
    <$> newIORef 0
    <*> newTVarIO mempty
    <*> newTVarIO 0

mapForgingStatsTxsProcessed ::
     ForgingStats
  -> (Int -> Int)
  -> IO Int
mapForgingStatsTxsProcessed fs f =
  atomicModifyIORef' (fsTxsProcessedNum fs) $
    \txCount -> join (,) $ f txCount

mapForgingCurrentThreadStats ::
     ForgingStats
  -> (ForgeThreadStats -> (ForgeThreadStats, a))
  -> IO a
mapForgingCurrentThreadStats ForgingStats { fsState } f = do
  tid <- myThreadId
  allStats <- readTVarIO fsState
  varStats <- case Map.lookup tid allStats of
    Nothing -> do
      varStats <- newTVarIO $ ForgeThreadStats 0 0 0 0 0
      atomically $ modifyTVar fsState $ Map.insert tid varStats
      return varStats
    Just varStats ->
      return varStats
  atomically $ do
    stats <- readTVar varStats
    let !(!stats', x) = f stats
    writeTVar varStats stats'
    return x

mapForgingCurrentThreadStats_ ::
     ForgingStats
  -> (ForgeThreadStats -> ForgeThreadStats)
  -> IO ()
mapForgingCurrentThreadStats_ fs f =
  void $ mapForgingCurrentThreadStats fs ((, ()) . f)

threadStatsProjection ::
     ForgingStats
  -> (ForgeThreadStats -> a)
  -> IO [a]
threadStatsProjection fs f = atomically $ do
  allStats <- readTVar (fsState fs)
  mapM (fmap f . readTVar) $ Map.elems allStats
