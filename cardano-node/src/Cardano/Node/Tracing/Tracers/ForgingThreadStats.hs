{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Node.Tracing.Tracers.ForgingThreadStats
    ( ForgingStats (..)
    , ForgeThreadStats (..)
    , forgeThreadStats
  ) where

import           Cardano.Logging
import           Cardano.Node.Tracing.Tracers.StartLeadershipCheck (ForgeTracerType)
import           Cardano.Slotting.Slot (SlotNo (..))
import           Ouroboros.Consensus.Node.Tracers
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import           Ouroboros.Consensus.Shelley.Node ()

import           Control.Concurrent (ThreadId, myThreadId)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Aeson (Value (..), (.=))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)

--------------------------------------------------------------------------------
-- ForgeThreadStats Tracer
--------------------------------------------------------------------------------

-- | Per-forging-thread statistics.
data ForgeThreadStats = ForgeThreadStats
  { ftsNodeCannotForgeNum :: !Int
  , ftsNodeIsLeaderNum    :: !Int
  , ftsBlocksForgedNum    :: !Int
  , ftsSlotsMissedNum     :: !Int
    -- ^ Potentially missed slots.  Note that this is not the same as the number
    -- of missed blocks, since this includes all occurrences of not reaching a
    -- leadership check decision, whether or not leadership was possible or not.
    --
    -- Also note that when the aggregate total for this metric is reported in the
    -- multi-pool case, it can be much larger than the actual number of slots
    -- occurring since node start, for it is a sum total for all threads.
  , ftsLastSlot           :: !Int
  }

instance LogFormatting ForgeThreadStats where
  forHuman ForgeThreadStats {..} =
    "Node cannot forge "  <> showT ftsNodeCannotForgeNum
    <> " node is leader " <> showT ftsNodeIsLeaderNum
    <> " blocks forged "  <> showT ftsBlocksForgedNum
    <> " slots missed "   <> showT ftsSlotsMissedNum
    <> " last slot "      <> showT ftsLastSlot
  forMachine _dtal ForgeThreadStats {..} =
    mconcat [ "kind" .= String "ForgeThreadStats"
             , "nodeCannotForgeNum" .= String (showT ftsNodeCannotForgeNum)
             , "nodeIsLeaderNum"    .= String (showT ftsNodeIsLeaderNum)
             , "blocksForgedNum"    .= String (showT ftsBlocksForgedNum)
             , "slotsMissed"        .= String (showT ftsSlotsMissedNum)
             , "lastSlot"           .= String (showT ftsLastSlot)
             ]
  asMetrics ForgeThreadStats {..} =
    [ IntM "Forge.NodeCannotForgeNum" (fromIntegral ftsNodeCannotForgeNum)
    , IntM "Forge.NodeIsLeaderNum"    (fromIntegral ftsNodeIsLeaderNum)
    , IntM "Forge.BlocksForgedNum"    (fromIntegral ftsBlocksForgedNum)
    , IntM "Forge.SlotsMissed"        (fromIntegral ftsSlotsMissedNum)
    ]

instance MetaTrace ForgeThreadStats where
    namespaceFor ForgeThreadStats {} = Namespace [] ["ForgeThreadStats"]

    severityFor _ _ = Just Info

    documentFor _ = Just ""

    metricsDocFor _ =
      [("Forge.NodeCannotForgeNum",
        "How many times was this node unable to forge [a block]?")
      ,("Forge.NodeIsLeaderNum",
        "How many times was this node slot leader?")
      ,("Forge.BlocksForgedNum",
        "How many blocks did this node forge?")
      ,("Forge.SlotsMissed",
        "How many slots did this node miss?")
      ]

    allNamespaces = [Namespace [] ["ForgeThreadStats"]]

emptyForgeThreadStats :: ForgeThreadStats
emptyForgeThreadStats = ForgeThreadStats 0 0 0 0 0


--------------------------------------------------------------------------------
-- ForgingStats Tracer
--------------------------------------------------------------------------------

-- | This structure stores counters of blockchain-related events,
--   per individual thread in fsStats.
data ForgingStats
  = ForgingStats
  { fsStats              :: !(Map ThreadId ForgeThreadStats)
  , fsNodeCannotForgeNum :: !Int
  , fsNodeIsLeaderNum    :: !Int
  , fsBlocksForgedNum    :: !Int
  , fsSlotsMissedNum     :: !Int
  }

instance LogFormatting ForgingStats where
  forHuman ForgingStats {..} =
    "Node cannot forge "  <> showT fsNodeCannotForgeNum
    <> " node is leader " <> showT fsNodeIsLeaderNum
    <> " blocks forged "  <> showT fsBlocksForgedNum
    <> " slots missed "   <> showT fsSlotsMissedNum
  forMachine _dtal ForgingStats {..} =
    mconcat [ "kind" .= String "ForgingStats"
             , "nodeCannotForgeNum" .= String (showT fsNodeCannotForgeNum)
             , "nodeIsLeaderNum"    .= String (showT fsNodeIsLeaderNum)
             , "blocksForgedNum"    .= String (showT fsBlocksForgedNum)
             , "slotsMissed"        .= String (showT fsSlotsMissedNum)
             ]
  asMetrics ForgingStats {..} =
    [ IntM "Forge.NodeCannotForgeNum" (fromIntegral fsNodeCannotForgeNum)
    , IntM "Forge.NodeIsLeaderNum"    (fromIntegral fsNodeIsLeaderNum)
    , IntM "Forge.BlocksForgedNum"    (fromIntegral fsBlocksForgedNum)
    , IntM "Forge.SlotsMissed"        (fromIntegral fsSlotsMissedNum)
    ]

instance MetaTrace ForgingStats where
    namespaceFor ForgingStats {} = Namespace [] ["ForgingStats"]

    severityFor _ _ = Just Info

    documentFor _ = Just
      "nodeCannotForgeNum shows how many times this node could not forge.\
      \\nnodeIsLeaderNum shows how many times this node was leader.\
      \\nblocksForgedNum shows how many blocks did forge in this node.\
      \\nslotsMissed shows how many slots were missed in this node."

    metricsDocFor _ =
      [("Forge.NodeCannotForgeNum",
        "How many times was this node unable to forge [a block]?")
      ,("Forge.NodeIsLeaderNum",
        "How many times was this node slot leader?")
      ,("Forge.BlocksForgedNum",
        "How many blocks did this node forge?")
      ,("Forge.SlotsMissed",
        "How many slots did this node miss?")
      ,("Forge.LastSlot",
        "")
      ]

    allNamespaces = [Namespace [] ["ForgingStats"]]


emptyForgingStats :: ForgingStats
emptyForgingStats = ForgingStats mempty 0 0 0 0

forgeThreadStats :: Trace IO ForgingStats
  -> IO (Trace IO (ForgeTracerType blk))
forgeThreadStats tr =
  let tr' = contramap unfold tr
  in foldCondTraceM calculateThreadStats emptyForgingStats
      (\case
          Left Consensus.TraceStartLeadershipCheck{} -> True
          Left _ -> False
          Right _  -> True
          )
      tr'

calculateThreadStats :: MonadIO m
  => ForgingStats
  -> LoggingContext
  -> ForgeTracerType blk
  -> m ForgingStats
calculateThreadStats stats _context
    (Left TraceNodeCannotForge {}) = do
      mapThreadStats
        stats
        (\fts -> (fts { ftsNodeCannotForgeNum = ftsNodeCannotForgeNum fts + 1}
                      , Nothing))
        (\fs _ ->  (fs  { fsNodeCannotForgeNum  = fsNodeCannotForgeNum fs + 1 }))
calculateThreadStats stats _context
    (Left (TraceNodeIsLeader (SlotNo slot'))) = do
      let slot = fromIntegral slot'
      mapThreadStats
        stats
        (\fts -> (fts { ftsNodeIsLeaderNum = ftsNodeIsLeaderNum fts + 1
                   , ftsLastSlot = slot}, Nothing))
        (\fs _ ->  (fs  { fsNodeIsLeaderNum  = fsNodeIsLeaderNum fs + 1 }))
calculateThreadStats stats _context
    (Left TraceForgedBlock {}) = do
      mapThreadStats
        stats
        (\fts -> (fts { ftsBlocksForgedNum = ftsBlocksForgedNum fts + 1}
                      , Nothing))
        (\fs _ ->  (fs  { fsBlocksForgedNum  = fsBlocksForgedNum fs + 1 }))
calculateThreadStats stats _context
    (Left (TraceNodeNotLeader (SlotNo slot'))) = do
      let slot = fromIntegral slot'
      mapThreadStats
        stats
        (\fts ->
          if ftsLastSlot fts == 0 || succ (ftsLastSlot fts) == slot
            then (fts { ftsLastSlot = slot }, Nothing)
            else
              let missed = (slot - ftsLastSlot fts)
              in (fts { ftsLastSlot = slot
                      , ftsSlotsMissedNum = ftsSlotsMissedNum fts + missed}
                 , Just missed))
        (\fs mbMissed -> case mbMissed of
                            Nothing -> fs
                            Just missed -> (fs { fsSlotsMissedNum =
                              fsSlotsMissedNum fs + missed}))
calculateThreadStats stats _context _message = pure stats

mapThreadStats ::
     MonadIO m
  => ForgingStats
  -> (ForgeThreadStats -> (ForgeThreadStats, Maybe a))
  -> (ForgingStats -> Maybe a -> ForgingStats)
  -> m ForgingStats
mapThreadStats fs@ForgingStats { fsStats } f1 f2 = do
  tid <- liftIO myThreadId
  let threadStats   =  fromMaybe emptyForgeThreadStats (Map.lookup tid fsStats)
      (newStats, w) = f1 threadStats
  pure $ f2 (fs {fsStats = Map.insert tid newStats fsStats}) w
