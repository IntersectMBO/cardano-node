{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Node.Tracing.Tracers.ChainDB
   ( withAddedToCurrentChainEmptyLimited
   ) where

import           Data.Aeson (Value (String), toJSON, (.=))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Numeric (showFFloat)
import           Prelude (id)
import           Text.Show

import           Cardano.Logging
import           Cardano.Node.Tracing.Era.Byron ()
import           Cardano.Node.Tracing.Era.Shelley ()
import           Cardano.Node.Tracing.Formatting ()
import           Cardano.Node.Tracing.Render
import           Cardano.Prelude hiding (Show, show, trace)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation (HeaderEnvelopeError (..), HeaderError (..),
                   OtherHeaderEnvelopeError)
import           Ouroboros.Consensus.Ledger.Abstract (LedgerError)
import           Ouroboros.Consensus.Ledger.Extended (ExtValidationError (..))
import           Ouroboros.Consensus.Ledger.Inspect (InspectLedger, LedgerEvent (..))
import           Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol)
import           Ouroboros.Consensus.Protocol.Abstract (ValidationErr)
import qualified Ouroboros.Consensus.Protocol.PBFT as PBFT
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmDB
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal (chunkNoToInt)
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Types as ImmDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB
import           Ouroboros.Consensus.Storage.LedgerDB.Types (UpdateLedgerDbTraceEvent (..))
import qualified Ouroboros.Consensus.Storage.LedgerDB.Types as LedgerDB
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolDB
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Consensus.Util.Enclose


import qualified Ouroboros.Network.AnchoredFragment as AF

{-# ANN module ("HLint: ignore Redundant bracket" :: Text) #-}

-- TODO implement differently so that it uses configuration
withAddedToCurrentChainEmptyLimited
  :: Trace IO (ChainDB.TraceEvent blk)
  -> IO (Trace IO (ChainDB.TraceEvent blk))
withAddedToCurrentChainEmptyLimited tr = do
  ltr <- limitFrequency 1.25 "AddedToCurrentChainLimiter" mempty tr
  routingTrace (selecting ltr) tr
 where
    selecting
      ltr
      (ChainDB.TraceAddBlockEvent (ChainDB.AddedToCurrentChain events _ _ _)) =
        if null events
          then pure ltr
          else pure tr
    selecting _ _ = pure tr


-- --------------------------------------------------------------------------------
-- -- ChainDB Tracer
-- --------------------------------------------------------------------------------

instance (  LogFormatting (Header blk)
          , LogFormatting (LedgerEvent blk)
          , LogFormatting (RealPoint blk)
          , ConvertRawHash blk
          , ConvertRawHash (Header blk)
          , LedgerSupportsProtocol blk
          , InspectLedger blk
          ) => LogFormatting (ChainDB.TraceEvent blk) where
  forHuman (ChainDB.TraceAddBlockEvent v)          = forHuman v
  forHuman (ChainDB.TraceFollowerEvent v)          = forHuman v
  forHuman (ChainDB.TraceCopyToImmutableDBEvent v) = forHuman v
  forHuman (ChainDB.TraceGCEvent v)                = forHuman v
  forHuman (ChainDB.TraceInitChainSelEvent v)      = forHuman v
  forHuman (ChainDB.TraceOpenEvent v)              = forHuman v
  forHuman (ChainDB.TraceIteratorEvent v)          = forHuman v
  forHuman (ChainDB.TraceLedgerEvent v)            = forHuman v
  forHuman (ChainDB.TraceLedgerReplayEvent v)      = forHuman v
  forHuman (ChainDB.TraceImmutableDBEvent v)       = forHuman v
  forHuman (ChainDB.TraceVolatileDBEvent v)        = forHuman v

  forMachine details (ChainDB.TraceAddBlockEvent v) =
    forMachine details v
  forMachine details (ChainDB.TraceFollowerEvent v) =
    forMachine details v
  forMachine details (ChainDB.TraceCopyToImmutableDBEvent v) =
    forMachine details v
  forMachine details (ChainDB.TraceGCEvent v) =
    forMachine details v
  forMachine details (ChainDB.TraceInitChainSelEvent v) =
    forMachine details v
  forMachine details (ChainDB.TraceOpenEvent v) =
    forMachine details v
  forMachine details (ChainDB.TraceIteratorEvent v) =
    forMachine details v
  forMachine details (ChainDB.TraceLedgerEvent v) =
    forMachine details v
  forMachine details (ChainDB.TraceLedgerReplayEvent v) =
    forMachine details v
  forMachine details (ChainDB.TraceImmutableDBEvent v) =
    forMachine details v
  forMachine details (ChainDB.TraceVolatileDBEvent v) =
    forMachine details v

  asMetrics (ChainDB.TraceAddBlockEvent v)          = asMetrics v
  asMetrics (ChainDB.TraceFollowerEvent v)          = asMetrics v
  asMetrics (ChainDB.TraceCopyToImmutableDBEvent v) = asMetrics v
  asMetrics (ChainDB.TraceGCEvent v)                = asMetrics v
  asMetrics (ChainDB.TraceInitChainSelEvent v)      = asMetrics v
  asMetrics (ChainDB.TraceOpenEvent v)              = asMetrics v
  asMetrics (ChainDB.TraceIteratorEvent v)          = asMetrics v
  asMetrics (ChainDB.TraceLedgerEvent v)            = asMetrics v
  asMetrics (ChainDB.TraceLedgerReplayEvent v)      = asMetrics v
  asMetrics (ChainDB.TraceImmutableDBEvent v)       = asMetrics v
  asMetrics (ChainDB.TraceVolatileDBEvent v)        = asMetrics v


instance MetaTrace  (ChainDB.TraceEvent blk) where
  namespaceFor (ChainDB.TraceAddBlockEvent ev) =
    nsPrependInner "AddBlockEvent" (namespaceFor ev)
  namespaceFor (ChainDB.TraceFollowerEvent ev) =
    nsPrependInner "FollowerEvent" (namespaceFor ev)
  namespaceFor (ChainDB.TraceCopyToImmutableDBEvent ev) =
    nsPrependInner "CopyToImmutableDBEvent" (namespaceFor ev)
  namespaceFor (ChainDB.TraceGCEvent ev) =
    nsPrependInner "GCEvent" (namespaceFor ev)
  namespaceFor (ChainDB.TraceInitChainSelEvent ev) =
    nsPrependInner "InitChainSelEvent" (namespaceFor ev)
  namespaceFor (ChainDB.TraceOpenEvent ev) =
    nsPrependInner "OpenEvent" (namespaceFor ev)
  namespaceFor (ChainDB.TraceIteratorEvent ev) =
    nsPrependInner "IteratorEvent" (namespaceFor ev)
  namespaceFor (ChainDB.TraceLedgerEvent ev) =
    nsPrependInner "LedgerEvent" (namespaceFor ev)
  namespaceFor (ChainDB.TraceLedgerReplayEvent ev) =
     nsPrependInner "LedgerReplay" (namespaceFor ev)
  namespaceFor (ChainDB.TraceImmutableDBEvent ev) =
    nsPrependInner "ImmDbEvent" (namespaceFor ev)
  namespaceFor (ChainDB.TraceVolatileDBEvent ev) =
     nsPrependInner "VolatileDbEvent" (namespaceFor ev)

  severityFor (Namespace out ("AddBlockEvent" : tl)) (Just (ChainDB.TraceAddBlockEvent ev')) =
    severityFor (Namespace out tl) (Just ev')
  severityFor (Namespace out ("AddBlockEvent" : tl)) Nothing =
    severityFor (Namespace out tl  :: Namespace (ChainDB.TraceAddBlockEvent blk)) Nothing
  severityFor (Namespace out ("FollowerEvent" : tl)) (Just (ChainDB.TraceFollowerEvent ev')) =
    severityFor (Namespace out tl) (Just ev')
  severityFor (Namespace out ("FollowerEvent" : tl)) Nothing =
    severityFor (Namespace out tl :: Namespace (ChainDB.TraceFollowerEvent blk)) Nothing
  severityFor (Namespace out ("CopyToImmutableDBEvent" : tl)) (Just (ChainDB.TraceCopyToImmutableDBEvent ev')) =
    severityFor (Namespace out tl) (Just ev')
  severityFor (Namespace out ("CopyToImmutableDBEvent" : tl)) Nothing =
    severityFor (Namespace out tl :: Namespace (ChainDB.TraceCopyToImmutableDBEvent blk)) Nothing
  severityFor (Namespace out ("GCEvent" : tl)) (Just (ChainDB.TraceGCEvent ev')) =
    severityFor (Namespace out tl) (Just ev')
  severityFor (Namespace out ("GCEvent" : tl)) Nothing =
    severityFor (Namespace out tl :: Namespace (ChainDB.TraceGCEvent blk)) Nothing
  severityFor (Namespace out ("InitChainSelEvent" : tl)) (Just (ChainDB.TraceInitChainSelEvent ev')) =
    severityFor (Namespace out tl) (Just ev')
  severityFor (Namespace out ("InitChainSelEvent" : tl)) Nothing =
    severityFor (Namespace out tl :: Namespace (ChainDB.TraceInitChainSelEvent blk)) Nothing
  severityFor (Namespace out ("OpenEvent" : tl)) (Just (ChainDB.TraceOpenEvent ev')) =
    severityFor (Namespace out tl) (Just ev')
  severityFor (Namespace out ("OpenEvent" : tl)) Nothing =
    severityFor (Namespace out tl :: Namespace (ChainDB.TraceOpenEvent blk)) Nothing
  severityFor (Namespace out ("IteratorEvent" : tl)) (Just (ChainDB.TraceIteratorEvent ev')) =
    severityFor (Namespace out tl) (Just ev')
  severityFor (Namespace out ("IteratorEvent" : tl)) Nothing =
    severityFor (Namespace out tl :: Namespace (ChainDB.TraceIteratorEvent blk)) Nothing
  severityFor (Namespace out ("LedgerEvent" : tl)) (Just (ChainDB.TraceLedgerEvent ev')) =
    severityFor (Namespace out tl) (Just ev')
  severityFor (Namespace out ("LedgerEvent" : tl)) Nothing =
    severityFor (Namespace out tl :: Namespace (LedgerDB.TraceEvent blk)) Nothing
  severityFor (Namespace out ("LedgerReplay" : tl)) (Just (ChainDB.TraceLedgerReplayEvent ev')) =
    severityFor (Namespace out tl) (Just ev')
  severityFor (Namespace out ("LedgerReplay" : tl)) Nothing =
    severityFor (Namespace out tl :: Namespace (LedgerDB.TraceReplayEvent blk)) Nothing
  severityFor (Namespace out ("ImmDbEvent" : tl)) (Just (ChainDB.TraceImmutableDBEvent ev')) =
    severityFor (Namespace out tl) (Just ev')
  severityFor (Namespace out ("ImmDbEvent" : tl)) Nothing =
    severityFor (Namespace out tl :: Namespace (ImmDB.TraceEvent blk)) Nothing
  severityFor (Namespace out ("VolatileDbEvent" : tl)) (Just (ChainDB.TraceVolatileDBEvent ev')) =
    severityFor (Namespace out tl) (Just ev')
  severityFor (Namespace out ("VolatileDbEvent" : tl)) Nothing =
    severityFor (Namespace out tl :: Namespace (VolDB.TraceEvent blk)) Nothing
  severityFor _ns _ = Nothing

  privacyFor (Namespace out ("AddBlockEvent" : tl)) (Just (ChainDB.TraceAddBlockEvent ev')) =
    privacyFor (Namespace out tl) (Just ev')
  privacyFor (Namespace out ("AddBlockEvent" : tl)) Nothing =
    privacyFor (Namespace out tl :: Namespace (ChainDB.TraceAddBlockEvent blk)) Nothing
  privacyFor (Namespace out ("FollowerEvent" : tl)) (Just (ChainDB.TraceFollowerEvent ev')) =
    privacyFor (Namespace out tl) (Just ev')
  privacyFor (Namespace out ("FollowerEvent" : tl)) Nothing =
    privacyFor (Namespace out tl :: Namespace (ChainDB.TraceFollowerEvent blk)) Nothing
  privacyFor (Namespace out ("CopyToImmutableDBEvent" : tl)) (Just (ChainDB.TraceCopyToImmutableDBEvent ev')) =
    privacyFor (Namespace out tl) (Just ev')
  privacyFor (Namespace out ("CopyToImmutableDBEvent" : tl)) Nothing =
    privacyFor (Namespace out tl :: Namespace (ChainDB.TraceCopyToImmutableDBEvent blk)) Nothing
  privacyFor (Namespace out ("GCEvent" : tl)) (Just (ChainDB.TraceGCEvent ev')) =
    privacyFor (Namespace out tl) (Just ev')
  privacyFor (Namespace out ("GCEvent" : tl)) Nothing =
    privacyFor (Namespace out tl :: Namespace (ChainDB.TraceGCEvent blk)) Nothing
  privacyFor (Namespace out ("InitChainSelEvent" : tl)) (Just (ChainDB.TraceInitChainSelEvent ev')) =
    privacyFor (Namespace out tl) (Just ev')
  privacyFor (Namespace out ("InitChainSelEvent" : tl)) Nothing =
    privacyFor (Namespace out tl :: Namespace (ChainDB.TraceInitChainSelEvent blk)) Nothing
  privacyFor (Namespace out ("OpenEvent" : tl)) (Just (ChainDB.TraceOpenEvent ev')) =
    privacyFor (Namespace out tl) (Just ev')
  privacyFor (Namespace out ("OpenEvent" : tl)) Nothing =
    privacyFor (Namespace out tl :: Namespace (ChainDB.TraceOpenEvent blk)) Nothing
  privacyFor (Namespace out ("IteratorEvent" : tl)) (Just (ChainDB.TraceIteratorEvent ev')) =
    privacyFor (Namespace out tl) (Just ev')
  privacyFor (Namespace out ("IteratorEvent" : tl)) Nothing =
    privacyFor (Namespace out tl :: Namespace (ChainDB.TraceIteratorEvent blk)) Nothing
  privacyFor (Namespace out ("LedgerEvent" : tl)) (Just (ChainDB.TraceLedgerEvent ev')) =
    privacyFor (Namespace out tl) (Just ev')
  privacyFor (Namespace out ("LedgerEvent" : tl)) Nothing =
    privacyFor (Namespace out tl :: Namespace (LedgerDB.TraceEvent blk)) Nothing
  privacyFor (Namespace out ("LedgerReplay" : tl)) (Just (ChainDB.TraceLedgerReplayEvent ev')) =
    privacyFor (Namespace out tl) (Just ev')
  privacyFor (Namespace out ("LedgerReplay" : tl)) Nothing =
    privacyFor (Namespace out tl :: Namespace (LedgerDB.TraceReplayEvent blk)) Nothing
  privacyFor (Namespace out ("ImmDbEvent" : tl)) (Just (ChainDB.TraceImmutableDBEvent ev')) =
    privacyFor (Namespace out tl) (Just ev')
  privacyFor (Namespace out ("ImmDbEvent" : tl)) Nothing =
    privacyFor (Namespace out tl :: Namespace (ImmDB.TraceEvent blk)) Nothing
  privacyFor (Namespace out ("VolatileDbEvent" : tl)) (Just (ChainDB.TraceVolatileDBEvent ev')) =
    privacyFor (Namespace out tl) (Just ev')
  privacyFor (Namespace out ("VolatileDbEvent" : tl)) Nothing =
    privacyFor (Namespace out tl :: Namespace (VolDB.TraceEvent blk)) Nothing
  privacyFor _ _ = Nothing

  detailsFor (Namespace out ("AddBlockEvent" : tl)) (Just (ChainDB.TraceAddBlockEvent ev')) =
    detailsFor (Namespace out tl) (Just ev')
  detailsFor (Namespace out ("AddBlockEvent" : tl)) Nothing =
    detailsFor (Namespace out tl :: Namespace (ChainDB.TraceAddBlockEvent blk)) Nothing
  detailsFor (Namespace out ("FollowerEvent" : tl)) (Just (ChainDB.TraceFollowerEvent ev')) =
    detailsFor (Namespace out tl) (Just ev')
  detailsFor (Namespace out ("FollowerEvent" : tl)) Nothing =
    detailsFor (Namespace out tl :: Namespace (ChainDB.TraceFollowerEvent blk)) Nothing
  detailsFor (Namespace out ("CopyToImmutableDBEvent" : tl)) (Just (ChainDB.TraceCopyToImmutableDBEvent ev')) =
    detailsFor (Namespace out tl) (Just ev')
  detailsFor (Namespace out ("CopyToImmutableDBEvent" : tl)) Nothing =
    detailsFor (Namespace out tl :: Namespace (ChainDB.TraceCopyToImmutableDBEvent blk)) Nothing
  detailsFor (Namespace out ("GCEvent" : tl)) (Just (ChainDB.TraceGCEvent ev')) =
    detailsFor (Namespace out tl) (Just ev')
  detailsFor (Namespace out ("GCEvent" : tl)) Nothing =
    detailsFor (Namespace out tl :: Namespace (ChainDB.TraceGCEvent blk)) Nothing
  detailsFor (Namespace out ("InitChainSelEvent" : tl)) (Just (ChainDB.TraceInitChainSelEvent ev')) =
    detailsFor (Namespace out tl) (Just ev')
  detailsFor (Namespace out ("InitChainSelEvent" : tl)) Nothing =
    detailsFor (Namespace out tl :: Namespace (ChainDB.TraceInitChainSelEvent blk)) Nothing
  detailsFor (Namespace out ("OpenEvent" : tl)) (Just (ChainDB.TraceOpenEvent ev')) =
    detailsFor (Namespace out tl) (Just ev')
  detailsFor (Namespace out ("OpenEvent" : tl)) Nothing =
    detailsFor (Namespace out tl :: Namespace (ChainDB.TraceOpenEvent blk)) Nothing
  detailsFor (Namespace out ("IteratorEvent" : tl)) (Just (ChainDB.TraceIteratorEvent ev')) =
    detailsFor (Namespace out tl) (Just ev')
  detailsFor (Namespace out ("IteratorEvent" : tl)) Nothing =
    detailsFor (Namespace out tl :: Namespace (ChainDB.TraceIteratorEvent blk)) Nothing
  detailsFor (Namespace out ("LedgerEvent" : tl)) (Just (ChainDB.TraceLedgerEvent ev')) =
    detailsFor (Namespace out tl) (Just ev')
  detailsFor (Namespace out ("LedgerEvent" : tl)) Nothing =
    detailsFor (Namespace out tl :: Namespace (LedgerDB.TraceReplayEvent blk)) Nothing
  detailsFor (Namespace out ("LedgerReplay" : tl)) (Just (ChainDB.TraceLedgerReplayEvent ev')) =
    detailsFor (Namespace out tl) (Just ev')
  detailsFor (Namespace out ("LedgerReplay" : tl)) Nothing =
    detailsFor (Namespace out tl :: Namespace (LedgerDB.TraceReplayEvent blk)) Nothing
  detailsFor (Namespace out ("ImmDbEvent" : tl)) (Just (ChainDB.TraceImmutableDBEvent ev')) =
    detailsFor (Namespace out tl) (Just ev')
  detailsFor (Namespace out ("ImmDbEvent" : tl)) Nothing =
    detailsFor (Namespace out tl :: (Namespace (ImmDB.TraceEvent blk))) Nothing
  detailsFor (Namespace out ("VolatileDbEvent" : tl)) (Just (ChainDB.TraceVolatileDBEvent ev')) =
    detailsFor (Namespace out tl) (Just ev')
  detailsFor (Namespace out ("VolatileDbEvent" : tl)) Nothing =
    detailsFor (Namespace out tl :: (Namespace (VolDB.TraceEvent blk))) Nothing
  detailsFor _ _ = Nothing

  metricsDocFor (Namespace out ("AddBlockEvent" : tl)) =
    metricsDocFor (Namespace out tl :: Namespace (ChainDB.TraceAddBlockEvent blk))
  metricsDocFor (Namespace out ("FollowerEvent" : tl)) =
    metricsDocFor (Namespace out tl :: Namespace (ChainDB.TraceFollowerEvent blk))
  metricsDocFor (Namespace out ("CopyToImmutableDBEvent" : tl)) =
    metricsDocFor (Namespace out tl :: Namespace (ChainDB.TraceCopyToImmutableDBEvent blk))
  metricsDocFor (Namespace out ("GCEvent" : tl)) =
    metricsDocFor (Namespace out tl :: Namespace (ChainDB.TraceGCEvent blk))
  metricsDocFor (Namespace out ("InitChainSelEvent" : tl)) =
    metricsDocFor (Namespace out tl :: Namespace (ChainDB.TraceInitChainSelEvent blk))
  metricsDocFor (Namespace out ("OpenEvent" : tl)) =
    metricsDocFor (Namespace out tl :: Namespace (ChainDB.TraceOpenEvent blk))
  metricsDocFor (Namespace out ("IteratorEvent" : tl)) =
    metricsDocFor (Namespace out tl :: Namespace (ChainDB.TraceIteratorEvent blk))
  metricsDocFor (Namespace out ("LedgerEvent" : tl)) =
    metricsDocFor (Namespace out tl :: Namespace (LedgerDB.TraceEvent blk))
  metricsDocFor (Namespace out ("LedgerReplay" : tl)) =
    metricsDocFor (Namespace out tl :: Namespace (LedgerDB.TraceReplayEvent blk))
  metricsDocFor (Namespace out ("ImmDbEvent" : tl)) =
    metricsDocFor (Namespace out tl :: Namespace (ImmDB.TraceEvent blk))
  metricsDocFor (Namespace out ("VolatileDbEvent" : tl)) =
    metricsDocFor (Namespace out tl :: Namespace (VolDB.TraceEvent blk))
  metricsDocFor _ = []

  documentFor (Namespace out ("AddBlockEvent" : tl)) =
    documentFor (Namespace out tl :: Namespace (ChainDB.TraceAddBlockEvent blk))
  documentFor (Namespace out ("FollowerEvent" : tl)) =
    documentFor (Namespace out tl :: Namespace (ChainDB.TraceFollowerEvent blk))
  documentFor (Namespace out ("CopyToImmutableDBEvent" : tl)) =
    documentFor (Namespace out tl :: Namespace (ChainDB.TraceCopyToImmutableDBEvent blk))
  documentFor (Namespace out ("GCEvent" : tl)) =
    documentFor (Namespace out tl :: Namespace (ChainDB.TraceGCEvent blk))
  documentFor (Namespace out ("InitChainSelEvent" : tl)) =
    documentFor (Namespace out tl :: Namespace (ChainDB.TraceInitChainSelEvent blk))
  documentFor (Namespace out ("OpenEvent" : tl)) =
    documentFor (Namespace out tl :: Namespace (ChainDB.TraceOpenEvent blk))
  documentFor (Namespace out ("IteratorEvent" : tl)) =
    documentFor (Namespace out tl :: Namespace (ChainDB.TraceIteratorEvent blk))
  documentFor (Namespace out ("LedgerEvent" : tl)) =
    documentFor (Namespace out tl :: Namespace (LedgerDB.TraceEvent blk))
  documentFor (Namespace out ("LedgerReplay" : tl)) =
    documentFor (Namespace out tl :: Namespace (LedgerDB.TraceReplayEvent blk))
  documentFor (Namespace out ("ImmDbEvent" : tl)) =
    documentFor (Namespace out tl :: Namespace (ImmDB.TraceEvent blk))
  documentFor (Namespace out ("VolatileDbEvent" : tl)) =
    documentFor (Namespace out tl :: Namespace (VolDB.TraceEvent blk))
  documentFor _ = Nothing

  allNamespaces =
        map  (nsPrependInner "AddBlockEvent")
                  (allNamespaces :: [Namespace (ChainDB.TraceAddBlockEvent blk)])
          ++ map  (nsPrependInner "FollowerEvent")
                  (allNamespaces :: [Namespace (ChainDB.TraceFollowerEvent blk)])
          ++ map  (nsPrependInner "CopyToImmutableDBEvent")
                  (allNamespaces :: [Namespace (ChainDB.TraceCopyToImmutableDBEvent blk)])
          ++ map  (nsPrependInner "GCEvent")
                  (allNamespaces :: [Namespace (ChainDB.TraceGCEvent blk)])
          ++ map  (nsPrependInner "InitChainSelEvent")
                  (allNamespaces :: [Namespace (ChainDB.TraceInitChainSelEvent blk)])
          ++ map  (nsPrependInner "OpenEvent")
                  (allNamespaces :: [Namespace (ChainDB.TraceOpenEvent blk)])
          ++ map  (nsPrependInner "IteratorEvent")
                  (allNamespaces :: [Namespace (ChainDB.TraceIteratorEvent blk)])
          ++ map  (nsPrependInner "LedgerEvent")
                  (allNamespaces :: [Namespace (LedgerDB.TraceEvent blk)])
          ++ map  (nsPrependInner "LedgerReplay")
                  (allNamespaces :: [Namespace (LedgerDB.TraceReplayEvent blk)])
          ++ map  (nsPrependInner "ImmDbEvent")
                  (allNamespaces :: [Namespace (ImmDB.TraceEvent blk)])
          ++ map  (nsPrependInner "VolatileDbEvent")
                  (allNamespaces :: [Namespace (VolDB.TraceEvent blk)])


--------------------------------------------------------------------------------
-- AddBlockEvent
--------------------------------------------------------------------------------


instance ( LogFormatting (Header blk)
         , LogFormatting (LedgerEvent blk)
         , LogFormatting (RealPoint blk)
         , ConvertRawHash blk
         , ConvertRawHash (Header blk)
         , LedgerSupportsProtocol blk
         , InspectLedger blk
         ) => LogFormatting (ChainDB.TraceAddBlockEvent blk) where
  forHuman (ChainDB.IgnoreBlockOlderThanK pt) =
    "Ignoring block older than K: " <> renderRealPointAsPhrase pt
  forHuman (ChainDB.IgnoreBlockAlreadyInVolatileDB pt) =
      "Ignoring block already in DB: " <> renderRealPointAsPhrase pt
  forHuman (ChainDB.IgnoreInvalidBlock pt _reason) =
      "Ignoring previously seen invalid block: " <> renderRealPointAsPhrase pt
  forHuman (ChainDB.AddedBlockToQueue pt edgeSz) =
      case edgeSz of
        RisingEdge ->
          "About to add block to queue: " <> renderRealPointAsPhrase pt
        FallingEdgeWith sz ->
          "Block added to queue: " <> renderRealPointAsPhrase pt <> ", queue size " <> condenseT sz
  forHuman (ChainDB.PoppedBlockFromQueue edgePt) =
      case edgePt of
        RisingEdge ->
          "Popping block from queue"
        FallingEdgeWith pt ->
          "Popped block from queue: " <> renderRealPointAsPhrase pt
  forHuman (ChainDB.BlockInTheFuture pt slot) =
      "Ignoring block from future: " <> renderRealPointAsPhrase pt <> ", slot " <> condenseT slot
  forHuman (ChainDB.StoreButDontChange pt) =
      "Ignoring block: " <> renderRealPointAsPhrase pt
  forHuman (ChainDB.TryAddToCurrentChain pt) =
      "Block fits onto the current chain: " <> renderRealPointAsPhrase pt
  forHuman (ChainDB.TrySwitchToAFork pt _) =
      "Block fits onto some fork: " <> renderRealPointAsPhrase pt
  forHuman (ChainDB.ChangingSelection pt) =
      "Changing selection to: " <> renderPointAsPhrase pt
  forHuman (ChainDB.AddedToCurrentChain es _ _ c) =
      "Chain extended, new tip: " <> renderPointAsPhrase (AF.headPoint c) <>
        Text.concat [ "\nEvent: " <> showT e | e <- es ]
  forHuman (ChainDB.SwitchedToAFork es _ _ c) =
      "Switched to a fork, new tip: " <> renderPointAsPhrase (AF.headPoint c) <>
        Text.concat [ "\nEvent: " <> showT e | e <- es ]
  forHuman (ChainDB.AddBlockValidation ev') = forHuman ev'
  forHuman (ChainDB.AddedBlockToVolatileDB pt _ _ enclosing) =
      case enclosing of
        RisingEdge  -> "Chain about to add block " <> renderRealPointAsPhrase pt
        FallingEdge -> "Chain added block " <> renderRealPointAsPhrase pt
  forHuman (ChainDB.ChainSelectionForFutureBlock pt) =
      "Chain selection run for block previously from future: " <> renderRealPointAsPhrase pt
  forHuman (ChainDB.PipeliningEvent ev') = forHuman ev'
  forMachine dtal (ChainDB.IgnoreBlockOlderThanK pt) =
      mconcat [ "kind" .= String "IgnoreBlockOlderThanK"
               , "block" .= forMachine dtal pt ]
  forMachine dtal (ChainDB.IgnoreBlockAlreadyInVolatileDB pt) =
      mconcat [ "kind" .= String "IgnoreBlockAlreadyInVolatileDB"
               , "block" .= forMachine dtal pt ]
  forMachine dtal (ChainDB.IgnoreInvalidBlock pt reason) =
      mconcat [ "kind" .= String "IgnoreInvalidBlock"
               , "block" .= forMachine dtal pt
               , "reason" .= showT reason ]
  forMachine dtal (ChainDB.AddedBlockToQueue pt edgeSz) =
      mconcat [ "kind" .= String "AddedBlockToQueue"
               , "block" .= forMachine dtal pt
               , case edgeSz of
                   RisingEdge         -> "risingEdge" .= True
                   FallingEdgeWith sz -> "queueSize" .= toJSON sz ]
  forMachine dtal (ChainDB.PoppedBlockFromQueue edgePt) =
      mconcat [ "kind" .= String "TraceAddBlockEvent.PoppedBlockFromQueue"
               , case edgePt of
                   RisingEdge         -> "risingEdge" .= True
                   FallingEdgeWith pt -> "block" .= forMachine dtal pt ]
  forMachine dtal (ChainDB.BlockInTheFuture pt slot) =
      mconcat [ "kind" .= String "BlockInTheFuture"
               , "block" .= forMachine dtal pt
               , "slot" .= forMachine dtal slot ]
  forMachine dtal (ChainDB.StoreButDontChange pt) =
      mconcat [ "kind" .= String "StoreButDontChange"
               , "block" .= forMachine dtal pt ]
  forMachine dtal (ChainDB.TryAddToCurrentChain pt) =
      mconcat [ "kind" .= String "TryAddToCurrentChain"
               , "block" .= forMachine dtal pt ]
  forMachine dtal (ChainDB.TrySwitchToAFork pt _) =
      mconcat [ "kind" .= String "TraceAddBlockEvent.TrySwitchToAFork"
               , "block" .= forMachine dtal pt ]
  forMachine dtal (ChainDB.ChangingSelection pt) =
      mconcat [ "kind" .= String "TraceAddBlockEvent.ChangingSelection"
               , "block" .= forMachine dtal pt ]
  forMachine dtal (ChainDB.AddedToCurrentChain events _ base extended) =
      mconcat $
               [ "kind" .=  String "AddedToCurrentChain"
               , "newtip" .= renderPointForDetails dtal (AF.headPoint extended)
               ]
            ++ [ "headers" .= toJSON (forMachine dtal `map` addedHdrsNewChain base extended)
               | dtal == DDetailed ]
            ++ [ "events" .= toJSON (map (forMachine dtal) events)
               | not (null events) ]
  forMachine dtal (ChainDB.SwitchedToAFork events _ old new) =
      mconcat $
               [ "kind" .= String "TraceAddBlockEvent.SwitchedToAFork"
               , "newtip" .= renderPointForDetails dtal (AF.headPoint new)
               ]
            ++ [ "headers" .= toJSON (forMachine dtal `map` addedHdrsNewChain old new)
               | dtal == DDetailed ]
            ++ [ "events" .= toJSON (map (forMachine dtal) events)
               | not (null events) ]
  forMachine dtal (ChainDB.AddBlockValidation ev') =
    forMachine dtal ev'
  forMachine dtal (ChainDB.AddedBlockToVolatileDB pt (BlockNo bn) _ enclosing) =
      mconcat $ [ "kind" .= String "AddedBlockToVolatileDB"
                , "block" .= forMachine dtal pt
                , "blockNo" .= showT bn ]
                <> [ "risingEdge" .= True | RisingEdge <- [enclosing] ]
  forMachine dtal (ChainDB.ChainSelectionForFutureBlock pt) =
      mconcat [ "kind" .= String "TChainSelectionForFutureBlock"
               , "block" .= forMachine dtal pt ]
  forMachine dtal (ChainDB.PipeliningEvent ev') =
    forMachine dtal ev'

  asMetrics (ChainDB.SwitchedToAFork _warnings newTipInfo _oldChain newChain) =
    let ChainInformation { slots, blocks, density, epoch, slotInEpoch } =
          chainInformation newTipInfo newChain 0
    in  [ DoubleM "ChainDB.Density" (fromRational density)
        , IntM    "ChainDB.SlotNum" (fromIntegral slots)
        , IntM    "ChainDB.BlockNum" (fromIntegral blocks)
        , IntM    "ChainDB.SlotInEpoch" (fromIntegral slotInEpoch)
        , IntM    "ChainDB.Epoch" (fromIntegral (unEpochNo epoch))
        ]
  asMetrics (ChainDB.AddedToCurrentChain _warnings newTipInfo _oldChain newChain) =
    let ChainInformation { slots, blocks, density, epoch, slotInEpoch } =
          chainInformation newTipInfo newChain 0
    in  [ DoubleM "ChainDB.Density" (fromRational density)
        , IntM    "ChainDB.SlotNum" (fromIntegral slots)
        , IntM    "ChainDB.BlockNum" (fromIntegral blocks)
        , IntM    "ChainDB.SlotInEpoch" (fromIntegral slotInEpoch)
        , IntM    "ChainDB.Epoch" (fromIntegral (unEpochNo epoch))
        ]
  asMetrics _ = []


instance MetaTrace  (ChainDB.TraceAddBlockEvent blk) where
  namespaceFor ChainDB.IgnoreBlockOlderThanK {} =
    Namespace [] ["IgnoreBlockOlderThanK"]
  namespaceFor ChainDB.IgnoreBlockAlreadyInVolatileDB {} =
    Namespace [] ["IgnoreBlockAlreadyInVolatileDB"]
  namespaceFor ChainDB.IgnoreInvalidBlock {} =
    Namespace [] ["IgnoreInvalidBlock"]
  namespaceFor ChainDB.AddedBlockToQueue {} =
    Namespace [] ["AddedBlockToQueue"]
  namespaceFor ChainDB.PoppedBlockFromQueue {} =
    Namespace [] ["PoppedBlockFromQueue"]
  namespaceFor ChainDB.BlockInTheFuture {} =
    Namespace [] ["BlockInTheFuture"]
  namespaceFor ChainDB.AddedBlockToVolatileDB {} =
    Namespace [] ["AddedBlockToVolatileDB"]
  namespaceFor ChainDB.TryAddToCurrentChain {} =
    Namespace [] ["TryAddToCurrentChain"]
  namespaceFor ChainDB.TrySwitchToAFork {} =
    Namespace [] ["TrySwitchToAFork"]
  namespaceFor ChainDB.StoreButDontChange {} =
    Namespace [] ["StoreButDontChange"]
  namespaceFor ChainDB.AddedToCurrentChain {} =
    Namespace [] ["AddedToCurrentChain"]
  namespaceFor ChainDB.SwitchedToAFork {} =
    Namespace [] ["SwitchedToAFork"]
  namespaceFor ChainDB.ChangingSelection {} =
    Namespace [] ["ChangingSelection"]
  namespaceFor (ChainDB.AddBlockValidation ev') =
    nsPrependInner "AddBlockValidation" (namespaceFor ev')
  namespaceFor ChainDB.ChainSelectionForFutureBlock {} =
    Namespace [] ["ChainSelectionForFutureBlock"]
  namespaceFor (ChainDB.PipeliningEvent ev') =
    nsPrependInner "PipeliningEvent" (namespaceFor ev')

  severityFor (Namespace _ ["IgnoreBlockOlderThanK"]) _ = Just Info
  severityFor (Namespace _ ["IgnoreBlockAlreadyInVolatileDB"]) _ = Just Info
  severityFor (Namespace _ ["IgnoreInvalidBlock"]) _ = Just Info
  severityFor (Namespace _ ["AddedBlockToQueue"]) _ = Just Debug
  severityFor (Namespace _ ["BlockInTheFuture"]) _ = Just Info
  severityFor (Namespace _ ["AddedBlockToVolatileDB"]) _ = Just Debug
  severityFor (Namespace _ ["PoppedBlockFromQueue"]) _ = Just Debug
  severityFor (Namespace _ ["TryAddToCurrentChain"]) _ = Just Debug
  severityFor (Namespace _ ["TrySwitchToAFork"]) _ = Just Info
  severityFor (Namespace _ ["StoreButDontChange"]) _ = Just Debug
  severityFor (Namespace _ ["ChangingSelection"]) _ = Just Debug
  severityFor (Namespace _ ["AddedToCurrentChain"])
              (Just (ChainDB.AddedToCurrentChain events _ _ _)) =
    Just $ maximumDef Notice (map sevLedgerEvent events)
  severityFor (Namespace _ ["AddedToCurrentChain"]) Nothing = Just Notice
  severityFor (Namespace _ ["SwitchedToAFork"])
              (Just (ChainDB.SwitchedToAFork events _ _ _)) =
    Just $ maximumDef Notice (map sevLedgerEvent events)
  severityFor (Namespace _ ["SwitchedToAFork"]) _ =
    Just Notice
  severityFor (Namespace out ("AddBlockValidation" : tl))
              (Just (ChainDB.AddBlockValidation ev')) =
    severityFor (Namespace out tl) (Just ev')
  severityFor (Namespace _ ("AddBlockValidation" : _tl)) Nothing = Just Notice
  severityFor (Namespace _ ["ChainSelectionForFutureBlock"]) _ = Just Debug
  severityFor (Namespace out ("PipeliningEvent" : tl)) (Just (ChainDB.PipeliningEvent ev')) =
    severityFor (Namespace out tl) (Just ev')
  severityFor (Namespace out ("PipeliningEvent" : tl)) Nothing =
    severityFor (Namespace out tl :: Namespace (ChainDB.TracePipeliningEvent blk)) Nothing
  severityFor _ _ = Nothing

  privacyFor (Namespace out ("AddBlockEvent" : tl)) (Just (ChainDB.AddBlockValidation ev')) =
    privacyFor (Namespace out tl) (Just ev')
  privacyFor (Namespace out ("AddBlockEvent" : tl)) Nothing =
    privacyFor (Namespace out tl :: Namespace (ChainDB.TraceValidationEvent blk)) Nothing
  privacyFor (Namespace out ("PipeliningEvent" : tl)) (Just (ChainDB.PipeliningEvent ev')) =
    privacyFor (Namespace out tl) (Just ev')
  privacyFor (Namespace out ("PipeliningEvent" : tl)) Nothing =
    privacyFor (Namespace out tl :: Namespace (ChainDB.TracePipeliningEvent blk)) Nothing
  privacyFor _ _ = Just Public

  detailsFor (Namespace out ("AddBlockEvent" : tl)) (Just (ChainDB.AddBlockValidation ev')) =
    detailsFor (Namespace out tl) (Just ev')
  detailsFor (Namespace out ("AddBlockEvent" : tl)) Nothing =
    detailsFor (Namespace out tl :: Namespace (ChainDB.TraceValidationEvent blk)) Nothing
  detailsFor (Namespace out ("PipeliningEvent" : tl)) (Just (ChainDB.PipeliningEvent ev')) =
    detailsFor (Namespace out tl) (Just ev')
  detailsFor (Namespace out ("PipeliningEvent" : tl)) Nothing =
    detailsFor (Namespace out tl :: Namespace (ChainDB.TracePipeliningEvent blk)) Nothing
  detailsFor _ _ = Just DNormal

  metricsDocFor (Namespace _ ["SwitchedToAFork"]) =
        [ ("ChainDB.Density",
          "The actual number of blocks created over the maximum expected number\
          \ of blocks that could be created over the span of the last @k@ blocks.")
        , ("ChainDB.Slots",
          "Number of slots in this chain fragment.")
        , ("ChainDB.Blocks",
          "Number of blocks in this chain fragment.")
        , ("ChainDB.SlotInEpoch",
          "Relative slot number of the tip of the current chain within the\
          \epoch..")
        , ("ChainDB.Epoch",
          "In which epoch is the tip of the current chain.")
        ]
  metricsDocFor (Namespace _ ["AddedToCurrentChain"]) =
        [("ChainDB.Density",
          "The actual number of blocks created over the maximum expected number\
          \ of blocks that could be created over the span of the last @k@ blocks.")
        , ("ChainDB.Slots",
          "Number of slots in this chain fragment.")
        , ("ChainDB.Blocks",
          "Number of blocks in this chain fragment.")
        , ("ChainDB.SlotInEpoch",
          "Relative slot number of the tip of the current chain within the\
          \epoch..")
        , ("ChainDB.Epoch",
          "In which epoch is the tip of the current chain.")
        ]
  metricsDocFor _ = []

  documentFor (Namespace _ ["IgnoreBlockOlderThanK"]) = Just
    "A block with a 'BlockNo' more than @k@ back than the current tip\
         \ was ignored."
  documentFor (Namespace _ ["IgnoreBlockAlreadyInVolatileDB"]) = Just
    "A block that is already in the Volatile DB was ignored."
  documentFor (Namespace _ ["IgnoreInvalidBlock"]) = Just
    "A block that is invalid was ignored."
  documentFor (Namespace _ ["AddedBlockToQueue"]) = Just
    "The block was added to the queue and will be added to the ChainDB by\
         \ the background thread. The size of the queue is included.."
  documentFor (Namespace _ ["BlockInTheFuture"]) = Just
    "The block is from the future, i.e., its slot number is greater than\
         \ the current slot (the second argument)."
  documentFor (Namespace _ ["AddedBlockToVolatileDB"]) = Just
    "A block was added to the Volatile DB"
  documentFor (Namespace _ ["PoppedBlockFromQueue"]) = Just""
  documentFor (Namespace _ ["TryAddToCurrentChain"]) = Just
    "The block fits onto the current chain, we'll try to use it to extend\
         \ our chain."
  documentFor (Namespace _ ["TrySwitchToAFork"]) = Just
    "The block fits onto some fork, we'll try to switch to that fork (if\
         \ it is preferable to our chain)"
  documentFor (Namespace _ ["StoreButDontChange"]) = Just
    "The block fits onto some fork, we'll try to switch to that fork (if\
         \ it is preferable to our chain)."
  documentFor (Namespace _ ["ChangingSelection"]) = Just
    "The new block fits onto the current chain (first\
         \ fragment) and we have successfully used it to extend our (new) current\
         \ chain (second fragment)."
  documentFor (Namespace _ ["AddedToCurrentChain"]) = Just
    "The new block fits onto the current chain (first\
         \ fragment) and we have successfully used it to extend our (new) current\
         \ chain (second fragment)."
  documentFor (Namespace _out ["SwitchedToAFork"]) = Just
    "The new block fits onto some fork and we have switched to that fork\
             \ (second fragment), as it is preferable to our (previous) current chain\
             \ (first fragment)."
  documentFor (Namespace out ("AddBlockValidation" : tl)) =
    documentFor (Namespace out tl :: Namespace (ChainDB.TraceValidationEvent blk))
  documentFor (Namespace _ ["ChainSelectionForFutureBlock"]) = Just
    "Run chain selection for a block that was previously from the future.\
         \ This is done for all blocks from the future each time a new block is\
         \ added."
  documentFor (Namespace out ("PipeliningEvent" : tl)) =
    documentFor (Namespace out tl :: Namespace (ChainDB.TracePipeliningEvent blk))
  documentFor _ = Nothing


  allNamespaces =
    [ Namespace [] ["IgnoreBlockOlderThanK"]
    , Namespace [] ["IgnoreBlockAlreadyInVolatileDB"]
    , Namespace [] ["IgnoreInvalidBlock"]
    , Namespace [] ["AddedBlockToQueue"]
    , Namespace [] ["BlockInTheFuture"]
    , Namespace [] ["AddedBlockToVolatileDB"]
    , Namespace [] ["PoppedBlockFromQueue"]
    , Namespace [] ["TryAddToCurrentChain"]
    , Namespace [] ["TrySwitchToAFork"]
    , Namespace [] ["StoreButDontChange"]
    , Namespace [] ["ChangingSelection"]
    , Namespace [] ["AddedToCurrentChain"]
    , Namespace [] ["SwitchedToAFork"]
    , Namespace [] ["ChainSelectionForFutureBlock"]
    ]
    ++ map (nsPrependInner "PipeliningEvent")
          (allNamespaces :: [Namespace (ChainDB.TracePipeliningEvent blk)])
    ++ map (nsPrependInner "AddBlockValidation")
          (allNamespaces :: [Namespace (ChainDB.TraceValidationEvent blk)])

--------------------------------------------------------------------------------
-- ChainDB TracePipeliningEvent
--------------------------------------------------------------------------------

instance ( ConvertRawHash (Header blk)
         , HasHeader (Header blk)
         ) => LogFormatting (ChainDB.TracePipeliningEvent blk) where
  forHuman (ChainDB.SetTentativeHeader hdr enclosing) =
      case enclosing of
        RisingEdge  -> "About to set tentative header to " <> renderPointAsPhrase (blockPoint hdr)
        FallingEdge -> "Set tentative header to " <> renderPointAsPhrase (blockPoint hdr)
  forHuman (ChainDB.TrapTentativeHeader hdr) =
      "Discovered trap tentative header " <> renderPointAsPhrase (blockPoint hdr)
  forHuman (ChainDB.OutdatedTentativeHeader hdr) =
      "Tentative header is now outdated " <> renderPointAsPhrase (blockPoint hdr)

  forMachine dtals (ChainDB.SetTentativeHeader hdr enclosing) =
      mconcat $ [ "kind" .= String "SetTentativeHeader"
                , "block" .= renderPointForDetails dtals (blockPoint hdr) ]
                <> [ "risingEdge" .= True | RisingEdge <- [enclosing] ]
  forMachine dtals (ChainDB.TrapTentativeHeader hdr) =
      mconcat [ "kind" .= String "TrapTentativeHeader"
               , "block" .= renderPointForDetails dtals (blockPoint hdr) ]
  forMachine dtals (ChainDB.OutdatedTentativeHeader hdr) =
      mconcat [ "kind" .= String "OutdatedTentativeHeader"
               , "block" .= renderPointForDetails dtals (blockPoint hdr)]

instance MetaTrace  (ChainDB.TracePipeliningEvent blk) where
  namespaceFor ChainDB.SetTentativeHeader {} =
    Namespace [] ["SetTentativeHeader"]
  namespaceFor ChainDB.TrapTentativeHeader {} =
    Namespace [] ["TrapTentativeHeader"]
  namespaceFor ChainDB.OutdatedTentativeHeader {} =
    Namespace [] ["OutdatedTentativeHeader"]

  severityFor (Namespace _ ["SetTentativeHeader"]) _ = Just Debug
  severityFor (Namespace _ ["TrapTentativeHeader"]) _ = Just Debug
  severityFor (Namespace _ ["OutdatedTentativeHeader"]) _ = Just Debug
  severityFor _ _ = Nothing

  documentFor (Namespace _ ["SetTentativeHeader"]) = Just
    "A new tentative header got set"
  documentFor (Namespace _ ["TrapTentativeHeader"]) = Just
    "The body of tentative block turned out to be invalid."
  documentFor (Namespace _ ["OutdatedTentativeHeader"]) = Just
    "We selected a new (better) chain, which cleared the previous tentative header."
  documentFor _ = Nothing

  allNamespaces =
    [ Namespace [] ["SetTentativeHeader"]
    , Namespace [] ["TrapTentativeHeader"]
    , Namespace [] ["OutdatedTentativeHeader"]
    ]


addedHdrsNewChain :: HasHeader (Header blk)
  => AF.AnchoredFragment (Header blk)
  -> AF.AnchoredFragment (Header blk)
  -> [Header blk]
addedHdrsNewChain fro to_ =
 case AF.intersect fro to_ of
   Just (_, _, _, s2 :: AF.AnchoredFragment (Header blk)) ->
     AF.toOldestFirst s2
   Nothing -> [] -- No sense to do validation here.

--------------------------------------------------------------------------------
-- ChainDB TraceFollowerEvent
--------------------------------------------------------------------------------

instance (ConvertRawHash blk, StandardHash blk) =>
            LogFormatting (ChainDB.TraceFollowerEvent blk) where
  forHuman ChainDB.NewFollower = "A new Follower was created"
  forHuman (ChainDB.FollowerNoLongerInMem _rrs) =
    "The follower was in the 'FollowerInMem' state but its point is no longer on\
    \ the in-memory chain fragment, so it has to switch to the\
    \ 'FollowerInImmutableDB' state"
  forHuman (ChainDB.FollowerSwitchToMem point slot) =
    "The follower was in the 'FollowerInImmutableDB' state and is switched to\
    \ the 'FollowerInMem' state. Point: " <> showT point <> " slot: " <> showT slot
  forHuman (ChainDB.FollowerNewImmIterator point slot) =
    "The follower is in the 'FollowerInImmutableDB' state but the iterator is\
    \ exhausted while the ImmDB has grown, so we open a new iterator to\
    \ stream these blocks too. Point: " <> showT point <> " slot: " <> showT slot

  forMachine _dtal ChainDB.NewFollower =
      mconcat [ "kind" .= String "NewFollower" ]
  forMachine _dtal (ChainDB.FollowerNoLongerInMem _) =
      mconcat [ "kind" .= String "FollowerNoLongerInMem" ]
  forMachine _dtal (ChainDB.FollowerSwitchToMem _ _) =
      mconcat [ "kind" .= String "FollowerSwitchToMem" ]
  forMachine _dtal (ChainDB.FollowerNewImmIterator _ _) =
      mconcat [ "kind" .= String "FollowerNewImmIterator" ]

instance MetaTrace (ChainDB.TraceFollowerEvent blk) where
  namespaceFor ChainDB.NewFollower =
    Namespace [] ["NewFollower"]
  namespaceFor ChainDB.FollowerNoLongerInMem {} =
    Namespace [] ["FollowerNoLongerInMem"]
  namespaceFor ChainDB.FollowerSwitchToMem {} =
    Namespace [] ["FollowerSwitchToMem"]
  namespaceFor ChainDB.FollowerNewImmIterator {} =
    Namespace [] ["FollowerNewImmIterator"]

  severityFor (Namespace _ ["NewFollower"]) _ = Just Debug
  severityFor (Namespace _ ["FollowerNoLongerInMem"]) _ = Just Debug
  severityFor (Namespace _ ["FollowerSwitchToMem"]) _ = Just Debug
  severityFor (Namespace _ ["FollowerNewImmIterator"]) _ = Just Debug
  severityFor _ _ = Nothing

  documentFor (Namespace _ ["NewFollower"]) = Just
    "A new follower was created."
  documentFor (Namespace _ ["FollowerNoLongerInMem"]) = Just
    "The follower was in 'FollowerInMem' state and is switched to\
    \ the 'FollowerInImmutableDB' state."
  documentFor (Namespace _ ["FollowerSwitchToMem"]) = Just
     "The follower was in the 'FollowerInImmutableDB' state and is switched to\
   \ the 'FollowerInMem' state."
  documentFor (Namespace _ ["FollowerNewImmIterator"]) = Just
     "The follower is in the 'FollowerInImmutableDB' state but the iterator is\
         \ exhausted while the ImmDB has grown, so we open a new iterator to\
         \ stream these blocks too."
  documentFor _ = Nothing

  allNamespaces =
    [ Namespace [] ["NewFollower"]
    , Namespace [] ["FollowerNoLongerInMem"]
    , Namespace [] ["FollowerSwitchToMem"]
    , Namespace [] ["FollowerNewImmIterator"]
    ]


--------------------------------------------------------------------------------
-- ChainDB TraceCopyToImmutableDB
--------------------------------------------------------------------------------

instance ConvertRawHash blk
          => LogFormatting (ChainDB.TraceCopyToImmutableDBEvent blk) where
  forHuman (ChainDB.CopiedBlockToImmutableDB pt) =
      "Copied block " <> renderPointAsPhrase pt <> " to the ImmDB"
  forHuman ChainDB.NoBlocksToCopyToImmutableDB  =
      "There are no blocks to copy to the ImmDB"

  forMachine dtals (ChainDB.CopiedBlockToImmutableDB pt) =
      mconcat [ "kind" .= String "CopiedBlockToImmutableDB"
               , "slot" .= forMachine dtals pt ]
  forMachine _dtals ChainDB.NoBlocksToCopyToImmutableDB =
      mconcat [ "kind" .= String "NoBlocksToCopyToImmutableDB" ]

instance MetaTrace (ChainDB.TraceCopyToImmutableDBEvent blk) where
  namespaceFor ChainDB.CopiedBlockToImmutableDB {} =
    Namespace [] ["CopiedBlockToImmutableDB"]
  namespaceFor ChainDB.NoBlocksToCopyToImmutableDB {} =
    Namespace [] ["NoBlocksToCopyToImmutableDB"]

  severityFor (Namespace _ ["CopiedBlockToImmutableDB"]) _ = Just Debug
  severityFor (Namespace _ ["NoBlocksToCopyToImmutableDB"]) _ = Just Debug
  severityFor _ _ = Nothing

  documentFor (Namespace _ ["CopiedBlockToImmutableDB"]) = Just
    "A block was successfully copied to the ImmDB."
  documentFor (Namespace _ ["NoBlocksToCopyToImmutableDB"]) = Just
     "There are no block to copy to the ImmDB."
  documentFor _ = Nothing

  allNamespaces =
    [ Namespace [] ["CopiedBlockToImmutableDB"]
    , Namespace [] ["NoBlocksToCopyToImmutableDB"]
    ]

-- --------------------------------------------------------------------------------
-- -- ChainDB GCEvent
-- --------------------------------------------------------------------------------

instance LogFormatting (ChainDB.TraceGCEvent blk) where
  forHuman (ChainDB.PerformedGC slot) =
      "Performed a garbage collection for " <> condenseT slot
  forHuman (ChainDB.ScheduledGC slot _difft) =
      "Scheduled a garbage collection for " <> condenseT slot

  forMachine dtals (ChainDB.PerformedGC slot) =
      mconcat [ "kind" .= String "PerformedGC"
               , "slot" .= forMachine dtals slot ]
  forMachine dtals (ChainDB.ScheduledGC slot difft) =
      mconcat $ [ "kind" .= String "ScheduledGC"
                 , "slot" .= forMachine dtals slot ] <>
                 [ "difft" .= String ((Text.pack . show) difft) | dtals >= DDetailed]

instance MetaTrace (ChainDB.TraceGCEvent blk) where
  namespaceFor ChainDB.PerformedGC {} =
    Namespace [] ["PerformedGC"]
  namespaceFor ChainDB.ScheduledGC {} =
    Namespace [] ["ScheduledGC"]

  severityFor (Namespace _ ["PerformedGC"]) _ = Just Debug
  severityFor (Namespace _ ["ScheduledGC"]) _ = Just Debug
  severityFor _ _ = Nothing

  documentFor (Namespace _ ["PerformedGC"]) = Just
    "A garbage collection for the given 'SlotNo' was performed."
  documentFor (Namespace _ ["ScheduledGC"]) = Just
     "A garbage collection for the given 'SlotNo' was scheduled to happen\
     \ at the given time."
  documentFor _ = Nothing

  allNamespaces =
    [ Namespace [] ["PerformedGC"]
    , Namespace [] ["ScheduledGC"]
    ]

-- --------------------------------------------------------------------------------
-- -- TraceInitChainSelEvent
-- --------------------------------------------------------------------------------

instance (ConvertRawHash blk, LedgerSupportsProtocol blk)
  => LogFormatting (ChainDB.TraceInitChainSelEvent blk) where
    forHuman (ChainDB.InitChainSelValidation v) = forHuman v
    forHuman (ChainDB.InitalChainSelected {}) =
        "Initial chain selected"
    forHuman (ChainDB.StartedInitChainSelection {}) =
        "Started initial chain selection"

    forMachine dtal (ChainDB.InitChainSelValidation v) = forMachine dtal v
    forMachine _dtal ChainDB.InitalChainSelected =
      mconcat ["kind" .= String "Follower.InitalChainSelected"]
    forMachine _dtal ChainDB.StartedInitChainSelection =
      mconcat ["kind" .= String "Follower.StartedInitChainSelection"]

    asMetrics (ChainDB.InitChainSelValidation v) = asMetrics v
    asMetrics ChainDB.InitalChainSelected        = []
    asMetrics ChainDB.StartedInitChainSelection  = []

instance MetaTrace (ChainDB.TraceInitChainSelEvent blk) where
  namespaceFor ChainDB.InitalChainSelected {} =
    Namespace [] ["InitalChainSelected"]
  namespaceFor ChainDB.StartedInitChainSelection {} =
    Namespace [] ["StartedInitChainSelection"]
  namespaceFor (ChainDB.InitChainSelValidation ev') =
    nsPrependInner "Validation" (namespaceFor ev')

  severityFor (Namespace _ ["InitalChainSelected"]) _ = Just Info
  severityFor (Namespace _ ["StartedInitChainSelection"]) _ = Just Info
  severityFor (Namespace out ("InitChainSelValidation" : tl))
                            (Just (ChainDB.InitChainSelValidation ev')) =
    severityFor (Namespace out tl) (Just ev')
  severityFor (Namespace out ("InitChainSelValidation" : tl)) Nothing =
    severityFor (Namespace out tl ::
      Namespace (ChainDB.TraceValidationEvent blk)) Nothing
  severityFor _ _ = Nothing

  privacyFor (Namespace out ("InitChainSelValidation" : tl))
              (Just (ChainDB.InitChainSelValidation ev')) =
    privacyFor (Namespace out tl) (Just ev')
  privacyFor (Namespace out ("InitChainSelValidation" : tl)) Nothing =
    privacyFor (Namespace out tl ::
      Namespace (ChainDB.TraceValidationEvent blk)) Nothing
  privacyFor _ _ = Just Public

  detailsFor (Namespace out ("InitChainSelValidation" : tl))
              (Just (ChainDB.InitChainSelValidation ev')) =
    detailsFor (Namespace out tl) (Just ev')
  detailsFor (Namespace out ("InitChainSelValidation" : tl)) Nothing =
    detailsFor (Namespace out tl ::
      Namespace (ChainDB.TraceValidationEvent blk)) Nothing
  detailsFor _ _ = Just DNormal

  metricsDocFor (Namespace out ("InitChainSelValidation" : tl)) =
    metricsDocFor (Namespace out tl :: Namespace (ChainDB.TraceValidationEvent blk))
  metricsDocFor _ = []

  documentFor (Namespace _ ["InitalChainSelected"]) = Just
    "A garbage collection for the given 'SlotNo' was performed."
  documentFor (Namespace _ ["StartedInitChainSelection"]) = Just
     "A garbage collection for the given 'SlotNo' was scheduled to happen\
     \ at the given time."
  documentFor (Namespace o ("InitChainSelValidation" : tl)) =
     documentFor (Namespace o tl :: Namespace (ChainDB.TraceValidationEvent blk))
  documentFor _ = Nothing

  allNamespaces =
    [ Namespace [] ["InitalChainSelected"]
    , Namespace [] ["StartedInitChainSelection"]
    ]
    ++ map (nsPrependInner "InitChainSelValidation")
          (allNamespaces :: [Namespace (ChainDB.TraceValidationEvent blk)])



--------------------------------------------------------------------------------
-- ChainDB TraceValidationEvent
--------------------------------------------------------------------------------

instance ( LedgerSupportsProtocol blk
         , ConvertRawHash (Header blk)
         , ConvertRawHash blk
         , LogFormatting (RealPoint blk))
         => LogFormatting (ChainDB.TraceValidationEvent blk) where
    forHuman (ChainDB.InvalidBlock err pt) =
        "Invalid block " <> renderRealPointAsPhrase pt <> ": " <> showT err
    forHuman (ChainDB.ValidCandidate c) =
        "Valid candidate " <> renderPointAsPhrase (AF.headPoint c)
    forHuman (ChainDB.CandidateContainsFutureBlocks c hdrs) =
        "Candidate contains blocks from near future:  " <>
          renderPointAsPhrase (AF.headPoint c) <> ", slots " <>
          Text.intercalate ", " (map (renderPoint . headerPoint) hdrs)
    forHuman (ChainDB.CandidateContainsFutureBlocksExceedingClockSkew c hdrs) =
        "Candidate contains blocks from future exceeding clock skew limit: " <>
          renderPointAsPhrase (AF.headPoint c) <> ", slots " <>
          Text.intercalate ", " (map (renderPoint . headerPoint) hdrs)
    forHuman (ChainDB.UpdateLedgerDbTraceEvent
                (StartedPushingBlockToTheLedgerDb
                  (LedgerDB.PushStart start)
                  (LedgerDB.PushGoal goal)
                  (LedgerDB.Pushing curr))) =
            let fromSlot = unSlotNo $ realPointSlot start
                atSlot   = unSlotNo $ realPointSlot curr
                atDiff   = atSlot - fromSlot
                toSlot   = unSlotNo $ realPointSlot goal
                toDiff   = toSlot - fromSlot
            in
              "Pushing ledger state for block " <> renderRealPointAsPhrase curr <> ". Progress: " <>
              showProgressT (fromIntegral atDiff) (fromIntegral toDiff) <> "%"

    forMachine dtal  (ChainDB.InvalidBlock err pt) =
            mconcat [ "kind" .= String "InvalidBlock"
                     , "block" .= forMachine dtal pt
                     , "error" .= showT err ]
    forMachine dtal  (ChainDB.ValidCandidate c) =
            mconcat [ "kind" .= String "ValidCandidate"
                     , "block" .= renderPointForDetails dtal (AF.headPoint c) ]
    forMachine dtal  (ChainDB.CandidateContainsFutureBlocks c hdrs) =
            mconcat [ "kind" .= String "CandidateContainsFutureBlocks"
                     , "block"   .= renderPointForDetails dtal (AF.headPoint c)
                     , "headers" .= map (renderPointForDetails dtal . headerPoint) hdrs ]
    forMachine dtal  (ChainDB.CandidateContainsFutureBlocksExceedingClockSkew c hdrs) =
            mconcat [ "kind" .= String "CandidateContainsFutureBlocksExceedingClockSkew"
                     , "block"   .= renderPointForDetails dtal (AF.headPoint c)
                     , "headers" .= map (renderPointForDetails dtal . headerPoint) hdrs ]
    forMachine _dtal (ChainDB.UpdateLedgerDbTraceEvent
                        (StartedPushingBlockToTheLedgerDb
                          (LedgerDB.PushStart start)
                          (LedgerDB.PushGoal goal)
                          (LedgerDB.Pushing curr))) =
            mconcat [ "kind" .= String "UpdateLedgerDbTraceEvent.StartedPushingBlockToTheLedgerDb"
                     , "startingBlock" .= renderRealPoint start
                     , "currentBlock" .= renderRealPoint curr
                     , "targetBlock" .= renderRealPoint goal
                     ]

instance MetaTrace (ChainDB.TraceValidationEvent blk) where
    namespaceFor ChainDB.ValidCandidate {} =
      Namespace [] ["ValidCandidate"]
    namespaceFor ChainDB.CandidateContainsFutureBlocks {} =
      Namespace [] ["CandidateContainsFutureBlocks"]
    namespaceFor ChainDB.CandidateContainsFutureBlocksExceedingClockSkew {} =
      Namespace [] ["CandidateContainsFutureBlocksExceedingClockSkew"]
    namespaceFor ChainDB.InvalidBlock {} =
      Namespace [] ["InvalidBlock"]
    namespaceFor ChainDB.UpdateLedgerDbTraceEvent {} =
      Namespace [] ["UpdateLedgerDb"]

    severityFor (Namespace _ ["ValidCandidate"]) _ = Just Info
    severityFor (Namespace _ ["CandidateContainsFutureBlocks"]) _ = Just Debug
    severityFor (Namespace _ ["CandidateContainsFutureBlocksExceedingClockSkew"]) _ = Just Error
    severityFor (Namespace _ ["InvalidBlock"]) _ = Just Error
    severityFor (Namespace _ ["UpdateLedgerDb"]) _ = Just Debug
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["ValidCandidate"]) = Just
        "An event traced during validating performed while adding a block.\
        \ A candidate chain was valid."
    documentFor (Namespace _ ["CandidateContainsFutureBlocks"]) = Just
        "An event traced during validating performed while adding a block.\
        \ Candidate contains headers from the future which do no exceed the\
        \ clock skew."
    documentFor (Namespace _ ["CandidateContainsFutureBlocksExceedingClockSkew"]) = Just
        "An event traced during validating performed while adding a block.\
        \ Candidate contains headers from the future which exceed the\
        \ clock skew."
    documentFor (Namespace _ ["InvalidBlock"]) = Just
        "An event traced during validating performed while adding a block.\
        \ A point was found to be invalid."
    documentFor (Namespace _ ["UpdateLedgerDb"]) = Just ""
    documentFor _ = Nothing

    allNamespaces =
      [ Namespace [] ["ValidCandidate"]
      , Namespace [] ["CandidateContainsFutureBlocks"]
      , Namespace [] ["CandidateContainsFutureBlocksExceedingClockSkew"]
      , Namespace [] ["InvalidBlock"]
      , Namespace [] ["UpdateLedgerDb"]
      ]

--------------------------------------------------------------------------------
-- TraceOpenEvent
--------------------------------------------------------------------------------

instance ConvertRawHash blk
          => LogFormatting (ChainDB.TraceOpenEvent blk) where
  forHuman (ChainDB.OpenedDB immTip tip') =
          "Opened db with immutable tip at " <> renderPointAsPhrase immTip <>
          " and tip " <> renderPointAsPhrase tip'
  forHuman (ChainDB.ClosedDB immTip tip') =
          "Closed db with immutable tip at " <> renderPointAsPhrase immTip <>
          " and tip " <> renderPointAsPhrase tip'
  forHuman (ChainDB.OpenedImmutableDB immTip chunk) =
          "Opened imm db with immutable tip at " <> renderPointAsPhrase immTip <>
          " and chunk " <> showT chunk
  forHuman ChainDB.OpenedVolatileDB = "Opened vol db"
  forHuman ChainDB.OpenedLgrDB = "Opened lgr db"
  forHuman ChainDB.StartedOpeningDB = "Started opening Chain DB"
  forHuman ChainDB.StartedOpeningImmutableDB = "Started opening Immutable DB"
  forHuman ChainDB.StartedOpeningVolatileDB = "Started opening Volatile DB"
  forHuman ChainDB.StartedOpeningLgrDB = "Started opening Ledger DB"

  forMachine dtal (ChainDB.OpenedDB immTip tip')=
    mconcat [ "kind" .= String "OpenedDB"
             , "immtip" .= forMachine dtal immTip
             , "tip" .= forMachine dtal tip' ]
  forMachine dtal (ChainDB.ClosedDB immTip tip') =
    mconcat [ "kind" .= String "TraceOpenEvent.ClosedDB"
             , "immtip" .= forMachine dtal immTip
             , "tip" .= forMachine dtal tip' ]
  forMachine dtal (ChainDB.OpenedImmutableDB immTip epoch) =
    mconcat [ "kind" .= String "OpenedImmutableDB"
             , "immtip" .= forMachine dtal immTip
             , "epoch" .= String ((Text.pack . show) epoch) ]
  forMachine _dtal ChainDB.OpenedVolatileDB =
      mconcat [ "kind" .= String "OpenedVolatileDB" ]
  forMachine _dtal ChainDB.OpenedLgrDB =
      mconcat [ "kind" .= String "OpenedLgrDB" ]
  forMachine _dtal ChainDB.StartedOpeningDB =
      mconcat ["kind" .= String "StartedOpeningDB"]
  forMachine _dtal ChainDB.StartedOpeningImmutableDB =
      mconcat ["kind" .= String "StartedOpeningImmutableDB"]
  forMachine _dtal ChainDB.StartedOpeningVolatileDB =
      mconcat ["kind" .= String "StartedOpeningVolatileDB"]
  forMachine _dtal ChainDB.StartedOpeningLgrDB =
      mconcat ["kind" .= String "StartedOpeningLgrDB"]

instance MetaTrace (ChainDB.TraceOpenEvent blk) where
    namespaceFor ChainDB.OpenedDB {} =
      Namespace [] ["OpenedDB"]
    namespaceFor ChainDB.ClosedDB {} =
      Namespace [] ["ClosedDB"]
    namespaceFor ChainDB.OpenedImmutableDB {} =
      Namespace [] ["OpenedImmutableDB"]
    namespaceFor ChainDB.OpenedVolatileDB {} =
      Namespace [] ["OpenedVolatileDB"]
    namespaceFor ChainDB.OpenedLgrDB {} =
      Namespace [] ["OpenedLgrDB"]
    namespaceFor ChainDB.StartedOpeningDB {} =
      Namespace [] ["StartedOpeningDB"]
    namespaceFor ChainDB.StartedOpeningImmutableDB {} =
      Namespace [] ["StartedOpeningImmutableDB"]
    namespaceFor ChainDB.StartedOpeningVolatileDB {} =
      Namespace [] ["StartedOpeningVolatileDB"]
    namespaceFor ChainDB.StartedOpeningLgrDB {} =
      Namespace [] ["StartedOpeningLgrDB"]

    severityFor (Namespace _ ["OpenedDB"]) _ = Just Info
    severityFor (Namespace _ ["ClosedDB"]) _ = Just Info
    severityFor (Namespace _ ["OpenedImmutableDB"]) _ = Just Info
    severityFor (Namespace _ ["OpenedVolatileDB"]) _ = Just Info
    severityFor (Namespace _ ["OpenedLgrDB"]) _ = Just Info
    severityFor (Namespace _ ["StartedOpeningDB"]) _ = Just Info
    severityFor (Namespace _ ["StartedOpeningImmutableDB"]) _ = Just Info
    severityFor (Namespace _ ["StartedOpeningVolatileDB"]) _ = Just Info
    severityFor (Namespace _ ["StartedOpeningLgrDB"]) _ = Just Info
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["OpenedDB"]) = Just
      "The ChainDB was opened."
    documentFor (Namespace _ ["ClosedDB"]) = Just
      "The ChainDB was closed."
    documentFor (Namespace _ ["OpenedImmutableDB"]) = Just
      "The ImmDB was opened."
    documentFor (Namespace _ ["OpenedVolatileDB"]) = Just
      "The VolatileDB was opened."
    documentFor (Namespace _ ["OpenedLgrDB"]) = Just
      "The LedgerDB was opened."
    documentFor (Namespace _ ["StartedOpeningDB"]) = Just
      ""
    documentFor (Namespace _ ["StartedOpeningImmutableDB"]) = Just
      ""
    documentFor (Namespace _ ["StartedOpeningVolatileDB"]) = Just
      ""
    documentFor (Namespace _ ["StartedOpeningLgrDB"]) = Just
      ""
    documentFor _ = Nothing

    allNamespaces =
      [ Namespace [] ["OpenedDB"]
      , Namespace [] ["ClosedDB"]
      , Namespace [] ["OpenedImmutableDB"]
      , Namespace [] ["OpenedVolatileDB"]
      , Namespace [] ["OpenedLgrDB"]
      , Namespace [] ["StartedOpeningDB"]
      , Namespace [] ["StartedOpeningImmutableDB"]
      , Namespace [] ["StartedOpeningVolatileDB"]
      , Namespace [] ["StartedOpeningLgrDB"]
      ]

--------------------------------------------------------------------------------
-- IteratorEvent
--------------------------------------------------------------------------------

instance  ( StandardHash blk
          , ConvertRawHash blk
          ) => LogFormatting (ChainDB.TraceIteratorEvent blk) where
  forHuman (ChainDB.UnknownRangeRequested ev') = forHuman ev'
  forHuman (ChainDB.BlockMissingFromVolatileDB realPt) =
      "This block is no longer in the VolatileDB because it has been garbage\
         \ collected. It might now be in the ImmDB if it was part of the\
         \ current chain. Block: " <> renderRealPoint realPt
  forHuman (ChainDB.StreamFromImmutableDB sFrom sTo) =
      "Stream only from the ImmDB. StreamFrom:" <> showT sFrom <>
        " StreamTo: " <> showT sTo
  forHuman (ChainDB.StreamFromBoth sFrom sTo pts) =
      "Stream from both the VolatileDB and the ImmDB."
        <> " StreamFrom: " <> showT sFrom <> " StreamTo: " <> showT sTo
        <> " Points: " <> showT (map renderRealPoint pts)
  forHuman (ChainDB.StreamFromVolatileDB sFrom sTo pts) =
      "Stream only from the VolatileDB."
        <> " StreamFrom: " <> showT sFrom <> " StreamTo: " <> showT sTo
        <> " Points: " <> showT (map renderRealPoint pts)
  forHuman (ChainDB.BlockWasCopiedToImmutableDB pt) =
      "This block has been garbage collected from the VolatileDB is now\
        \ found and streamed from the ImmDB. Block: " <> renderRealPoint pt
  forHuman (ChainDB.BlockGCedFromVolatileDB pt) =
      "This block no longer in the VolatileDB and isn't in the ImmDB\
        \ either; it wasn't part of the current chain. Block: " <> renderRealPoint pt
  forHuman ChainDB.SwitchBackToVolatileDB = "SwitchBackToVolatileDB"

  forMachine _dtal (ChainDB.UnknownRangeRequested unkRange) =
    mconcat [ "kind" .= String "UnknownRangeRequested"
             , "range" .= String (showT unkRange)
             ]
  forMachine _dtal (ChainDB.StreamFromVolatileDB streamFrom streamTo realPt) =
    mconcat [ "kind" .= String "StreamFromVolatileDB"
             , "from" .= String (showT streamFrom)
             , "to" .= String (showT streamTo)
             , "point" .= String (Text.pack . show $ map renderRealPoint realPt)
             ]
  forMachine _dtal (ChainDB.StreamFromImmutableDB streamFrom streamTo) =
    mconcat [ "kind" .= String "StreamFromImmutableDB"
             , "from" .= String (showT streamFrom)
             , "to" .= String (showT streamTo)
             ]
  forMachine _dtal (ChainDB.StreamFromBoth streamFrom streamTo realPt) =
    mconcat [ "kind" .= String "StreamFromBoth"
             , "from" .= String (showT streamFrom)
             , "to" .= String (showT streamTo)
             , "point" .= String (Text.pack . show $ map renderRealPoint realPt)
             ]
  forMachine _dtal (ChainDB.BlockMissingFromVolatileDB realPt) =
    mconcat [ "kind" .= String "BlockMissingFromVolatileDB"
             , "point" .= String (renderRealPoint realPt)
             ]
  forMachine _dtal (ChainDB.BlockWasCopiedToImmutableDB realPt) =
    mconcat [ "kind" .= String "BlockWasCopiedToImmutableDB"
             , "point" .= String (renderRealPoint realPt)
             ]
  forMachine _dtal (ChainDB.BlockGCedFromVolatileDB realPt) =
    mconcat [ "kind" .= String "BlockGCedFromVolatileDB"
             , "point" .= String (renderRealPoint realPt)
             ]
  forMachine _dtal ChainDB.SwitchBackToVolatileDB =
    mconcat ["kind" .= String "SwitchBackToVolatileDB"
             ]

instance MetaTrace (ChainDB.TraceIteratorEvent blk) where
    namespaceFor (ChainDB.UnknownRangeRequested ur) =
      nsPrependInner "UnknownRangeRequested" (namespaceFor ur)
    namespaceFor ChainDB.StreamFromVolatileDB {} =
      Namespace [] ["StreamFromVolatileDB"]
    namespaceFor ChainDB.StreamFromImmutableDB {} =
      Namespace [] ["StreamFromImmutableDB"]
    namespaceFor ChainDB.StreamFromBoth {} =
      Namespace [] ["StreamFromBoth"]
    namespaceFor ChainDB.BlockMissingFromVolatileDB {} =
      Namespace [] ["BlockMissingFromVolatileDB"]
    namespaceFor ChainDB.BlockWasCopiedToImmutableDB {} =
      Namespace [] ["BlockWasCopiedToImmutableDB"]
    namespaceFor ChainDB.BlockGCedFromVolatileDB {} =
      Namespace [] ["BlockGCedFromVolatileDB"]
    namespaceFor ChainDB.SwitchBackToVolatileDB {} =
      Namespace [] ["SwitchBackToVolatileDB"]


    severityFor (Namespace out ("UnknownRangeRequested" : tl))
                (Just (ChainDB.UnknownRangeRequested ur)) =
      severityFor (Namespace out tl) (Just ur)
    severityFor (Namespace out ("UnknownRangeRequested" : tl)) Nothing =
      severityFor (Namespace out tl :: Namespace (ChainDB.UnknownRange blk)) Nothing
    severityFor _ _ = Just Debug

    privacyFor (Namespace out ("UnknownRangeRequested" : tl))
                (Just (ChainDB.UnknownRangeRequested ev')) =
      privacyFor (Namespace out tl) (Just ev')
    privacyFor (Namespace out ("UnknownRangeRequested" : tl)) Nothing =
      privacyFor (Namespace out tl ::
        Namespace (ChainDB.UnknownRange blk)) Nothing
    privacyFor _ _ = Just Public

    detailsFor (Namespace out ("UnknownRangeRequested" : tl))
                (Just (ChainDB.UnknownRangeRequested ev')) =
      detailsFor (Namespace out tl) (Just ev')
    detailsFor (Namespace out ("UnknownRangeRequested" : tl)) Nothing =
      detailsFor (Namespace out tl ::
        Namespace (ChainDB.UnknownRange blk)) Nothing
    detailsFor _ _ = Just DNormal

    documentFor (Namespace out ("UnknownRangeRequested" : tl)) =
      documentFor (Namespace out tl :: Namespace (ChainDB.UnknownRange blk))
    documentFor (Namespace _ ["StreamFromVolatileDB"]) = Just
       "Stream only from the VolatileDB."
    documentFor (Namespace _ ["StreamFromImmutableDB"]) = Just
      "Stream only from the ImmDB."
    documentFor (Namespace _ ["StreamFromBoth"]) = Just
      "Stream from both the VolatileDB and the ImmDB."
    documentFor (Namespace _ ["BlockMissingFromVolatileDB"]) = Just
      "A block is no longer in the VolatileDB because it has been garbage\
       \ collected. It might now be in the ImmDB if it was part of the\
       \ current chain."
    documentFor (Namespace _ ["BlockWasCopiedToImmutableDB"]) = Just
      "A block that has been garbage collected from the VolatileDB is now\
       \ found and streamed from the ImmDB."
    documentFor (Namespace _ ["BlockGCedFromVolatileDB"]) = Just
      "A block is no longer in the VolatileDB and isn't in the ImmDB\
       \ either; it wasn't part of the current chain."
    documentFor (Namespace _ ["SwitchBackToVolatileDB"]) = Just
      "We have streamed one or more blocks from the ImmDB that were part\
      \ of the VolatileDB when initialising the iterator. Now, we have to look\
      \ back in the VolatileDB again because the ImmDB doesn't have the\
      \ next block we're looking for."
    documentFor _ = Nothing

    allNamespaces =
      [ Namespace [] ["StreamFromVolatileDB"]
      , Namespace [] ["StreamFromImmutableDB"]
      , Namespace [] ["StreamFromBoth"]
      , Namespace [] ["BlockMissingFromVolatileDB"]
      , Namespace [] ["BlockWasCopiedToImmutableDB"]
      , Namespace [] ["BlockGCedFromVolatileDB"]
      , Namespace [] ["SwitchBackToVolatileDB"]
      ]
      ++ map  (nsPrependInner "UnknownRangeRequested")
              (allNamespaces :: [Namespace (ChainDB.UnknownRange blk)])

--------------------------------------------------------------------------------
-- UnknownRange
--------------------------------------------------------------------------------

instance  ( StandardHash blk
          , ConvertRawHash blk
          ) => LogFormatting (ChainDB.UnknownRange blk) where
  forHuman (ChainDB.MissingBlock realPt) =
      "The block at the given point was not found in the ChainDB."
        <> renderRealPoint realPt
  forHuman (ChainDB.ForkTooOld streamFrom) =
      "The requested range forks off too far in the past"
        <> showT streamFrom

  forMachine _dtal (ChainDB.MissingBlock realPt) =
    mconcat [ "kind"  .= String "MissingBlock"
             , "point" .= String (renderRealPoint realPt)
             ]
  forMachine _dtal (ChainDB.ForkTooOld streamFrom) =
    mconcat [ "kind" .= String "ForkTooOld"
             , "from" .= String (showT streamFrom)
             ]

instance MetaTrace (ChainDB.UnknownRange blk) where
    namespaceFor ChainDB.MissingBlock {} = Namespace [] ["MissingBlock"]
    namespaceFor ChainDB.ForkTooOld {} = Namespace []  ["ForkTooOld"]

    -- TODO Tracers Is this really as intended?
    severityFor _ _ = Just Debug

    documentFor (Namespace _ ["MissingBlock"]) = Just
      ""
    documentFor (Namespace _ ["ForkTooOld"]) = Just
      ""
    documentFor _ = Nothing

    allNamespaces =
      [ Namespace [] ["MissingBlock"]
      , Namespace [] ["ForkTooOld"]
      ]

-- --------------------------------------------------------------------------------
-- -- LedgerDB.TraceEvent
-- --------------------------------------------------------------------------------

instance ( StandardHash blk
         , ConvertRawHash blk)
         => LogFormatting (LedgerDB.TraceEvent blk) where
  forHuman (LedgerDB.TookSnapshot snap pt) =
      "Took ledger snapshot " <> showT snap <>
        " at " <> renderRealPointAsPhrase pt
  forHuman (LedgerDB.DeletedSnapshot snap) =
      "Deleted old snapshot " <> showT snap
  forHuman (LedgerDB.InvalidSnapshot snap failure) =
      "Invalid snapshot " <> showT snap <> showT failure

  forMachine dtals (LedgerDB.TookSnapshot snap pt) =
    mconcat [ "kind" .= String "TookSnapshot"
             , "snapshot" .= forMachine dtals snap
             , "tip" .= show pt ]
  forMachine dtals (LedgerDB.DeletedSnapshot snap) =
    mconcat [ "kind" .= String "DeletedSnapshot"
             , "snapshot" .= forMachine dtals snap ]
  forMachine dtals (LedgerDB.InvalidSnapshot snap failure) =
    mconcat [ "kind" .= String "InvalidSnapshot"
             , "snapshot" .= forMachine dtals snap
             , "failure" .= show failure ]

instance MetaTrace (LedgerDB.TraceEvent blk) where
    namespaceFor LedgerDB.TookSnapshot {} = Namespace [] ["TookSnapshot"]
    namespaceFor LedgerDB.DeletedSnapshot {} = Namespace [] ["DeletedSnapshot"]
    namespaceFor LedgerDB.InvalidSnapshot {} = Namespace [] ["InvalidSnapshot"]

    severityFor  (Namespace _ ["TookSnapshot"]) _ = Just Info
    severityFor  (Namespace _ ["DeletedSnapshot"]) _ = Just Debug
    severityFor  (Namespace _ ["InvalidSnapshot"]) _ = Just Error
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["TookSnapshot"]) = Just
          "A snapshot was written to disk."
    documentFor (Namespace _ ["DeletedSnapshot"]) = Just
          "A snapshot was written to disk."
    documentFor (Namespace _ ["InvalidSnapshot"]) = Just
          "An on disk snapshot was skipped because it was invalid."
    documentFor _ = Nothing

    allNamespaces =
      [ Namespace [] ["TookSnapshot"]
      , Namespace [] ["DeletedSnapshot"]
      , Namespace [] ["InvalidSnapshot"]
      ]


--------------------------------------------------------------------------------
-- LedgerDB TraceReplayEvent
--------------------------------------------------------------------------------

instance (StandardHash blk, ConvertRawHash blk)
          => LogFormatting (LedgerDB.TraceReplayEvent blk) where
  forHuman (LedgerDB.ReplayFromGenesis _replayTo) =
      "Replaying ledger from genesis"
  forHuman (LedgerDB.ReplayFromSnapshot snap tip' _ _) =
      "Replaying ledger from snapshot " <> showT snap <> " at " <>
        renderRealPointAsPhrase tip'
  forHuman (LedgerDB.ReplayedBlock
              pt
              _ledgerEvents
              (LedgerDB.ReplayStart replayFrom)
              (LedgerDB.ReplayGoal replayTo)) =
          let fromSlot = withOrigin 0 id $ unSlotNo <$> pointSlot replayFrom
              atSlot   = unSlotNo $ realPointSlot pt
              atDiff   = atSlot - fromSlot
              toSlot   = withOrigin 0 id $ unSlotNo <$> pointSlot replayTo
              toDiff   = toSlot - fromSlot
          in
             "Replayed block: slot "
          <> showT atSlot
          <> " out of "
          <> showT toSlot
          <> ". Progress: "
          <> showProgressT (fromIntegral atDiff) (fromIntegral toDiff)
          <> "%"

  forMachine _dtal (LedgerDB.ReplayFromGenesis _replayTo) =
      mconcat [ "kind" .= String "ReplayFromGenesis" ]
  forMachine dtal (LedgerDB.ReplayFromSnapshot snap tip' _ _) =
      mconcat [ "kind" .= String "ReplayFromSnapshot"
               , "snapshot" .= forMachine dtal snap
               , "tip" .= show tip' ]
  forMachine _dtal (LedgerDB.ReplayedBlock
                      pt
                      _ledgerEvents
                      _
                      (LedgerDB.ReplayGoal replayTo)) =
      mconcat [ "kind" .= String "ReplayedBlock"
               , "slot" .= unSlotNo (realPointSlot pt)
               , "tip"  .= withOrigin 0 unSlotNo (pointSlot replayTo) ]

instance MetaTrace (LedgerDB.TraceReplayEvent blk) where
    namespaceFor LedgerDB.ReplayFromGenesis {} = Namespace [] ["ReplayFromGenesis"]
    namespaceFor LedgerDB.ReplayFromSnapshot {} = Namespace [] ["ReplayFromSnapshot"]
    namespaceFor LedgerDB.ReplayedBlock {} = Namespace [] ["ReplayedBlock"]

    severityFor  (Namespace _ ["ReplayFromGenesis"]) _ = Just Info
    severityFor  (Namespace _ ["ReplayFromSnapshot"]) _ = Just Info
    severityFor  (Namespace _ ["ReplayedBlock"]) _ = Just Info
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["ReplayFromGenesis"]) = Just
      "There were no LedgerDB snapshots on disk, so we're replaying all\
      \ blocks starting from Genesis against the initial ledger.\
      \ The @replayTo@ parameter corresponds to the block at the tip of the\
      \ ImmDB, i.e., the last block to replay."
    documentFor (Namespace _ ["ReplayFromSnapshot"]) = Just
      "There was a LedgerDB snapshot on disk corresponding to the given tip.\
      \ We're replaying more recent blocks against it.\
      \ The @replayTo@ parameter corresponds to the block at the tip of the\
      \ ImmDB, i.e., the last block to replay."
    documentFor (Namespace _ ["ReplayedBlock"]) = Just
      "We replayed the given block (reference) on the genesis snapshot\
      \ during the initialisation of the LedgerDB.\
      \\n\
      \ The @blockInfo@ parameter corresponds replayed block and the @replayTo@\
      \ parameter corresponds to the block at the tip of the ImmDB, i.e.,\
      \ the last block to replay."
    documentFor _ = Nothing

    allNamespaces =
      [ Namespace [] ["ReplayFromGenesis"]
      , Namespace [] ["ReplayFromSnapshot"]
      , Namespace [] ["ReplayedBlock"]
      ]

--------------------------------------------------------------------------------
-- ImmDB.TraceEvent
--------------------------------------------------------------------------------

instance (ConvertRawHash blk, StandardHash blk)
  => LogFormatting (ImmDB.TraceEvent blk) where
    forMachine _dtal ImmDB.NoValidLastLocation =
      mconcat [ "kind" .= String "NoValidLastLocation" ]
    forMachine _dtal (ImmDB.ValidatedLastLocation chunkNo immTip) =
      mconcat [ "kind" .= String "ValidatedLastLocation"
               , "chunkNo" .= String (renderChunkNo chunkNo)
               , "immTip" .= String (renderTipHash immTip)
               , "blockNo" .= String (renderTipBlockNo immTip)
               ]
    forMachine dtal (ImmDB.ChunkValidationEvent traceChunkValidation) =
      forMachine dtal traceChunkValidation
    forMachine _dtal (ImmDB.DeletingAfter immTipWithInfo) =
      mconcat [ "kind" .= String "DeletingAfter"
               , "immTipHash" .= String (renderWithOrigin renderTipHash immTipWithInfo)
               , "immTipBlockNo" .= String (renderWithOrigin renderTipBlockNo immTipWithInfo)
               ]
    forMachine _dtal ImmDB.DBAlreadyClosed =
      mconcat [ "kind" .= String "DBAlreadyClosed" ]
    forMachine _dtal ImmDB.DBClosed =
      mconcat [ "kind" .= String "DBClosed" ]
    forMachine dtal (ImmDB.TraceCacheEvent cacheEv) =
      forMachine dtal cacheEv
    forMachine _dtal (ImmDB.ChunkFileDoesntFit expectPrevHash actualPrevHash) =
      mconcat [ "kind" .= String "ChunkFileDoesntFit"
               , "expectedPrevHash" .= String (renderChainHash (Text.decodeLatin1 .
                                              toRawHash (Proxy @blk)) expectPrevHash)
               , "actualPrevHash" .= String (renderChainHash (Text.decodeLatin1 .
                                              toRawHash (Proxy @blk)) actualPrevHash)
               ]
    forMachine _dtal (ImmDB.Migrating txt) =
      mconcat [ "kind" .= String "Migrating"
               , "info" .= String txt
               ]

    forHuman ImmDB.NoValidLastLocation =
          "No valid last location was found. Starting from Genesis."
    forHuman (ImmDB.ValidatedLastLocation cn t) =
            "Found a valid last location at chunk "
          <> showT cn
          <> " with tip "
          <> renderRealPoint (ImmDB.tipToRealPoint t)
          <> "."
    forHuman (ImmDB.ChunkValidationEvent e) = case e of
          ImmDB.StartedValidatingChunk chunkNo outOf ->
               "Validating chunk no. " <> showT chunkNo <> " out of " <> showT outOf
            <> ". Progress: " <> showProgressT (max (chunkNoToInt chunkNo - 1) 0) (chunkNoToInt outOf) <> "%"
          ImmDB.ValidatedChunk chunkNo outOf ->
               "Validated chunk no. " <> showT chunkNo <> " out of " <> showT outOf
            <> ". Progress: " <> showProgressT (chunkNoToInt chunkNo) (chunkNoToInt outOf) <> "%"
          ImmDB.MissingChunkFile cn      ->
            "The chunk file with number " <> showT cn <> " is missing."
          ImmDB.InvalidChunkFile cn er    ->
            "The chunk file with number " <> showT cn <> " is invalid: " <> showT er
          ImmDB.MissingPrimaryIndex cn   ->
            "The primary index of the chunk file with number " <> showT cn <> " is missing."
          ImmDB.MissingSecondaryIndex cn ->
            "The secondary index of the chunk file with number " <> showT cn <> " is missing."
          ImmDB.InvalidPrimaryIndex cn   ->
            "The primary index of the chunk file with number " <> showT cn <> " is invalid."
          ImmDB.InvalidSecondaryIndex cn ->
            "The secondary index of the chunk file with number " <> showT cn <> " is invalid."
          ImmDB.RewritePrimaryIndex cn   ->
            "Rewriting the primary index for the chunk file with number " <> showT cn <> "."
          ImmDB.RewriteSecondaryIndex cn ->
            "Rewriting the secondary index for the chunk file with number " <> showT cn <> "."
    forHuman (ImmDB.ChunkFileDoesntFit ch1 ch2 ) =
          "Chunk file doesn't fit. The hash of the block " <> showT ch2 <> " doesn't match the previous hash of the first block in the current epoch: " <> showT ch1 <> "."
    forHuman (ImmDB.Migrating t) = "Migrating: " <> t
    forHuman (ImmDB.DeletingAfter wot) = "Deleting chunk files after " <> showT wot
    forHuman ImmDB.DBAlreadyClosed {} = "Immutable DB was already closed. Double closing."
    forHuman ImmDB.DBClosed {} = "Closed Immutable DB."
    forHuman (ImmDB.TraceCacheEvent ev') = "Cache event: " <> case ev' of
          ImmDB.TraceCurrentChunkHit   cn   curr -> "Current chunk hit: " <> showT cn <> ", cache size: " <> showT curr
          ImmDB.TracePastChunkHit      cn   curr -> "Past chunk hit: " <> showT cn <> ", cache size: " <> showT curr
          ImmDB.TracePastChunkMiss     cn   curr -> "Past chunk miss: " <> showT cn <> ", cache size: " <> showT curr
          ImmDB.TracePastChunkEvict    cn   curr -> "Past chunk evict: " <> showT cn <> ", cache size: " <> showT curr
          ImmDB.TracePastChunksExpired cns  curr -> "Past chunks expired: " <> showT cns <> ", cache size: " <> showT curr


instance MetaTrace (ImmDB.TraceEvent blk) where
    namespaceFor ImmDB.NoValidLastLocation {} = Namespace [] ["NoValidLastLocation"]
    namespaceFor ImmDB.ValidatedLastLocation {} = Namespace [] ["ValidatedLastLocation"]
    namespaceFor (ImmDB.ChunkValidationEvent ev) =
      nsPrependInner "ChunkValidation" (namespaceFor ev)
    namespaceFor ImmDB.ChunkFileDoesntFit {} = Namespace [] ["ChunkFileDoesntFit"]
    namespaceFor ImmDB.Migrating {} = Namespace [] ["Migrating"]
    namespaceFor ImmDB.DeletingAfter {} = Namespace [] ["DeletingAfter"]
    namespaceFor ImmDB.DBAlreadyClosed {} = Namespace [] ["DBAlreadyClosed"]
    namespaceFor ImmDB.DBClosed {} = Namespace [] ["DBClosed"]
    namespaceFor (ImmDB.TraceCacheEvent ev) =
      nsPrependInner "CacheEvent" (namespaceFor ev)

    severityFor  (Namespace _ ["NoValidLastLocation"]) _ = Just Info
    severityFor  (Namespace _ ["ValidatedLastLocation"]) _ = Just Info
    severityFor (Namespace out ("ChunkValidation" : tl))
                    (Just (ImmDB.ChunkValidationEvent ev')) =
      severityFor (Namespace out tl) (Just ev')
    severityFor (Namespace out ("ChunkValidation" : tl)) Nothing =
      severityFor (Namespace out tl :: Namespace (ImmDB.TraceChunkValidation blk ImmDB.ChunkNo)) Nothing

    severityFor (Namespace out ("ChunkValidationEvent" : tl)) Nothing =
      severityFor (Namespace out tl :: Namespace (ImmDB.TraceChunkValidation blk chunkNo)) Nothing
    severityFor  (Namespace _ ["ChunkFileDoesntFit"]) _ = Just Warning
    severityFor  (Namespace _ ["Migrating"]) _ = Just Debug
    severityFor  (Namespace _ ["DeletingAfter"]) _ = Just Debug
    severityFor  (Namespace _ ["DBAlreadyClosed"]) _ = Just Error
    severityFor  (Namespace _ ["DBClosed"]) _ = Just Info
    severityFor (Namespace out ("CacheEvent" : tl))
                    (Just (ImmDB.TraceCacheEvent ev')) =
      severityFor (Namespace out tl) (Just ev')
    severityFor (Namespace out ("CacheEvent" : tl)) Nothing =
      severityFor (Namespace out tl :: Namespace ImmDB.TraceCacheEvent) Nothing
    severityFor _ _ = Nothing

    privacyFor (Namespace out ("ChunkValidation" : tl))
                    (Just (ImmDB.ChunkValidationEvent ev')) =
      privacyFor (Namespace out tl) (Just ev')
    privacyFor (Namespace out ("ChunkValidationEvent" : tl)) Nothing =
      privacyFor (Namespace out tl :: Namespace (ImmDB.TraceChunkValidation blk chunkNo)) Nothing
    privacyFor (Namespace out ("CacheEvent" : tl))
                    (Just (ImmDB.TraceCacheEvent ev')) =
      privacyFor (Namespace out tl) (Just ev')
    privacyFor _ _ = Just Public

    detailsFor (Namespace out ("ChunkValidation" : tl))
                    (Just (ImmDB.ChunkValidationEvent ev')) =
      detailsFor (Namespace out tl) (Just ev')
    detailsFor (Namespace out ("ChunkValidationEvent" : tl)) Nothing =
      detailsFor (Namespace out tl :: Namespace (ImmDB.TraceChunkValidation blk chunkNo)) Nothing
    detailsFor (Namespace out ("CacheEvent" : tl))
                    (Just (ImmDB.TraceCacheEvent ev')) =
      detailsFor (Namespace out tl) (Just ev')
    detailsFor _ _ = Just DNormal

    documentFor (Namespace _ ["NoValidLastLocation"]) = Just
      "No valid last location was found"
    documentFor (Namespace _ ["ValidatedLastLocation"]) = Just
      "The last location was validatet"
    documentFor (Namespace o ("ChunkValidation" : tl)) =
       documentFor (Namespace o tl :: Namespace (ImmDB.TraceChunkValidation blk chunkNo))
    documentFor (Namespace _ ["ChunkFileDoesntFit"]) = Just
      "The hash of the last block in the previous epoch doesn't match the\
       \ previous hash of the first block in the current epoch"
    documentFor (Namespace _ ["Migrating"]) = Just
      "Performing a migration of the on-disk files."
    documentFor (Namespace _ ["DeletingAfter"]) = Just
      "Delete after"
    documentFor (Namespace _ ["DBAlreadyClosed"]) = Just
      ""
    documentFor (Namespace _ ["DBClosed"]) = Just
      "Closing the immutable DB"
    documentFor (Namespace o ("CacheEvent" : tl)) =
       documentFor (Namespace o tl :: Namespace ImmDB.TraceCacheEvent)
    documentFor _ = Nothing

    allNamespaces =
      [ Namespace [] ["NoValidLastLocation"]
      , Namespace [] ["ValidatedLastLocation"]
      , Namespace [] ["ChunkFileDoesntFit"]
      , Namespace [] ["Migrating"]
      , Namespace [] ["DeletingAfter"]
      , Namespace [] ["DBAlreadyClosed"]
      , Namespace [] ["DBClosed"]
      ]
      ++ map  (nsPrependInner "ChunkValidation")
              (allNamespaces :: [Namespace (ImmDB.TraceChunkValidation blk chunkNo)])
      ++ map  (nsPrependInner "CacheEvent")
              (allNamespaces :: [Namespace ImmDB.TraceCacheEvent])

--------------------------------------------------------------------------------
-- ImmDB.TraceChunkValidation
--------------------------------------------------------------------------------

instance ConvertRawHash blk => LogFormatting (ImmDB.TraceChunkValidation blk ImmDB.ChunkNo) where
    forMachine _dtal (ImmDB.RewriteSecondaryIndex chunkNo) =
        mconcat [ "kind" .= String "TraceImmutableDBEvent.RewriteSecondaryIndex"
                 , "chunkNo" .= String (renderChunkNo chunkNo)
                 ]
    forMachine _dtal (ImmDB.RewritePrimaryIndex chunkNo) =
        mconcat [ "kind" .= String "TraceImmutableDBEvent.RewritePrimaryIndex"
                 , "chunkNo" .= String (renderChunkNo chunkNo)
                 ]
    forMachine _dtal (ImmDB.MissingPrimaryIndex chunkNo) =
        mconcat [ "kind" .= String "TraceImmutableDBEvent.MissingPrimaryIndex"
                 , "chunkNo" .= String (renderChunkNo chunkNo)
                 ]
    forMachine _dtal (ImmDB.MissingSecondaryIndex chunkNo) =
        mconcat [ "kind" .= String "TraceImmutableDBEvent.MissingSecondaryIndex"
                 , "chunkNo" .= String (renderChunkNo chunkNo)
                 ]
    forMachine _dtal (ImmDB.InvalidPrimaryIndex chunkNo) =
        mconcat [ "kind" .= String "TraceImmutableDBEvent.InvalidPrimaryIndex"
                 , "chunkNo" .= String (renderChunkNo chunkNo)
                 ]
    forMachine _dtal (ImmDB.InvalidSecondaryIndex chunkNo) =
        mconcat [ "kind" .= String "TraceImmutableDBEvent.InvalidSecondaryIndex"
                 , "chunkNo" .= String (renderChunkNo chunkNo)
                 ]
    forMachine _dtal (ImmDB.InvalidChunkFile chunkNo
                      (ImmDB.ChunkErrHashMismatch hashPrevBlock prevHashOfBlock)) =
        mconcat [ "kind" .= String "TraceImmutableDBEvent.InvalidChunkFile.ChunkErrHashMismatch"
                 , "chunkNo" .= String (renderChunkNo chunkNo)
                 , "hashPrevBlock" .= String (Text.decodeLatin1 . toRawHash (Proxy @blk) $ hashPrevBlock)
                 , "prevHashOfBlock" .= String (renderChainHash (Text.decodeLatin1 . toRawHash (Proxy @blk)) prevHashOfBlock)
                 ]
    forMachine dtal (ImmDB.InvalidChunkFile chunkNo (ImmDB.ChunkErrCorrupt pt)) =
        mconcat [ "kind" .= String "TraceImmutableDBEvent.InvalidChunkFile.ChunkErrCorrupt"
                 , "chunkNo" .= String (renderChunkNo chunkNo)
                 , "block" .= String (renderPointForDetails dtal pt)
                 ]
    forMachine _dtal (ImmDB.ValidatedChunk chunkNo _) =
        mconcat [ "kind" .= String "TraceImmutableDBEvent.ValidatedChunk"
                 , "chunkNo" .= String (renderChunkNo chunkNo)
                 ]
    forMachine _dtal (ImmDB.MissingChunkFile chunkNo) =
        mconcat [ "kind" .= String "TraceImmutableDBEvent.MissingChunkFile"
                 , "chunkNo" .= String (renderChunkNo chunkNo)
                 ]
    forMachine _dtal (ImmDB.InvalidChunkFile chunkNo (ImmDB.ChunkErrRead readIncErr)) =
        mconcat [ "kind" .= String "TraceImmutableDBEvent.InvalidChunkFile.ChunkErrRead"
                 , "chunkNo" .= String (renderChunkNo chunkNo)
                 , "error" .= String (showT readIncErr)
                 ]
    forMachine _dtal (ImmDB.StartedValidatingChunk initialChunk finalChunk) =
        mconcat [ "kind" .= String "TraceImmutableDBEvent.StartedValidatingChunk"
                 , "initialChunk" .= renderChunkNo initialChunk
                 , "finalChunk" .= renderChunkNo finalChunk
                 ]

instance MetaTrace (ImmDB.TraceChunkValidation blk chunkNo) where
    namespaceFor ImmDB.StartedValidatingChunk {} = Namespace [] ["StartedValidatingChunk"]
    namespaceFor ImmDB.ValidatedChunk {} = Namespace [] ["ValidatedChunk"]
    namespaceFor ImmDB.MissingChunkFile {} = Namespace [] ["MissingChunkFile"]
    namespaceFor ImmDB.InvalidChunkFile {} = Namespace [] ["InvalidChunkFile"]
    namespaceFor ImmDB.MissingPrimaryIndex {} = Namespace [] ["MissingPrimaryIndex"]
    namespaceFor ImmDB.MissingSecondaryIndex {} = Namespace [] ["MissingSecondaryIndex"]
    namespaceFor ImmDB.InvalidPrimaryIndex {} = Namespace [] ["InvalidPrimaryIndex"]
    namespaceFor ImmDB.InvalidSecondaryIndex {} = Namespace [] ["InvalidSecondaryIndex"]
    namespaceFor ImmDB.RewritePrimaryIndex {} = Namespace [] ["RewritePrimaryIndex"]
    namespaceFor ImmDB.RewriteSecondaryIndex {} = Namespace [] ["RewriteSecondaryIndex"]

    severityFor  (Namespace _ ["StartedValidatingChunk"]) _ = Just Info
    severityFor  (Namespace _ ["ValidatedChunk"]) _ = Just Info
    severityFor  (Namespace _ ["MissingChunkFile"]) _ = Just Warning
    severityFor  (Namespace _ ["InvalidChunkFile"]) _ = Just Warning
    severityFor  (Namespace _ ["MissingPrimaryIndex"]) _ = Just Warning
    severityFor  (Namespace _ ["MissingSecondaryIndex"]) _ = Just Warning
    severityFor  (Namespace _ ["InvalidPrimaryIndex"]) _ = Just Warning
    severityFor  (Namespace _ ["InvalidSecondaryIndex"]) _ = Just Warning
    severityFor  (Namespace _ ["RewritePrimaryIndex"]) _ = Just Warning
    severityFor  (Namespace _ ["RewriteSecondaryIndex"]) _ = Just Warning
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["StartedValidatingChunk"]) = Just
      ""
    documentFor (Namespace _ ["ValidatedChunk"]) = Just
      ""
    documentFor (Namespace _ ["MissingChunkFile"]) = Just
      ""
    documentFor (Namespace _ ["InvalidChunkFile"]) = Just
      ""
    documentFor (Namespace _ ["MissingPrimaryIndex"]) = Just
      ""
    documentFor (Namespace _ ["MissingSecondaryIndex"]) = Just
      ""
    documentFor (Namespace _ ["InvalidPrimaryIndex"]) = Just
      ""
    documentFor (Namespace _ ["InvalidSecondaryIndex"]) = Just
      ""
    documentFor (Namespace _ ["RewritePrimaryIndex"]) = Just
      ""
    documentFor (Namespace _ ["RewriteSecondaryIndex"]) = Just
      ""
    documentFor _ = Nothing

    allNamespaces =
      [ Namespace [] ["StartedValidatingChunk"]
      , Namespace [] ["ValidatedChunk"]
      , Namespace [] ["MissingChunkFile"]
      , Namespace [] ["InvalidChunkFile"]
      , Namespace [] ["MissingPrimaryIndex"]
      , Namespace [] ["MissingSecondaryIndex"]
      , Namespace [] ["InvalidPrimaryIndex"]
      , Namespace [] ["InvalidSecondaryIndex"]
      , Namespace [] ["RewritePrimaryIndex"]
      , Namespace [] ["RewriteSecondaryIndex"]
      ]

--------------------------------------------------------------------------------
-- ImmDB.TraceCacheEvent
--------------------------------------------------------------------------------

instance LogFormatting ImmDB.TraceCacheEvent where
    forMachine _dtal (ImmDB.TraceCurrentChunkHit chunkNo nbPastChunksInCache) =
          mconcat [ "kind" .= String "TraceCurrentChunkHit"
                   , "chunkNo" .= String (renderChunkNo chunkNo)
                   , "noPastChunks" .= String (showT nbPastChunksInCache)
                   ]
    forMachine _dtal (ImmDB.TracePastChunkHit chunkNo nbPastChunksInCache) =
          mconcat [ "kind" .= String "TracePastChunkHit"
                   , "chunkNo" .= String (renderChunkNo chunkNo)
                   , "noPastChunks" .= String (showT nbPastChunksInCache)
                   ]
    forMachine _dtal (ImmDB.TracePastChunkMiss chunkNo nbPastChunksInCache) =
          mconcat [ "kind" .= String "TracePastChunkMiss"
                   , "chunkNo" .= String (renderChunkNo chunkNo)
                   , "noPastChunks" .= String (showT nbPastChunksInCache)
                   ]
    forMachine _dtal (ImmDB.TracePastChunkEvict chunkNo nbPastChunksInCache) =
          mconcat [ "kind" .= String "TracePastChunkEvict"
                   , "chunkNo" .= String (renderChunkNo chunkNo)
                   , "noPastChunks" .= String (showT nbPastChunksInCache)
                   ]
    forMachine _dtal (ImmDB.TracePastChunksExpired chunkNos nbPastChunksInCache) =
          mconcat [ "kind" .= String "TracePastChunksExpired"
                   , "chunkNos" .= String (Text.pack . show $ map renderChunkNo chunkNos)
                   , "noPastChunks" .= String (showT nbPastChunksInCache)
                   ]

instance MetaTrace ImmDB.TraceCacheEvent where
    namespaceFor ImmDB.TraceCurrentChunkHit {} = Namespace [] ["CurrentChunkHit"]
    namespaceFor ImmDB.TracePastChunkHit {} = Namespace [] ["PastChunkHit"]
    namespaceFor ImmDB.TracePastChunkMiss {} = Namespace [] ["PastChunkMiss"]
    namespaceFor ImmDB.TracePastChunkEvict {} = Namespace [] ["PastChunkEvict"]
    namespaceFor ImmDB.TracePastChunksExpired {} = Namespace [] ["PastChunkExpired"]

    severityFor  (Namespace _ ["CurrentChunkHit"]) _ = Just Debug
    severityFor  (Namespace _ ["PastChunkHit"]) _ = Just Debug
    severityFor  (Namespace _ ["PastChunkMiss"]) _ = Just Debug
    severityFor  (Namespace _ ["PastChunkEvict"]) _ = Just Debug
    severityFor  (Namespace _ ["PastChunkExpired"]) _ = Just Debug
    severityFor  _ _ = Nothing

    documentFor (Namespace _ ["CurrentChunkHit"]) = Just
      "Current chunk found in the cache."
    documentFor (Namespace _ ["PastChunkHit"]) = Just
      "Past chunk found in the cache"
    documentFor (Namespace _ ["PastChunkMiss"]) = Just
      "Past chunk was not found in the cache"
    documentFor (Namespace _ ["PastChunkEvict"]) = Just
      "The least recently used past chunk was evicted because the cache\
       \ was full."
    documentFor (Namespace _ ["PastChunkExpired"]) = Just
      ""
    documentFor _ = Nothing

    allNamespaces =
      [ Namespace [] ["CurrentChunkHit"]
      , Namespace [] ["PastChunkHit"]
      , Namespace [] ["PastChunkMiss"]
      , Namespace [] ["PastChunkEvict"]
      , Namespace [] ["PastChunkExpired"]
      ]

--------------------------------------------------------------------------------
-- VolDb.TraceEvent
--------------------------------------------------------------------------------

instance StandardHash blk => LogFormatting (VolDB.TraceEvent blk) where
    forMachine _dtal VolDB.DBAlreadyClosed =
      mconcat [ "kind" .= String "DBAlreadyClosed"]
    forMachine _dtal (VolDB.BlockAlreadyHere blockId) =
      mconcat [ "kind" .= String "BlockAlreadyHere"
               , "blockId" .= String (showT blockId)
               ]
    forMachine _dtal (VolDB.Truncate pErr fsPath blockOffset) =
      mconcat [ "kind" .= String "Truncate"
               , "parserError" .= String (showT pErr)
               , "file" .= String (showT fsPath)
               , "blockOffset" .= String (showT blockOffset)
               ]
    forMachine _dtal (VolDB.InvalidFileNames fsPaths) =
      mconcat [ "kind" .= String "InvalidFileNames"
               , "files" .= String (Text.pack . show $ map show fsPaths)
              ]

instance MetaTrace (VolDB.TraceEvent blk) where
    namespaceFor VolDB.DBAlreadyClosed {} = Namespace [] ["DBAlreadyClosed"]
    namespaceFor VolDB.BlockAlreadyHere {} = Namespace [] ["BlockAlreadyHere"]
    namespaceFor VolDB.Truncate {} = Namespace [] ["Truncate"]
    namespaceFor VolDB.InvalidFileNames {} = Namespace [] ["InvalidFileNames"]

    severityFor  (Namespace _ ["DBAlreadyClosed"]) _ = Just Debug
    severityFor  (Namespace _ ["BlockAlreadyHere"]) _ = Just Debug
    severityFor  (Namespace _ ["Truncate"]) _ = Just Debug
    severityFor  (Namespace _ ["InvalidFileNames"]) _ = Just Debug
    severityFor _ _ = Nothing

    documentFor  (Namespace _ ["DBAlreadyClosed"]) = Just
      "When closing the DB it was found it is closed already."
    documentFor  (Namespace _ ["BlockAlreadyHere"]) = Just
      "A block was found to be already in the DB."
    documentFor  (Namespace _ ["Truncate"]) =  Just
      "Truncates a file up to offset because of the error."
    documentFor  (Namespace _ ["InvalidFileNames"]) = Just
      "Reports a list of invalid file paths."
    documentFor _ = Nothing

    allNamespaces =
      [ Namespace [] ["DBAlreadyClosed"]
      , Namespace [] ["BlockAlreadyHere"]
      , Namespace [] ["Truncate"]
      , Namespace [] ["InvalidFileNames"]
      ]


--------------------------------------------------------------------------------
-- ChainInformation
--------------------------------------------------------------------------------

sevLedgerEvent :: LedgerEvent blk -> SeverityS
sevLedgerEvent (LedgerUpdate _)  = Notice
sevLedgerEvent (LedgerWarning _) = Critical

showProgressT :: Int -> Int -> Text
showProgressT chunkNo outOf =
  Text.pack (showFFloat
          (Just 2)
          (100 * fromIntegral chunkNo / fromIntegral outOf :: Float)
          mempty)

data ChainInformation = ChainInformation
  { slots                :: Word64
  , blocks               :: Word64
  , density              :: Rational
    -- ^ the actual number of blocks created over the maximum expected number
    -- of blocks that could be created over the span of the last @k@ blocks.
  , epoch                :: EpochNo
    -- ^ In which epoch is the tip of the current chain
  , slotInEpoch          :: Word64
    -- ^ Relative slot number of the tip of the current chain within the
    -- epoch.
  , blocksUncoupledDelta :: Int64
    -- ^ The net change in number of blocks forged since last restart not on the
    -- current chain.
  }

chainInformation
  :: forall blk. HasHeader (Header blk)
  => ChainDB.NewTipInfo blk
  -> AF.AnchoredFragment (Header blk)
  -> Int64
  -> ChainInformation
chainInformation newTipInfo frag blocksUncoupledDelta = ChainInformation
    { slots = unSlotNo $ fromWithOrigin 0 (AF.headSlot frag)
    , blocks = unBlockNo $ fromWithOrigin (BlockNo 1) (AF.headBlockNo frag)
    , density = fragmentChainDensity frag
    , epoch = ChainDB.newTipEpoch newTipInfo
    , slotInEpoch = ChainDB.newTipSlotInEpoch newTipInfo
    , blocksUncoupledDelta = blocksUncoupledDelta
    }

fragmentChainDensity ::
  HasHeader (Header blk)
  => AF.AnchoredFragment (Header blk) -> Rational
fragmentChainDensity frag = calcDensity blockD slotD
  where
    calcDensity :: Word64 -> Word64 -> Rational
    calcDensity bl sl
      | sl > 0 = toRational bl / toRational sl
      | otherwise = 0
    slotN  = unSlotNo $ fromWithOrigin 0 (AF.headSlot frag)
    -- Slot of the tip - slot @k@ blocks back. Use 0 as the slot for genesis
    -- includes EBBs
    slotD   = slotN
            - unSlotNo (fromWithOrigin 0 (AF.lastSlot frag))
    -- Block numbers start at 1. We ignore the genesis EBB, which has block number 0.
    blockD = blockN - firstBlock
    blockN = unBlockNo $ fromWithOrigin (BlockNo 1) (AF.headBlockNo frag)
    firstBlock = case unBlockNo . blockNo <$> AF.last frag of
      -- Empty fragment, no blocks. We have that @blocks = 1 - 1 = 0@
      Left _  -> 1
      -- The oldest block is the genesis EBB with block number 0,
      -- don't let it contribute to the number of blocks
      Right 0 -> 1
      Right b -> b

--------------------------------------------------------------------------------
-- Other orophans
--------------------------------------------------------------------------------

instance LogFormatting LedgerDB.DiskSnapshot where
  forMachine DDetailed snap =
    mconcat [ "kind" .= String "snapshot"
             , "snapshot" .= String (Text.pack $ show snap) ]
  forMachine _ _snap = mconcat [ "kind" .= String "snapshot" ]


instance ( StandardHash blk
         , LogFormatting (ValidationErr (BlockProtocol blk))
         , LogFormatting (OtherHeaderEnvelopeError blk)
         )
      => LogFormatting (HeaderError blk) where
  forMachine dtal (HeaderProtocolError err) =
    mconcat
      [ "kind" .= String "HeaderProtocolError"
      , "error" .= forMachine dtal err
      ]
  forMachine dtal (HeaderEnvelopeError err) =
    mconcat
      [ "kind" .= String "HeaderEnvelopeError"
      , "error" .= forMachine dtal err
      ]

instance ( StandardHash blk
         , LogFormatting (OtherHeaderEnvelopeError blk)
         )
      => LogFormatting (HeaderEnvelopeError blk) where
  forMachine _dtal (UnexpectedBlockNo expect act) =
    mconcat
      [ "kind" .= String "UnexpectedBlockNo"
      , "expected" .= condense expect
      , "actual" .= condense act
      ]
  forMachine _dtal (UnexpectedSlotNo expect act) =
    mconcat
      [ "kind" .= String "UnexpectedSlotNo"
      , "expected" .= condense expect
      , "actual" .= condense act
      ]
  forMachine _dtal (UnexpectedPrevHash expect act) =
    mconcat
      [ "kind" .= String "UnexpectedPrevHash"
      , "expected" .= String (Text.pack $ show expect)
      , "actual" .= String (Text.pack $ show act)
      ]
  forMachine dtal (OtherHeaderEnvelopeError err) =
    forMachine dtal err


instance (   LogFormatting (LedgerError blk)
           , LogFormatting (HeaderError blk))
        => LogFormatting (ExtValidationError blk) where
    forMachine dtal (ExtValidationErrorLedger err) = forMachine dtal err
    forMachine dtal (ExtValidationErrorHeader err) = forMachine dtal err

    forHuman (ExtValidationErrorLedger err) =  forHuman err
    forHuman (ExtValidationErrorHeader err) =  forHuman err

    asMetrics (ExtValidationErrorLedger err) =  asMetrics err
    asMetrics (ExtValidationErrorHeader err) =  asMetrics err

instance (Show (PBFT.PBftVerKeyHash c))
      => LogFormatting (PBFT.PBftValidationErr c) where
  forMachine _dtal (PBFT.PBftInvalidSignature text) =
    mconcat
      [ "kind" .= String "PBftInvalidSignature"
      , "error" .= String text
      ]
  forMachine _dtal (PBFT.PBftNotGenesisDelegate vkhash _ledgerView) =
    mconcat
      [ "kind" .= String "PBftNotGenesisDelegate"
      , "vk" .= String (Text.pack $ show vkhash)
      ]
  forMachine _dtal (PBFT.PBftExceededSignThreshold vkhash numForged) =
    mconcat
      [ "kind" .= String "PBftExceededSignThreshold"
      , "vk" .= String (Text.pack $ show vkhash)
      , "numForged" .= String (Text.pack (show numForged))
      ]
  forMachine _dtal PBFT.PBftInvalidSlot =
    mconcat
      [ "kind" .= String "PBftInvalidSlot"
      ]

instance (Show (PBFT.PBftVerKeyHash c))
      => LogFormatting (PBFT.PBftCannotForge c) where
  forMachine _dtal (PBFT.PBftCannotForgeInvalidDelegation vkhash) =
    mconcat
      [ "kind" .= String "PBftCannotForgeInvalidDelegation"
      , "vk" .= String (Text.pack $ show vkhash)
      ]
  forMachine _dtal (PBFT.PBftCannotForgeThresholdExceeded numForged) =
    mconcat
      [ "kind" .= String "PBftCannotForgeThresholdExceeded"
      , "numForged" .= numForged
      ]

instance ( ConvertRawHash blk
         , StandardHash blk
         , LogFormatting (LedgerError blk)
         , LogFormatting (RealPoint blk)
         , LogFormatting (OtherHeaderEnvelopeError blk)
         , LogFormatting (ExtValidationError blk)
         , LogFormatting (ValidationErr (BlockProtocol blk))
         )
      => LogFormatting (ChainDB.InvalidBlockReason blk) where
  forMachine dtal (ChainDB.ValidationError extvalerr) =
    mconcat
      [ "kind" .= String "ValidationError"
      , "error" .= forMachine dtal extvalerr
      ]
  forMachine dtal (ChainDB.InFutureExceedsClockSkew point) =
    mconcat
      [ "kind" .= String "InFutureExceedsClockSkew"
      , "point" .= forMachine dtal point
      ]
