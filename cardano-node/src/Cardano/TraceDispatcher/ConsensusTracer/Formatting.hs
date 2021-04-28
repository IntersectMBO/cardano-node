{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.TraceDispatcher.ConsensusTracer.Formatting
  (
  ) where

import           Data.Aeson (ToJSON, Value (String), toJSON, (.=))
import qualified Data.Text as Text
import           Data.Time(DiffTime)
import           Control.Monad.Class.MonadTime(Time(..))
import           Text.Show

import           Cardano.Logging
import           Cardano.Prelude hiding (Show, show)
import           Cardano.TraceDispatcher.OrphanInstances.Byron ()
import           Cardano.TraceDispatcher.OrphanInstances.Consensus ()
import           Cardano.TraceDispatcher.OrphanInstances.Network ()
import           Cardano.TraceDispatcher.OrphanInstances.Shelley ()
import           Cardano.TraceDispatcher.Render

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime (SystemStart (..))
import           Ouroboros.Consensus.BlockchainTime.WallClock.Util
                     (TraceBlockchainTimeEvent (..))
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr,
                     GenTxId, HasTxId)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Mempool.API (MempoolSize (..),
                     TraceEventMempool (..))
import           Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
                     (TraceBlockFetchServerEvent (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Server
import           Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server
                     (TraceLocalTxSubmissionServerEvent (..))
import           Ouroboros.Consensus.Node.Run (SerialiseNodeToNodeConstraints,
                     estimateBlockSize)
import           Ouroboros.Consensus.Node.Tracers

import           Ouroboros.Network.Block
import           Ouroboros.Network.BlockFetch.ClientState (TraceLabelPeer (..))
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.BlockFetch.Decision
import           Ouroboros.Network.DeltaQ (GSV (..), PeerGSV (..))
import           Ouroboros.Network.KeepAlive (TraceKeepAliveClient (..))
import           Ouroboros.Network.TxSubmission.Inbound
import           Ouroboros.Network.TxSubmission.Outbound



instance (Show (Header blk), ConvertRawHash blk, LedgerSupportsProtocol blk)
      => LogFormatting (TraceChainSyncClientEvent blk) where
  forHuman (TraceDownloadedHeader pt) =
    "While following a candidate chain, we rolled forward by downloading a\
    \ header. " <> showT (headerPoint pt)
  forHuman (TraceRolledBack tip) =
    "While following a candidate chain, we rolled back to the given point: "
      <> showT tip
  forHuman (TraceException exc) =
    "An exception was thrown by the Chain Sync Client. "
      <> showT exc
  forHuman (TraceFoundIntersection _ _ _) =
      "We found an intersection between our chain fragment and the\
      \ candidate's chain."
  forHuman (TraceTermination res) =
      "The client has terminated. " <> showT res

  forMachine dtal (TraceDownloadedHeader pt) =
      mkObject [ "kind" .= String "DownloadedHeader"
               , "block" .= forMachine dtal (headerPoint pt) ]
  forMachine dtal (TraceRolledBack tip) =
      mkObject [ "kind" .= String "RolledBack"
               , "tip" .= forMachine dtal tip ]
  forMachine _dtal (TraceException exc) =
      mkObject [ "kind" .= String "Exception"
               , "exception" .= String (Text.pack $ show exc) ]
  forMachine _dtal (TraceFoundIntersection _ _ _) =
      mkObject [ "kind" .= String "FoundIntersection" ]
  forMachine _dtal (TraceTermination _) =
      mkObject [ "kind" .= String "Termination" ]


instance ConvertRawHash blk
      => LogFormatting (TraceChainSyncServerEvent blk) where
  forMachine dtal (TraceChainSyncServerRead tip (AddBlock hdr)) =
      mkObject [ "kind" .= String "ChainSyncServerRead.AddBlock"
               , "tip" .= String (renderTipForDetails dtal tip)
               , "addedBlock" .= String (renderPointForDetails dtal hdr)
               ]
  forMachine dtal (TraceChainSyncServerRead tip (RollBack pt)) =
      mkObject [ "kind" .= String "ChainSyncServerRead.RollBack"
               , "tip" .= String (renderTipForDetails dtal tip)
               , "rolledBackBlock" .= String (renderPointForDetails dtal pt)
               ]
  forMachine dtal (TraceChainSyncServerReadBlocked tip (AddBlock hdr)) =
      mkObject [ "kind" .= String "ChainSyncServerReadBlocked.RollForward"
               , "tip" .= String (renderTipForDetails dtal tip)
               , "addedBlock" .= String (renderPointForDetails dtal hdr)
               ]
  forMachine dtal (TraceChainSyncServerReadBlocked tip (RollBack pt)) =
      mkObject [ "kind" .= String "ChainSyncServerReadBlocked.RollBack"
               , "tip" .= String (renderTipForDetails dtal tip)
               , "rolledBackBlock" .= String (renderPointForDetails dtal pt)
               ]
  forMachine dtal (TraceChainSyncRollForward point) =
      mkObject [ "kind" .= String "ChainSyncRollForward"
               , "point" .= forMachine dtal point
               ]
  forMachine dtal (TraceChainSyncRollBackward point) =
      mkObject [ "kind" .= String "ChainSyncRollBackward"
               , "point" .= forMachine dtal point
               ]

instance (Show peer, LogFormatting a) => LogFormatting (TraceLabelPeer peer a) where
  forMachine dtal (TraceLabelPeer peerid a) =
    mkObject [ "peer" .= show peerid ] <> forMachine dtal a

instance Show peer
      => LogFormatting [TraceLabelPeer peer (FetchDecision [Point header])] where
  forMachine DBrief _ = emptyObject
  forMachine _ [] = emptyObject
  forMachine _ xs = mkObject
    [ "kind"  .= String "PeersFetch"
    , "peers" .= toJSON
      (foldl' (\acc x -> forMachine DDetailed x : acc) [] xs) ]

instance LogFormatting (FetchDecision [Point header]) where
  forMachine _dtal (Left decline) =
    mkObject [ "kind" .= String "FetchDecision declined"
             , "declined" .= String (showT decline)
             ]
  forMachine _dtal (Right results) =
    mkObject [ "kind" .= String "FetchDecision results"
             , "length" .= String (showT $ length results)
             ]

instance LogFormatting (BlockFetch.TraceFetchClientState header) where
  forMachine _dtal BlockFetch.AddedFetchRequest {} =
    mkObject [ "kind" .= String "AddedFetchRequest" ]
  forMachine _dtal BlockFetch.AcknowledgedFetchRequest {} =
    mkObject [ "kind" .= String "AcknowledgedFetchRequest" ]
  forMachine _dtal BlockFetch.CompletedBlockFetch {} =
    mkObject [ "kind" .= String "CompletedBlockFetch" ]
  forMachine _dtal BlockFetch.CompletedFetchBatch {} =
    mkObject [ "kind" .= String "CompletedFetchBatch" ]
  forMachine _dtal BlockFetch.StartedFetchBatch {} =
    mkObject [ "kind" .= String "StartedFetchBatch" ]
  forMachine _dtal BlockFetch.RejectedFetchBatch {} =
    mkObject [ "kind" .= String "RejectedFetchBatch" ]
  forMachine _dtal BlockFetch.ClientTerminating {} =
    mkObject [ "kind" .= String "ClientTerminating" ]

instance LogFormatting (TraceBlockFetchServerEvent blk) where
  forMachine _dtal _ =
    mkObject [ "kind" .= String "BlockFetchServer" ]

instance LogFormatting (TraceTxSubmissionInbound txid tx) where
  forMachine _dtal (TraceTxSubmissionCollected count) =
    mkObject
      [ "kind" .= String "TraceTxSubmissionCollected"
      , "count" .= toJSON count
      ]
  forMachine _dtal (TraceTxSubmissionProcessed processed) =
    mkObject
      [ "kind" .= String "TraceTxSubmissionProcessed"
      , "accepted" .= toJSON (ptxcAccepted processed)
      , "rejected" .= toJSON (ptxcRejected processed)
      ]
  forMachine _dtal TraceTxInboundTerminated =
    mkObject
      [ "kind" .= String "TraceTxInboundTerminated"
      ]
  forMachine _dtal (TraceTxInboundCanRequestMoreTxs count) =
    mkObject
      [ "kind" .= String "TraceTxInboundCanRequestMoreTxs"
      , "count" .= toJSON count
      ]
  forMachine _dtal (TraceTxInboundCannotRequestMoreTxs count) =
    mkObject
      [ "kind" .= String "TraceTxInboundCannotRequestMoreTxs"
      , "count" .= toJSON count
      ]

instance (Show txid, Show tx)
      => LogFormatting (TraceTxSubmissionOutbound txid tx) where
  forMachine DDetailed (TraceTxSubmissionOutboundRecvMsgRequestTxs txids) =
    mkObject
      [ "kind" .= String "TraceTxSubmissionOutboundRecvMsgRequestTxs"
      , "txIds" .= String (Text.pack $ show txids)
      ]
  forMachine _dtal (TraceTxSubmissionOutboundRecvMsgRequestTxs _txids) =
    mkObject
      [ "kind" .= String "TraceTxSubmissionOutboundRecvMsgRequestTxs"
      ]
  forMachine DDetailed (TraceTxSubmissionOutboundSendMsgReplyTxs txs) =
    mkObject
      [ "kind" .= String "TraceTxSubmissionOutboundSendMsgReplyTxs"
      , "txs" .= String (Text.pack $ show txs)
      ]
  forMachine _dtal (TraceTxSubmissionOutboundSendMsgReplyTxs _txs) =
    mkObject
      [ "kind" .= String "TraceTxSubmissionOutboundSendMsgReplyTxs"
      ]
  forMachine _dtal (TraceControlMessage _msg) =
    mkObject
      [ "kind" .= String "TraceControlMessage"
      ]

instance LogFormatting (TraceLocalTxSubmissionServerEvent blk) where
  forMachine _dtal (TraceReceivedTx _gtx) =
    mkObject [ "kind" .= String "ReceivedTx" ]

instance ( Show (ApplyTxErr blk), LogFormatting (ApplyTxErr blk), LogFormatting (GenTx blk),
           ToJSON (GenTxId blk)
         ) => LogFormatting (TraceEventMempool blk) where
  forMachine dtal (TraceMempoolAddedTx tx _mpSzBefore mpSzAfter) =
    mkObject
      [ "kind" .= String "TraceMempoolAddedTx"
      , "tx" .= forMachine dtal tx
      , "mempoolSize" .= forMachine dtal mpSzAfter
      ]
  forMachine dtal (TraceMempoolRejectedTx tx txApplyErr mpSz) =
    mkObject
      [ "kind" .= String "TraceMempoolRejectedTx"
      , "err" .= forMachine dtal txApplyErr
      , "tx" .= forMachine dtal tx
      , "mempoolSize" .= forMachine dtal mpSz
      ]
  forMachine dtal (TraceMempoolRemoveTxs txs mpSz) =
    mkObject
      [ "kind" .= String "TraceMempoolRemoveTxs"
      , "txs" .= map (forMachine dtal) txs
      , "mempoolSize" .= forMachine dtal mpSz
      ]
  forMachine dtal (TraceMempoolManuallyRemovedTxs txs0 txs1 mpSz) =
    mkObject
      [ "kind" .= String "TraceMempoolManuallyRemovedTxs"
      , "txsRemoved" .= txs0
      , "txsInvalidated" .= map (forMachine dtal) txs1
      , "mempoolSize" .= forMachine dtal mpSz
      ]

instance LogFormatting MempoolSize where
  forMachine _dtal MempoolSize{msNumTxs, msNumBytes} =
    mkObject
      [ "numTxs" .= msNumTxs
      , "bytes" .= msNumBytes
      ]

instance ( tx ~ GenTx blk
         , ConvertRawHash blk
         , HasTxId tx
         , GetHeader blk
         , HasHeader blk
         , LedgerSupportsProtocol blk
         , SerialiseNodeToNodeConstraints blk
         , Show (TxId tx)
         , Show (ForgeStateUpdateError blk)
         , Show (CannotForge blk)
         , LogFormatting (InvalidBlockReason blk)
         , LogFormatting (CannotForge blk)
         , LogFormatting (ForgeStateUpdateError blk))
      => LogFormatting (TraceForgeEvent blk) where
  forMachine _dtal (TraceStartLeadershipCheck slotNo) =
    mkObject
      [ "kind" .= String "TraceStartLeadershipCheck"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  forMachine dtal (TraceSlotIsImmutable slotNo tipPoint tipBlkNo) =
    mkObject
      [ "kind" .= String "TraceSlotIsImmutable"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "tip" .= renderPointForDetails dtal tipPoint
      , "tipBlockNo" .= toJSON (unBlockNo tipBlkNo)
      ]
  forMachine _dtal (TraceBlockFromFuture currentSlot tip) =
    mkObject
      [ "kind" .= String "TraceBlockFromFuture"
      , "current slot" .= toJSON (unSlotNo currentSlot)
      , "tip" .= toJSON (unSlotNo tip)
      ]
  forMachine dtal (TraceBlockContext currentSlot tipBlkNo tipPoint) =
    mkObject
      [ "kind" .= String "TraceBlockContext"
      , "current slot" .= toJSON (unSlotNo currentSlot)
      , "tip" .= renderPointForDetails dtal tipPoint
      , "tipBlockNo" .= toJSON (unBlockNo tipBlkNo)
      ]
  forMachine _dtal (TraceNoLedgerState slotNo _pt) =
    mkObject
      [ "kind" .= String "TraceNoLedgerState"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  forMachine _dtal (TraceLedgerState slotNo _pt) =
    mkObject
      [ "kind" .= String "TraceLedgerState"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  forMachine _dtal (TraceNoLedgerView slotNo _) =
    mkObject
      [ "kind" .= String "TraceNoLedgerView"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  forMachine _dtal (TraceLedgerView slotNo) =
    mkObject
      [ "kind" .= String "TraceLedgerView"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  forMachine dtal (TraceForgeStateUpdateError slotNo reason) =
    mkObject
      [ "kind" .= String "TraceForgeStateUpdateError"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "reason" .= forMachine dtal reason
      ]
  forMachine dtal (TraceNodeCannotForge slotNo reason) =
    mkObject
      [ "kind" .= String "TraceNodeCannotForge"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "reason" .= forMachine dtal reason
      ]
  forMachine _dtal (TraceNodeNotLeader slotNo) =
    mkObject
      [ "kind" .= String "TraceNodeNotLeader"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  forMachine _dtal (TraceNodeIsLeader slotNo) =
    mkObject
      [ "kind" .= String "TraceNodeIsLeader"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  forMachine _dtal (TraceForgedBlock slotNo _ _ _) =
    mkObject
      [ "kind" .= String "TraceForgedBlock"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  forMachine _dtal (TraceDidntAdoptBlock slotNo _) =
    mkObject
      [ "kind" .= String "TraceDidntAdoptBlock"
      , "slot" .= toJSON (unSlotNo slotNo)
      ]
  forMachine dtal (TraceForgedInvalidBlock slotNo _ reason) =
    mkObject
      [ "kind" .= String "TraceForgedInvalidBlock"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "reason" .= forMachine dtal reason
      ]
  forMachine DDetailed (TraceAdoptedBlock slotNo blk _txs) =
    mkObject
      [ "kind" .= String "TraceAdoptedBlock"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "blockHash" .= renderHeaderHashForDetails
          (Proxy @blk)
          DDetailed
          (blockHash blk)
      , "blockSize" .= toJSON (estimateBlockSize (getHeader blk))
--      , "txIds" .= toJSON (map (show . txId) txs) TODO
      ]
  forMachine dtal (TraceAdoptedBlock slotNo blk _txs) =
    mkObject
      [ "kind" .= String "TraceAdoptedBlock"
      , "slot" .= toJSON (unSlotNo slotNo)
      , "blockHash" .= renderHeaderHashForDetails
          (Proxy @blk)
          dtal
          (blockHash blk)
      , "blockSize" .= toJSON (estimateBlockSize (getHeader blk))
      ]

  forHuman (TraceStartLeadershipCheck slotNo) =
      "Checking for leadership in slot " <> showT (unSlotNo slotNo)
  forHuman (TraceSlotIsImmutable slotNo immutableTipPoint immutableTipBlkNo) =
      "Couldn't forge block because current slot is immutable: "
        <> "immutable tip: " <> renderPointAsPhrase immutableTipPoint
        <> ", immutable tip block no: " <> showT (unBlockNo immutableTipBlkNo)
        <> ", current slot: " <> showT (unSlotNo slotNo)
  forHuman (TraceBlockFromFuture currentSlot tipSlot) =
      "Couldn't forge block because current tip is in the future: "
        <> "current tip slot: " <> showT (unSlotNo tipSlot)
        <> ", current slot: " <> showT (unSlotNo currentSlot)
  forHuman (TraceBlockContext currentSlot tipBlockNo tipPoint) =
      "New block will fit onto: "
        <> "tip: " <> renderPointAsPhrase tipPoint
        <> ", tip block no: " <> showT (unBlockNo tipBlockNo)
        <> ", current slot: " <> showT (unSlotNo currentSlot)
  forHuman (TraceNoLedgerState slotNo pt) =
      "Could not obtain ledger state for point "
        <> renderPointAsPhrase pt
        <> ", current slot: "
        <> showT (unSlotNo slotNo)
  forHuman (TraceLedgerState slotNo pt) =
      "Obtained a ledger state for point "
        <> renderPointAsPhrase pt
        <> ", current slot: "
        <> showT (unSlotNo slotNo)
  forHuman (TraceNoLedgerView slotNo _) =
      "Could not obtain ledger view for slot " <> showT (unSlotNo slotNo)
  forHuman (TraceLedgerView slotNo) =
      "Obtained a ledger view for slot " <> showT (unSlotNo slotNo)
  forHuman (TraceForgeStateUpdateError slotNo reason) =
      "Updating the forge state in slot "
        <> showT (unSlotNo slotNo)
        <> " failed because: "
        <> showT reason
  forHuman (TraceNodeCannotForge slotNo reason) =
      "We are the leader in slot "
        <> showT (unSlotNo slotNo)
        <> ", but we cannot forge because: "
        <> showT reason
  forHuman (TraceNodeNotLeader slotNo) =
      "Not leading slot " <> showT (unSlotNo slotNo)
  forHuman (TraceNodeIsLeader slotNo) =
      "Leading slot " <> showT (unSlotNo slotNo)
  forHuman (TraceForgedBlock slotNo _ _ _) =
      "Forged block in slot " <> showT (unSlotNo slotNo)
  forHuman (TraceDidntAdoptBlock slotNo _) =
      "Didn't adopt forged block in slot " <> showT (unSlotNo slotNo)
  forHuman (TraceForgedInvalidBlock slotNo _ reason) =
      "Forged invalid block in slot "
        <> showT (unSlotNo slotNo)
        <> ", reason: " <> showT reason
  forHuman (TraceAdoptedBlock slotNo blk _txs) =
      "Adopted block forged in slot "
        <> showT (unSlotNo slotNo)
        <> ": " <> renderHeaderHash (Proxy @blk) (blockHash blk)
      --  <> ", TxIds: " <> showT (map txId txs) TODO Fix

instance Show t => LogFormatting (TraceBlockchainTimeEvent t) where
    forMachine _dtal (TraceStartTimeInTheFuture (SystemStart start) toWait) =
        mkObject [ "kind" .= String "TStartTimeInTheFuture"
                 , "systemStart" .= String (showT start)
                 , "toWait" .= String (showT toWait)
                 ]
    forMachine _dtal (TraceCurrentSlotUnknown time _) =
        mkObject [ "kind" .= String "CurrentSlotUnknown"
                 , "time" .= String (showT time)
                 ]
    forMachine _dtal (TraceSystemClockMovedBack prevTime newTime) =
        mkObject [ "kind" .= String "SystemClockMovedBack"
                 , "prevTime" .= String (showT prevTime)
                 , "newTime" .= String (showT newTime)
                 ]
    forHuman (TraceStartTimeInTheFuture (SystemStart start) toWait) =
      "Waiting "
      <> (Text.pack . show) toWait
      <> " until genesis start time at "
      <> (Text.pack . show) start
    forHuman (TraceCurrentSlotUnknown time _) =
      "Too far from the chain tip to determine the current slot number for the time "
       <> (Text.pack . show) time
    forHuman (TraceSystemClockMovedBack prevTime newTime) =
      "The system wall clock time moved backwards, but within our tolerance "
      <> "threshold. Previous 'current' time: "
      <> (Text.pack . show) prevTime
      <> ". New 'current' time: "
      <> (Text.pack . show) newTime

instance Show remotePeer => LogFormatting (TraceKeepAliveClient remotePeer) where
    forMachine _dtal (AddSample peer rtt pgsv) =
        mkObject
          [ "kind" .= String "AddSample"
          , "address" .= show peer
          , "rtt" .= rtt
          , "sampleTime" .= show (dTime $ sampleTime pgsv)
          , "outboundG" .= (realToFrac $ gGSV (outboundGSV pgsv) :: Double)
          , "inboundG" .= (realToFrac $ gGSV (inboundGSV pgsv) :: Double)
          ]
        where
          gGSV :: GSV -> DiffTime
          gGSV (GSV g _ _) = g

          dTime :: Time -> Double
          dTime (Time d) = realToFrac d

    forHuman tr = showT tr
