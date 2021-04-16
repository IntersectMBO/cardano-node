{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# OPTIONS_GHC -Wno-deprecations  #-}

module Cardano.TraceDispatcher.ConsensusTracer.Docu
  ( docChainSyncClientEvent
  , docChainSyncServerEvent
  , docBlockFetchDecision
  , docBlockFetchClient
  , docBlockFetchServer
  , docTxInbound
  , docTxOutbound
  ) where

import           Cardano.Logging
import           Cardano.Prelude
import           Data.Time.Clock


import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
                     (TraceBlockFetchServerEvent (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Server

import           Ouroboros.Network.Block
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.BlockFetch.Decision (FetchDecision)
import           Ouroboros.Network.BlockFetch.DeltaQ
                     (PeerFetchInFlightLimits (..))
import           Ouroboros.Network.Mux (ControlMessage)
import           Ouroboros.Network.TxSubmission.Inbound
import           Ouroboros.Network.TxSubmission.Outbound

import           Cardano.TraceDispatcher.OrphanInstances.Byron ()
import           Cardano.TraceDispatcher.OrphanInstances.Consensus ()
import           Cardano.TraceDispatcher.OrphanInstances.Network ()
import           Cardano.TraceDispatcher.OrphanInstances.Shelley ()

protoHeader :: Header blk
protoHeader = undefined

protoPoint :: Point blk
protoPoint = undefined

protoOurTipBlock :: Our (Tip blk)
protoOurTipBlock = undefined

protoTheirTipBlock :: Their (Tip blk)
protoTheirTipBlock = undefined

protoChainSyncClientException :: ChainSyncClientException
protoChainSyncClientException = undefined

protoChainSyncClientResult :: ChainSyncClientResult
protoChainSyncClientResult = undefined

protoChainUpdate :: ChainUpdate block a
protoChainUpdate = undefined

protoTip :: Tip blk
protoTip = undefined

protoRemotePeer :: remotePeer
protoRemotePeer = undefined

protoFetchDecline :: FetchDecision [Point (Header blk)]
protoFetchDecline = Left undefined

protoFetchResult :: FetchDecision [Point (Header blk)]
protoFetchResult = Right undefined

protoFetchRequest :: BlockFetch.FetchRequest header
protoFetchRequest = undefined

protoPeerFetchInFlight :: BlockFetch.PeerFetchInFlight h
protoPeerFetchInFlight = undefined

protoPeerFetchInFlightLimits :: PeerFetchInFlightLimits
protoPeerFetchInFlightLimits = PeerFetchInFlightLimits 10 10

protoPeerFetchStatus :: BlockFetch.PeerFetchStatus header
protoPeerFetchStatus = undefined

protoChainRange :: BlockFetch.ChainRange (Point header)
protoChainRange = undefined

protoNominalDiffTime :: NominalDiffTime
protoNominalDiffTime = nominalDay

protoProcessedTxCount :: ProcessedTxCount
protoProcessedTxCount = ProcessedTxCount 2 1

protoTx :: tx
protoTx = undefined

protoTxId :: txId
protoTxId = undefined

protoControlMessage :: ControlMessage
protoControlMessage = undefined

--------------------

docChainSyncClientEvent :: Documented (TraceChainSyncClientEvent blk)
docChainSyncClientEvent = Documented [
    DocMsg
      (TraceDownloadedHeader protoHeader)
      []
      "While following a candidate chain, we rolled forward by downloading a\
      \ header."
  , DocMsg
      (TraceRolledBack protoPoint)
      []
      "While following a candidate chain, we rolled back to the given point."
  , DocMsg
      (TraceFoundIntersection protoPoint protoOurTipBlock protoTheirTipBlock)
      []
      "We found an intersection between our chain fragment and the\
      \ candidate's chain."
  , DocMsg
      (TraceException protoChainSyncClientException)
      []
      "An exception was thrown by the Chain Sync Client."
  , DocMsg
      (TraceTermination protoChainSyncClientResult)
      []
      "The client has terminated."
  ]

docChainSyncServerEvent :: Documented (TraceChainSyncServerEvent blk)
docChainSyncServerEvent = Documented [
    DocMsg
      (TraceChainSyncServerRead protoTip protoChainUpdate)
      []
      "TODO"
    , DocMsg
      (TraceChainSyncServerReadBlocked protoTip protoChainUpdate)
      []
      "TODO"
    , DocMsg
      (TraceChainSyncRollForward protoPoint)
      []
      "TODO"
    , DocMsg
      (TraceChainSyncRollBackward protoPoint)
      []
      "TODO"
  ]

docBlockFetchDecision ::
  Documented ([BlockFetch.TraceLabelPeer remotePeer (FetchDecision [Point (Header blk)])])
docBlockFetchDecision = Documented [
    DocMsg
      [BlockFetch.TraceLabelPeer protoRemotePeer protoFetchDecline]
      []
      "TODO"
    , DocMsg
      [BlockFetch.TraceLabelPeer protoRemotePeer protoFetchResult]
      []
      "TODO"
  ]


docBlockFetchClient ::
  Documented (BlockFetch.TraceLabelPeer remotePeer (BlockFetch.TraceFetchClientState header))
docBlockFetchClient = Documented [
    DocMsg
      (BlockFetch.TraceLabelPeer protoRemotePeer
        (BlockFetch.AddedFetchRequest
          protoFetchRequest
          protoPeerFetchInFlight
          protoPeerFetchInFlightLimits
          protoPeerFetchStatus))
      []
      "The block fetch decision thread has added a new fetch instruction\
      \ consisting of one or more individual request ranges."
  ,
    DocMsg
      (BlockFetch.TraceLabelPeer protoRemotePeer
        (BlockFetch.AcknowledgedFetchRequest
          protoFetchRequest))
      []
      "Mark the point when the fetch client picks up the request added\
      \ by the block fetch decision thread. Note that this event can happen\
      \ fewer times than the 'AddedFetchRequest' due to fetch request merging."
  ,
    DocMsg
      (BlockFetch.TraceLabelPeer protoRemotePeer
        (BlockFetch.StartedFetchBatch
          protoChainRange
          protoPeerFetchInFlight
          protoPeerFetchInFlightLimits
          protoPeerFetchStatus))
      []
      "Mark the start of receiving a streaming batch of blocks. This will\
      \ be followed by one or more 'CompletedBlockFetch' and a final\
      \ 'CompletedFetchBatch'"
  ,
    DocMsg
      (BlockFetch.TraceLabelPeer protoRemotePeer
        (BlockFetch.CompletedBlockFetch
          protoPoint
          protoPeerFetchInFlight
          protoPeerFetchInFlightLimits
          protoPeerFetchStatus
          protoNominalDiffTime))
      []
      "Mark the completion of of receiving a single block within a\
      \ streaming batch of blocks."
  ,
    DocMsg
      (BlockFetch.TraceLabelPeer protoRemotePeer
        (BlockFetch.CompletedFetchBatch
          protoChainRange
          protoPeerFetchInFlight
          protoPeerFetchInFlightLimits
          protoPeerFetchStatus))
      []
      "Mark the successful end of receiving a streaming batch of blocks."
  ,
    DocMsg
      (BlockFetch.TraceLabelPeer protoRemotePeer
        (BlockFetch.RejectedFetchBatch
          protoChainRange
          protoPeerFetchInFlight
          protoPeerFetchInFlightLimits
          protoPeerFetchStatus))
      []
      "If the other peer rejects our request then we have this event\
      \ instead of 'StartedFetchBatch' and 'CompletedFetchBatch'."
  ,
    DocMsg
      (BlockFetch.TraceLabelPeer protoRemotePeer
        (BlockFetch.ClientTerminating 1))
      []
      "The client is terminating.  Log the number of outstanding\
      \ requests."
  ]

docBlockFetchServer ::
  Documented (TraceBlockFetchServerEvent blk)
docBlockFetchServer = Documented [
    DocMsg
      TraceBlockFetchServerSendBlock
      []
      "The server sent a block to the peer."
  ]


docTxInbound ::
  Documented (BlockFetch.TraceLabelPeer remotePeer
    (TraceTxSubmissionInbound txid tx))
docTxInbound = Documented [
    DocMsg
    (BlockFetch.TraceLabelPeer protoRemotePeer
      (TraceTxSubmissionCollected 1))
    []
    "Number of transactions just about to be inserted."
  ,
    DocMsg
    (BlockFetch.TraceLabelPeer protoRemotePeer
      (TraceTxSubmissionProcessed protoProcessedTxCount))
    []
    "Just processed transaction pass/fail breakdown."
  ,
    DocMsg
    (BlockFetch.TraceLabelPeer protoRemotePeer
      TraceTxInboundTerminated)
    []
    "Server received 'MsgDone'."
  ,
    DocMsg
    (BlockFetch.TraceLabelPeer protoRemotePeer
      (TraceTxInboundCanRequestMoreTxs 1))
    []
    "TODO"
  ,
    DocMsg
    (BlockFetch.TraceLabelPeer protoRemotePeer
      (TraceTxInboundCannotRequestMoreTxs 1))
    []
    "TODO"
  ]

docTxOutbound ::
  Documented (BlockFetch.TraceLabelPeer remotePeer
    (TraceTxSubmissionOutbound txid tx))
docTxOutbound = Documented [
    DocMsg
    (BlockFetch.TraceLabelPeer protoRemotePeer
      (TraceTxSubmissionOutboundRecvMsgRequestTxs [protoTxId]))
    []
    "The IDs of the transactions requested."
  ,
    DocMsg
    (BlockFetch.TraceLabelPeer protoRemotePeer
      (TraceTxSubmissionOutboundSendMsgReplyTxs [protoTx]))
    []
    "The transactions to be sent in the response."
  ,
    DocMsg
    (BlockFetch.TraceLabelPeer protoRemotePeer
      (TraceControlMessage protoControlMessage))
    []
    "TODO"
  ]

-- data TraceTxSubmissionOutbound txid tx
-- = TraceTxSubmissionOutboundRecvMsgRequestTxs
--     [txid]
--     -- ^ The IDs of the transactions requested.
-- | TraceTxSubmissionOutboundSendMsgReplyTxs
--     [tx]
--     -- ^ The transactions to be sent in the response.
-- | TraceControlMessage ControlMessage
-- deriving Show
