{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
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
  , docLocalTxSubmissionServer
  , docMempool
  , docForge
  , docForgeStateInfo
  , docBlockchainTime
  , docKeepAliveClient
  ) where

import           Cardano.Logging
import           Cardano.Prelude
import           Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import           Data.Time.Clock

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types
                     (SystemStart (..))
import           Ouroboros.Consensus.BlockchainTime.WallClock.Util
                     (TraceBlockchainTimeEvent (..))
import           Ouroboros.Consensus.Forecast (OutsideForecastRange)
import           Ouroboros.Consensus.HardFork.History (PastHorizonException)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx,
                     GenTxId)
import           Ouroboros.Consensus.Mempool.API (MempoolSize (..),
                     TraceEventMempool (..))
import           Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
                     (TraceBlockFetchServerEvent (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Server
import           Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server
                     (TraceLocalTxSubmissionServerEvent (..))
import           Ouroboros.Consensus.Node.Tracers
import qualified Ouroboros.Consensus.Shelley.Protocol.HotKey as HotKey

import           Ouroboros.Network.Block
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.BlockFetch.Decision (FetchDecision,
                     FetchDecline (..))
import           Ouroboros.Network.BlockFetch.DeltaQ
                     (PeerFetchInFlightLimits (..), PeerGSV)
import           Ouroboros.Network.KeepAlive (TraceKeepAliveClient (..))
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

protoPointH :: Point (Header blk)
protoPointH = undefined

protoOurTipBlock :: Our (Tip blk)
protoOurTipBlock = undefined

protoTheirTipBlock :: Their (Tip blk)
protoTheirTipBlock = undefined

protoChainSyncClientException :: ChainSyncClientException
protoChainSyncClientException = undefined

protoChainSyncClientResult :: ChainSyncClientResult
protoChainSyncClientResult = undefined

protoChainUpdate :: ChainUpdate blk a
protoChainUpdate = undefined

protoTip :: Tip blk
protoTip = undefined

protoRemotePeer :: remotePeer
protoRemotePeer = undefined

protoFetchDecline :: FetchDecision [Point (Header blk)]
protoFetchDecline = Left FetchDeclineChainNotPlausible

protoFetchResult :: FetchDecision [Point (Header blk)]
protoFetchResult = Right [protoPointH]

protoFetchRequest :: BlockFetch.FetchRequest (Header blk)
protoFetchRequest = undefined

protoPeerFetchInFlight :: BlockFetch.PeerFetchInFlight (Header blk)
protoPeerFetchInFlight = undefined

protoPeerFetchInFlightLimits :: PeerFetchInFlightLimits
protoPeerFetchInFlightLimits = PeerFetchInFlightLimits 10 10

protoPeerFetchStatus :: BlockFetch.PeerFetchStatus (Header blk)
protoPeerFetchStatus = undefined

protoChainRange :: BlockFetch.ChainRange (Point header)
protoChainRange = undefined

protoNominalDiffTime :: NominalDiffTime
protoNominalDiffTime = nominalDay

protoDiffTime :: DiffTime
protoDiffTime = secondsToDiffTime 100

protoProcessedTxCount :: ProcessedTxCount
protoProcessedTxCount = ProcessedTxCount 2 1

protoTx :: tx
protoTx = undefined

protoTxId :: txId
protoTxId = undefined

protoGenTxId :: GenTxId txId
protoGenTxId = undefined

protoControlMessage :: ControlMessage
protoControlMessage = undefined

protoGenTx :: GenTx blk
protoGenTx = undefined

protoMempoolSize :: MempoolSize
protoMempoolSize = undefined

protoTxt :: Text
protoTxt = "info"

protoSlotNo :: SlotNo
protoSlotNo = SlotNo 1

protoBlockNo :: BlockNo
protoBlockNo = BlockNo 2

protoBlk :: blk
protoBlk = undefined

protoOutsideForecastRange :: OutsideForecastRange
protoOutsideForecastRange = undefined

protoInvalidBlockReason :: InvalidBlockReason blk
protoInvalidBlockReason = undefined

protoKESInfo :: HotKey.KESInfo
protoKESInfo = undefined

protoUTCTime :: UTCTime
protoUTCTime = UTCTime (fromOrdinalDate 2021 100) protoDiffTime

protoSystemStart :: SystemStart
protoSystemStart = SystemStart protoUTCTime

protoPastHorizonException :: PastHorizonException
protoPastHorizonException = undefined

protoPeerGSV :: PeerGSV
protoPeerGSV = undefined

-- Not working because of non-injective type families
-- protoApplyTxErr :: ApplyTxErr blk
-- protoApplyTxErr = undefined

-- protoForgeStateUpdateError :: ForgeStateUpdateError blk
-- protoForgeStateUpdateError = undefined

-- protoCannotForge :: CannotForge blk
-- protoCannotForge = undefined

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
  Documented (BlockFetch.TraceLabelPeer remotePeer (BlockFetch.TraceFetchClientState (Header blk)))
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
          protoPointH
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

docTxOutbound :: forall remotePeer txid tx.
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

docLocalTxSubmissionServer :: Documented (TraceLocalTxSubmissionServerEvent blk)
docLocalTxSubmissionServer = Documented [
    DocMsg
    (TraceReceivedTx protoGenTx)
    []
    "A transaction was received."
  ]

docMempool :: forall blk. Documented (TraceEventMempool blk)
docMempool = Documented [
    DocMsg
      (TraceMempoolAddedTx protoGenTx protoMempoolSize protoMempoolSize)
      []
      "New, valid transaction that was added to the Mempool."
  , DocMsg
      (TraceMempoolRejectedTx protoGenTx (undefined :: ApplyTxErr blk) protoMempoolSize)
      []
      "New, invalid transaction thas was rejected and thus not added to\
      \ the Mempool."
  , DocMsg
      (TraceMempoolRemoveTxs [protoGenTx] protoMempoolSize)
      []
      "Previously valid transactions that are no longer valid because of\
      \ changes in the ledger state. These transactions have been removed\
      \ from the Mempool."
  , DocMsg
      (TraceMempoolManuallyRemovedTxs [protoGenTxId] [protoGenTx] protoMempoolSize)
      []
      "Transactions that have been manually removed from the Mempool."
  ]

docForge :: Documented (TraceLabelCreds (TraceForgeEvent blk))
docForge = Documented [
    DocMsg
      (TraceLabelCreds protoTxt
        (TraceStartLeadershipCheck protoSlotNo))
      []
      "Start of the leadership check."
  , DocMsg
      (TraceLabelCreds protoTxt
        (TraceSlotIsImmutable protoSlotNo protoPoint protoBlockNo))
      []
      "Leadership check failed: the tip of the ImmutableDB inhabits the\
      \  current slot\
      \ \
      \  This might happen in two cases.\
      \ \
      \   1. the clock moved backwards, on restart we ignored everything from the\
      \      VolatileDB since it's all in the future, and now the tip of the\
      \      ImmutableDB points to a block produced in the same slot we're trying\
      \      to produce a block in\
      \ \
      \   2. k = 0 and we already adopted a block from another leader of the same\
      \      slot.\
      \ \
      \  We record both the current slot number as well as the tip of the\
      \  ImmutableDB.\
      \ \
      \ See also <https://github.com/input-output-hk/ouroboros-network/issues/1462>"
  , DocMsg
      (TraceLabelCreds protoTxt
        (TraceBlockFromFuture protoSlotNo protoSlotNo))
      []
      "Leadership check failed: the current chain contains a block from a slot\
      \  /after/ the current slot\
      \ \
      \  This can only happen if the system is under heavy load.\
      \ \
      \  We record both the current slot number as well as the slot number of the\
      \  block at the tip of the chain.\
      \ \
      \  See also <https://github.com/input-output-hk/ouroboros-network/issues/1462>"
  , DocMsg
      (TraceLabelCreds protoTxt
        (TraceBlockContext protoSlotNo protoBlockNo protoPoint))
      []
      "We found out to which block we are going to connect the block we are about\
      \  to forge.\
      \ \
      \  We record the current slot number, the block number of the block to\
      \  connect to and its point.\
      \ \
      \  Note that block number of the block we will try to forge is one more than\
      \  the recorded block number."
  , DocMsg
      (TraceLabelCreds protoTxt
        (TraceNoLedgerState protoSlotNo protoPoint))
      []
      "Leadership check failed: we were unable to get the ledger state for the\
      \  point of the block we want to connect to\
      \ \
      \  This can happen if after choosing which block to connect to the node\
      \  switched to a different fork. We expect this to happen only rather\
      \  rarely, so this certainly merits a warning; if it happens a lot, that\
      \  merits an investigation.\
      \ \
      \  We record both the current slot number as well as the point of the block\
      \  we attempt to connect the new block to (that we requested the ledger\
      \  state for)."
  , DocMsg
      (TraceLabelCreds protoTxt
        (TraceLedgerState protoSlotNo protoPoint))
      []
      "We obtained a ledger state for the point of the block we want to\
      \  connect to\
      \ \
      \  We record both the current slot number as well as the point of the block\
      \  we attempt to connect the new block to (that we requested the ledger\
      \  state for)."
  , DocMsg
      (TraceLabelCreds protoTxt
        (TraceNoLedgerView protoSlotNo protoOutsideForecastRange))
      []
      "Leadership check failed: we were unable to get the ledger view for the\
      \  current slot number\
      \ \
      \  This will only happen if there are many missing blocks between the tip of\
      \  our chain and the current slot.\
      \ \
      \  We record also the failure returned by 'forecastFor'."
  , DocMsg
      (TraceLabelCreds protoTxt
        (TraceLedgerView protoSlotNo))
      []
      "We obtained a ledger view for the current slot number\
      \ \
      \  We record the current slot number."
  , DocMsg
      (TraceLabelCreds protoTxt
        (TraceForgeStateUpdateError protoSlotNo undefined))
      []
      "Updating the forge state failed.\
      \ \
      \  For example, the KES key could not be evolved anymore.\
      \ \
      \  We record the error returned by 'updateForgeState'."
  , DocMsg
      (TraceLabelCreds protoTxt
        (TraceNodeCannotForge protoSlotNo undefined))
      []
      "We did the leadership check and concluded that we should lead and forge\
      \  a block, but cannot.\
      \ \
      \  This should only happen rarely and should be logged with warning severity.\
      \ \
      \  Records why we cannot forge a block."
  , DocMsg
      (TraceLabelCreds protoTxt
        (TraceNodeNotLeader protoSlotNo))
      []
      "We did the leadership check and concluded we are not the leader\
      \ \
      \  We record the current slot number"
  , DocMsg
      (TraceLabelCreds protoTxt
        (TraceNodeIsLeader protoSlotNo))
      []
      "We did the leadership check and concluded we /are/ the leader\
      \ \
      \  The node will soon forge; it is about to read its transactions from the\
      \  Mempool. This will be followed by TraceForgedBlock."
  , DocMsg
      (TraceLabelCreds protoTxt
        (TraceForgedBlock protoSlotNo protoPoint protoBlk protoMempoolSize))
      []
      "We forged a block\
      \ \
      \  We record the current slot number, the point of the predecessor, the block\
      \  itself, and the total size of the mempool snapshot at the time we produced\
      \  the block (which may be significantly larger than the block, due to\
      \  maximum block size)\
      \ \
      \  This will be followed by one of three messages:\
      \ \
      \  * TraceAdoptedBlock (normally)\
      \  * TraceDidntAdoptBlock (rarely)\
      \  * TraceForgedInvalidBlock (hopefully never -- this would indicate a bug)"
  , DocMsg
      (TraceLabelCreds protoTxt
        (TraceDidntAdoptBlock protoSlotNo protoBlk))
      []
      "We did not adopt the block we produced, but the block was valid. We\
      \  must have adopted a block that another leader of the same slot produced\
      \  before we got the chance of adopting our own block. This is very rare,\
      \  this warrants a warning."
  , DocMsg
      (TraceLabelCreds protoTxt
        (TraceForgedInvalidBlock protoSlotNo protoBlk protoInvalidBlockReason))
      []
      "We forged a block that is invalid according to the ledger in the\
      \  ChainDB. This means there is an inconsistency between the mempool\
      \  validation and the ledger validation. This is a serious error!"
  , DocMsg
      (TraceLabelCreds protoTxt
        (TraceAdoptedBlock protoSlotNo protoBlk [protoGenTx]))
      []
      "We adopted the block we produced, we also trace the transactions\
      \  that were adopted."
  ]

docForgeStateInfo :: Documented (TraceLabelCreds (HotKey.KESInfo))
docForgeStateInfo = Documented [
    DocMsg
      (TraceLabelCreds protoTxt protoKESInfo)
      []
      "TODO."
    ]

docBlockchainTime :: Documented (TraceBlockchainTimeEvent t)
docBlockchainTime = Documented [
    DocMsg
      (TraceStartTimeInTheFuture protoSystemStart protoNominalDiffTime)
      []
      "The start time of the blockchain time is in the future\
      \\
      \We have to block (for 'NominalDiffTime') until that time comes."
  , DocMsg
      (TraceCurrentSlotUnknown (undefined :: t) protoPastHorizonException)
      []
      "Current slot is not yet known\
      \\
      \ This happens when the tip of our current chain is so far in the past that\
      \ we cannot translate the current wallclock to a slot number, typically\
      \ during syncing. Until the current slot number is known, we cannot\
      \ produce blocks. Seeing this message during syncing therefore is\
      \ normal and to be expected.\
      \\
      \ We record the current time (the time we tried to translate to a 'SlotNo')\
      \ as well as the 'PastHorizonException', which provides detail on the\
      \ bounds between which we /can/ do conversions. The distance between the\
      \ current time and the upper bound should rapidly decrease with consecutive\
      \ 'TraceCurrentSlotUnknown' messages during syncing."
  , DocMsg
      (TraceSystemClockMovedBack (undefined :: t) (undefined :: t))
      []
      "The system clock moved back an acceptable time span, e.g., because of\
      \ an NTP sync.\
      \\
      \ The system clock moved back such that the new current slot would be\
      \ smaller than the previous one. If this is within the configured limit, we\
      \ trace this warning but *do not change the current slot*. The current slot\
      \ never decreases, but the current slot may stay the same longer than\
      \ expected.\
      \\
      \ When the system clock moved back more than the configured limit, we shut\
      \ down with a fatal exception."
  ]

docKeepAliveClient :: Documented (TraceKeepAliveClient peer)
docKeepAliveClient = Documented [
    DocMsg
      (AddSample (undefined :: peer) protoDiffTime protoPeerGSV)
      []
      "TODO"
  ]
