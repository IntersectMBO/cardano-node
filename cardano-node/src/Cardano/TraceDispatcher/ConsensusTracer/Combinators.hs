
module Cardano.TraceDispatcher.ConsensusTracer.Combinators
  (
    severityChainSyncClientEvent
  , namesForChainSyncClientEvent

  , severityChainSyncServerEvent
  , namesForChainSyncServerEvent

  , severityBlockFetchDecision
  , namesForBlockFetchDecision

  , severityBlockFetchClient
  , namesForBlockFetchClient

  , severityBlockFetchServer
  , namesForBlockFetchServer

  -- , severityStateInfo
  -- , namesForStateInfo

  , severityTxInbound
  , namesForTxInbound

  , severityTxOutbound
  , namesForTxOutbound

  ) where


import           Cardano.Logging
import           Cardano.Prelude

import           Ouroboros.Network.BlockFetch.ClientState (TraceLabelPeer (..))
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.BlockFetch.Decision
import           Ouroboros.Network.TxSubmission.Inbound
import           Ouroboros.Network.TxSubmission.Outbound

import           Ouroboros.Consensus.Block ({-ForgeStateInfo,-} Point)
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTx, GenTxId)
import           Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
                     (TraceBlockFetchServerEvent (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Server
-- import qualified Ouroboros.Consensus.Node.Tracers as Consensus


severityChainSyncClientEvent :: TraceChainSyncClientEvent blk -> SeverityS
severityChainSyncClientEvent TraceDownloadedHeader {}  = Info
severityChainSyncClientEvent TraceFoundIntersection {} = Info
severityChainSyncClientEvent TraceRolledBack {}        = Notice
severityChainSyncClientEvent TraceException {}         = Warning
severityChainSyncClientEvent TraceTermination {}       = Notice

namesForChainSyncClientEvent :: TraceChainSyncClientEvent blk -> [Text]
namesForChainSyncClientEvent TraceDownloadedHeader {} =
      ["DownloadedHeader"]
namesForChainSyncClientEvent TraceFoundIntersection {} =
      ["FoundIntersection"]
namesForChainSyncClientEvent TraceRolledBack {} =
      ["RolledBack"]
namesForChainSyncClientEvent TraceException {} =
      ["Exception"]
namesForChainSyncClientEvent TraceTermination {} =
      ["Termination"]

severityChainSyncServerEvent :: TraceChainSyncServerEvent blk -> SeverityS
severityChainSyncServerEvent TraceChainSyncServerRead        {} = Info
severityChainSyncServerEvent TraceChainSyncServerReadBlocked {} = Info
severityChainSyncServerEvent TraceChainSyncRollForward       {} = Info
severityChainSyncServerEvent TraceChainSyncRollBackward      {} = Info

namesForChainSyncServerEvent :: TraceChainSyncServerEvent blk -> [Text]
namesForChainSyncServerEvent TraceChainSyncServerRead        {} =
      ["ServerRead"]
namesForChainSyncServerEvent TraceChainSyncServerReadBlocked {} =
      ["ServerReadBlocked"]
namesForChainSyncServerEvent TraceChainSyncRollForward       {} =
      ["RollForward"]
namesForChainSyncServerEvent TraceChainSyncRollBackward      {} =
      ["RollBackward"]

severityBlockFetchDecision ::
     [TraceLabelPeer peer (FetchDecision [Point header])]
  -> SeverityS
severityBlockFetchDecision []  = Info
severityBlockFetchDecision l   = maximum $
  map (\(TraceLabelPeer _ a) -> fetchDecisionSeverity a) l
    where
      fetchDecisionSeverity :: FetchDecision a -> SeverityS
      fetchDecisionSeverity fd =
        case fd of
          Left FetchDeclineChainNotPlausible     -> Debug
          Left FetchDeclineChainNoIntersection   -> Notice
          Left FetchDeclineAlreadyFetched        -> Debug
          Left FetchDeclineInFlightThisPeer      -> Debug
          Left FetchDeclineInFlightOtherPeer     -> Debug
          Left FetchDeclinePeerShutdown          -> Info
          Left FetchDeclinePeerSlow              -> Info
          Left FetchDeclineReqsInFlightLimit {}  -> Info
          Left FetchDeclineBytesInFlightLimit {} -> Info
          Left FetchDeclinePeerBusy {}           -> Info
          Left FetchDeclineConcurrencyLimit {}   -> Info
          Right _                                -> Info

namesForBlockFetchDecision ::
     [TraceLabelPeer peer (FetchDecision [Point header])]
  -> [Text]
namesForBlockFetchDecision _ = []

severityBlockFetchClient ::
     TraceLabelPeer peer (BlockFetch.TraceFetchClientState header)
  -> SeverityS
severityBlockFetchClient (TraceLabelPeer _p bf) = severityBlockFetchClient' bf

severityBlockFetchClient' ::
     (BlockFetch.TraceFetchClientState header)
  -> SeverityS
severityBlockFetchClient' BlockFetch.AddedFetchRequest {}        = Info
severityBlockFetchClient' BlockFetch.AcknowledgedFetchRequest {} = Info
severityBlockFetchClient' BlockFetch.StartedFetchBatch {}        = Info
severityBlockFetchClient' BlockFetch.CompletedBlockFetch {}      = Info
severityBlockFetchClient' BlockFetch.CompletedFetchBatch {}      = Info
severityBlockFetchClient' BlockFetch.RejectedFetchBatch {}       = Info
severityBlockFetchClient' BlockFetch.ClientTerminating {}        = Notice

namesForBlockFetchClient ::
    TraceLabelPeer peer (BlockFetch.TraceFetchClientState header)
  -> [Text]
namesForBlockFetchClient (TraceLabelPeer _p bf) = namesForBlockFetchClient' bf

namesForBlockFetchClient' ::
    BlockFetch.TraceFetchClientState header
  -> [Text]
namesForBlockFetchClient' BlockFetch.AddedFetchRequest {} =
      ["AddedFetchRequest"]
namesForBlockFetchClient' BlockFetch.AcknowledgedFetchRequest {}  =
      ["AcknowledgedFetchRequest"]
namesForBlockFetchClient' BlockFetch.StartedFetchBatch {} =
      ["StartedFetchBatch"]
namesForBlockFetchClient' BlockFetch.CompletedBlockFetch  {} =
      ["CompletedBlockFetch"]
namesForBlockFetchClient' BlockFetch.CompletedFetchBatch {} =
      ["CompletedFetchBatch"]
namesForBlockFetchClient' BlockFetch.RejectedFetchBatch  {} =
      ["RejectedFetchBatch"]
namesForBlockFetchClient' BlockFetch.ClientTerminating {} =
      ["ClientTerminating"]

severityBlockFetchServer ::
     (TraceBlockFetchServerEvent blk)
  -> SeverityS
severityBlockFetchServer _ = Info

namesForBlockFetchServer ::
     (TraceBlockFetchServerEvent blk)
  -> [Text]
namesForBlockFetchServer TraceBlockFetchServerSendBlock {} = ["SendBlock"]

-- type instance ForgeStateInfo ByronBlock = ()
-- type instance ForgeStateInfo (ShelleyBlock era) = HotKey.KESInfo

-- TODO
-- severityStateInfo :: Consensus.TraceLabelCreds (ForgeStateInfo blk) -> SeverityS
-- severityStateInfo (Consensus.TraceLabelCreds _ a) = severityStateInfo' a
--
-- severityStateInfo' :: ForgeStateInfo blk -> SeverityS
-- severityStateInfo' byronBlock = Info

severityTxInbound ::
    TraceLabelPeer peer (TraceTxSubmissionInbound (GenTxId blk) (GenTx blk))
  -> SeverityS
severityTxInbound (TraceLabelPeer _p ti) = severityTxInbound' ti

severityTxInbound' ::
    TraceTxSubmissionInbound (GenTxId blk) (GenTx blk)
  -> SeverityS
severityTxInbound' _ti = Info

namesForTxInbound ::
    TraceLabelPeer peer (TraceTxSubmissionInbound (GenTxId blk) (GenTx blk))
  -> [Text]
namesForTxInbound (TraceLabelPeer _p ti) = namesForTxInbound' ti

namesForTxInbound' ::
    TraceTxSubmissionInbound (GenTxId blk) (GenTx blk)
  -> [Text]
namesForTxInbound' TraceTxSubmissionCollected {} =
    ["TxSubmissionCollected"]
namesForTxInbound' TraceTxSubmissionProcessed {} =
    ["TxSubmissionProcessed"]
namesForTxInbound' TraceTxInboundTerminated {}   =
    ["TxInboundTerminated"]
namesForTxInbound' TraceTxInboundCanRequestMoreTxs {} =
    ["TxInboundCanRequestMoreTxs"]
namesForTxInbound' TraceTxInboundCannotRequestMoreTxs {} =
    ["TxInboundCannotRequestMoreTxs"]

severityTxOutbound ::
    TraceLabelPeer peer (TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk))
  -> SeverityS
severityTxOutbound (TraceLabelPeer _p ti) = severityTxOutbound' ti

severityTxOutbound' ::
    TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk)
  -> SeverityS
severityTxOutbound' _ti = Info

namesForTxOutbound ::
    TraceLabelPeer peer (TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk))
  -> [Text]
namesForTxOutbound (TraceLabelPeer _p ti) = namesForTxOutbound' ti

namesForTxOutbound' ::
    TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk)
  -> [Text]
namesForTxOutbound' TraceTxSubmissionOutboundRecvMsgRequestTxs {} =
    ["TxSubmissionOutboundRecvMsgRequest"]
namesForTxOutbound' TraceTxSubmissionOutboundSendMsgReplyTxs {} =
    ["TxSubmissionOutboundSendMsgReply"]
namesForTxOutbound' TraceControlMessage {} =
    ["ControlMessage"]
