{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE PackageImports       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wno-unused-imports  #-}


module Cardano.TraceDispatcher.Tracers
  ( mkDispatchTracers
  , docTracers
  ) where

import           Cardano.Prelude hiding (trace)
import qualified Data.Text.IO as T

import           Cardano.Logging
import           Cardano.TraceDispatcher.ChainDBTracer.Combinators
import           Cardano.TraceDispatcher.ChainDBTracer.Docu
import           Cardano.TraceDispatcher.ChainDBTracer.Formatting
import           Cardano.TraceDispatcher.ConsensusTracer.Combinators
import           Cardano.TraceDispatcher.ConsensusTracer.Docu
import           Cardano.TraceDispatcher.ConsensusTracer.Formatting ()
import           Cardano.TraceDispatcher.OrphanInstances.Consensus ()

import           Cardano.Node.Configuration.Logging (EKGDirect)

import qualified Cardano.BM.Data.Trace as Old
import           Cardano.Tracing.Config (TraceOptions (..))
import           Cardano.Tracing.Constraints (TraceConstraints)
import           Cardano.Tracing.ConvertTxId
import           Cardano.Tracing.Kernel (NodeKernelData)
import           Cardano.Tracing.Metrics (HasKESInfo, HasKESMetricsData)
import           Cardano.Tracing.Tracers
import           "contra-tracer" Control.Tracer (Tracer (..), nullTracer)

import           Ouroboros.Consensus.Block (ConvertRawHash, ForgeStateInfo,
                     HasHeader, Header, Point)
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import           Ouroboros.Consensus.Byron.Ledger.Config (BlockConfig)
import           Ouroboros.Consensus.Ledger.Inspect (InspectLedger,
                     LedgerUpdate, LedgerWarning)
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTx, GenTxId, TxId)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import           Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
                     (TraceBlockFetchServerEvent (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
                     (TraceChainSyncClientEvent)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Server
                     (TraceChainSyncServerEvent)
import qualified Ouroboros.Consensus.Network.NodeToClient as NodeToClient
import qualified Ouroboros.Consensus.Network.NodeToNode as NodeToNode
import qualified Ouroboros.Consensus.Node.Run as Consensus (RunNode)
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB

import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.BlockFetch.Decision
import           Ouroboros.Network.TxSubmission.Inbound
                     (TraceTxSubmissionInbound)
import           Ouroboros.Network.TxSubmission.Outbound
                     (TraceTxSubmissionOutbound)

chainDBMachineTracer ::
  ( LogFormatting (LedgerUpdate blk)
  , LogFormatting (LedgerWarning blk)
  , LogFormatting (Header blk)
  , ConvertRawHash blk
  , LedgerSupportsProtocol blk
  , InspectLedger blk
  )
  => Trace IO FormattedMessage
  -> IO (Trace IO (ChainDB.TraceEvent blk))
chainDBMachineTracer trBase = do
    tr <- humanFormatter True "Cardano" trBase
    let cdbmTrNs = appendName "ChainDB" $ appendName "Node" tr
    pure $ withNamesAppended namesForChainDBTraceEvents
            $ withSeverity severityChainDB cdbmTrNs

chainSyncClientTracer ::
  ( ConvertRawHash blk
  , LedgerSupportsProtocol blk
  , Show (Header blk)
  )
  => Trace IO FormattedMessage
  -> IO (Trace IO (TraceChainSyncClientEvent blk))
chainSyncClientTracer trBase = do
    tr <- humanFormatter True "Cardano" trBase
    let cdbmTrNs = appendName "ChainSyncClient" $ appendName "Node" tr
    pure $ withNamesAppended namesForChainSyncClientEvent
            $ withSeverity severityChainSyncClientEvent cdbmTrNs

chainSyncServerHeaderTracer ::
  ( ConvertRawHash blk
  )
  => Trace IO FormattedMessage
  -> IO (Trace IO (TraceChainSyncServerEvent blk))
chainSyncServerHeaderTracer trBase = do
    tr <- humanFormatter True "Cardano" trBase
    let cdbmTrNs = appendName "ChainSyncServerHeader" $ appendName "Node" tr
    pure $ withNamesAppended namesForChainSyncServerEvent
            $ withSeverity severityChainSyncServerEvent cdbmTrNs

chainSyncServerBlockTracer ::
  ( ConvertRawHash blk
  )
  => Trace IO FormattedMessage
  -> IO (Trace IO (TraceChainSyncServerEvent blk))
chainSyncServerBlockTracer trBase = do
    tr <- humanFormatter True "Cardano" trBase
    let cdbmTrNs = appendName "ChainSyncServerBlock" $ appendName "Node" tr
    pure $ withNamesAppended namesForChainSyncServerEvent
            $ withSeverity severityChainSyncServerEvent cdbmTrNs

blockFetchDecisionTracer :: forall blk remotePeer .
  ( Show remotePeer
  )
  => Trace IO FormattedMessage
  -> IO (Trace IO [BlockFetch.TraceLabelPeer remotePeer
                  (FetchDecision [Point (Header blk)])])
blockFetchDecisionTracer trBase = do
    tr <- humanFormatter True "Cardano" trBase
    let cdbmTrNs = appendName "BlockFetchDecision" $ appendName "Node" tr
    pure $ withNamesAppended namesForBlockFetchDecision
            $ withSeverity severityBlockFetchDecision cdbmTrNs


blockFetchClientTracer :: forall blk remotePeer .
  ( Show remotePeer
  , LogFormatting (BlockFetch.TraceFetchClientState (Header blk))
  )
  => Trace IO FormattedMessage
  -> IO (Trace IO (BlockFetch.TraceLabelPeer remotePeer
                  (BlockFetch.TraceFetchClientState (Header blk))))
blockFetchClientTracer trBase = do
    tr <- humanFormatter True "Cardano" trBase
    let cdbmTrNs = appendName "BlockFetchClient" $ appendName "Node" tr
    pure $ withNamesAppended namesForBlockFetchClient
            $ withSeverity severityBlockFetchClient cdbmTrNs

blockFetchServerTracer :: forall blk .
     Trace IO FormattedMessage
  -> IO (Trace IO (TraceBlockFetchServerEvent blk))
blockFetchServerTracer trBase = do
    tr <- humanFormatter True "Cardano" trBase
    let cdbmTrNs = appendName "BlockFetchServer" $ appendName "Node" tr
    pure $ withNamesAppended namesForBlockFetchServer
            $ withSeverity severityBlockFetchServer cdbmTrNs

-- TODO
-- forgeStateInfoTracer :: forall blk .
--      Trace IO FormattedMessage
--   -> IO (Trace IO (Consensus.TraceLabelCreds (ForgeStateInfo blk)))
-- forgeStateInfoTracer trBase = do
--     tr <- humanFormatter True "Cardano" trBase
--     let cdbmTrNs = appendName "Forge" $ appendName "Node" tr
--     pure $ withNamesAppended namesForStateInfo
--             $ withSeverity severityStateInfo cdbmTrNs

txInboundTracer :: forall blk remotePeer.
  (Show remotePeer)
  => Trace IO FormattedMessage
  -> IO (Trace IO (BlockFetch.TraceLabelPeer remotePeer
                    (TraceTxSubmissionInbound  (GenTxId blk) (GenTx blk))))
txInboundTracer trBase = do
    tr <- humanFormatter True "Cardano" trBase
    let cdbmTrNs = appendName "TxInbound" $ appendName "Node" tr
    pure $ withNamesAppended namesForTxInbound
            $ withSeverity severityTxInbound cdbmTrNs

txOutboundTracer :: forall blk remotePeer.
  ( Show remotePeer,
    Show (Ouroboros.Consensus.Ledger.SupportsMempool.TxId (GenTx blk))
  , Show (GenTx blk))
  => Trace IO FormattedMessage
  -> IO (Trace IO (BlockFetch.TraceLabelPeer remotePeer
                    (TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk))))
txOutboundTracer trBase = do
    tr <- humanFormatter True "Cardano" trBase
    let cdbmTrNs = appendName "TxOutbound" $ appendName "Node" tr
    pure $ withNamesAppended namesForTxOutbound
            $ withSeverity severityTxOutbound cdbmTrNs

docTracers :: forall blk remotePeer.
  ( Show remotePeer
  , Show (TxId (GenTx blk))
  , Show (GenTx blk)
  )
  => IO ()
docTracers = do
  trBase <- standardTracer Nothing

  cdbmTr <- chainDBMachineTracer trBase
  cscTr  <- chainSyncClientTracer trBase
  csshTr <- chainSyncServerHeaderTracer trBase
  cssbTr <- chainSyncServerBlockTracer trBase
  bfdTr  <- blockFetchDecisionTracer trBase
  bfsTr  <- blockFetchServerTracer trBase
  txiTr  <- txInboundTracer trBase
  txoTr  <- txOutboundTracer trBase

  cdbmTrDoc    <- documentMarkdown
              (docChainDBTraceEvent :: Documented
                (ChainDB.TraceEvent ByronBlock))
              [cdbmTr]
  cscTrDoc    <- documentMarkdown
              (docChainSyncClientEvent :: Documented
                (TraceChainSyncClientEvent ByronBlock))
              [cscTr]
  csshTrDoc    <- documentMarkdown
              (docChainSyncServerEvent :: Documented
                (TraceChainSyncServerEvent ByronBlock))
              [csshTr]
  cssbTrDoc    <- documentMarkdown
              (docChainSyncServerEvent :: Documented
                (TraceChainSyncServerEvent ByronBlock))
              [cssbTr]
  bfdTrDoc    <- documentMarkdown
              (docBlockFetchDecision :: Documented
                [BlockFetch.TraceLabelPeer remotePeer (FetchDecision [Point (Header ByronBlock)])])
              [bfdTr]
  bfsTrDoc    <- documentMarkdown
              (docBlockFetchServer :: Documented
                (TraceBlockFetchServerEvent ByronBlock))
              [bfsTr]
  txiTrDoc    <- documentMarkdown
              (docTxInbound :: Documented
                (BlockFetch.TraceLabelPeer remotePeer
                  (TraceTxSubmissionInbound (GenTxId blk) (GenTx blk))))
              [txiTr]
  txoTrDoc    <- documentMarkdown
              (docTxOutbound :: Documented
                (BlockFetch.TraceLabelPeer remotePeer
                  (TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk))))
              [txoTr]
  let bl = cdbmTrDoc
        ++ cscTrDoc
        ++ csshTrDoc
        ++ cssbTrDoc
        ++ bfdTrDoc
        ++ bfsTrDoc
        ++ txiTrDoc
        ++ txoTrDoc
  T.writeFile "/home/yupanqui/IOHK/CardanoLogging.md" (buildersToText bl)

-- | Tracers for all system components.
--
mkDispatchTracers
  :: forall peer localPeer blk.
  ( Consensus.RunNode blk
  , LogFormatting (LedgerWarning blk)
  , HasKESMetricsData blk
  , HasKESInfo blk
  , TraceConstraints blk
  , Show peer, Eq peer
  , Show localPeer
  )
  => BlockConfig blk
  -> TraceOptions
  -> Old.Trace IO Text
  -> NodeKernelData blk
  -> Maybe EKGDirect
  -> Trace IO FormattedMessage
  -> IO (Tracers peer localPeer blk)
mkDispatchTracers _blockConfig (TraceDispatcher _trSel) _tr _nodeKern _ekgDirect trBase = do

  cdbmTr <- chainDBMachineTracer trBase
  cscTr  <- chainSyncClientTracer trBase
  csshTr <- chainSyncServerHeaderTracer trBase
  cssbTr <- chainSyncServerBlockTracer trBase
  bfdTr  <- blockFetchDecisionTracer trBase
  bfcTr  <- blockFetchClientTracer trBase
  txiTr  <- txInboundTracer trBase
  bfsTr :: (Trace IO (TraceBlockFetchServerEvent blk)) <- blockFetchServerTracer trBase
  txoTr  <- txOutboundTracer trBase


  configureTracers emptyTraceConfig docChainDBTraceEvent [cdbmTr]
  configureTracers emptyTraceConfig docChainSyncClientEvent [cscTr]
  configureTracers emptyTraceConfig docChainSyncServerEvent [csshTr]
  configureTracers emptyTraceConfig docChainSyncServerEvent [cssbTr]
  configureTracers emptyTraceConfig docBlockFetchDecision   [bfdTr]
  configureTracers emptyTraceConfig docBlockFetchClient     [bfcTr]
  configureTracers emptyTraceConfig docBlockFetchServer     [bfsTr]
  configureTracers emptyTraceConfig docTxInbound            [txiTr]


  pure Tracers
    { chainDBTracer = Tracer (traceWith cdbmTr)
    , consensusTracers = Consensus.Tracers
      { Consensus.chainSyncClientTracer = Tracer (traceWith cscTr)
      , Consensus.chainSyncServerHeaderTracer = Tracer (traceWith csshTr)
      , Consensus.chainSyncServerBlockTracer = Tracer (traceWith cssbTr)
      , Consensus.blockFetchDecisionTracer = Tracer (traceWith bfdTr)
      , Consensus.blockFetchClientTracer = Tracer (traceWith bfcTr)
      , Consensus.blockFetchServerTracer = Tracer (traceWith bfsTr)
      , Consensus.forgeStateInfoTracer = nullTracer
      , Consensus.txInboundTracer = Tracer (traceWith txiTr)
      , Consensus.txOutboundTracer = Tracer (traceWith txoTr)
      , Consensus.localTxSubmissionServerTracer = nullTracer
      , Consensus.mempoolTracer = nullTracer
      , Consensus.forgeTracer = nullTracer
      , Consensus.blockchainTimeTracer = nullTracer
      , Consensus.keepAliveClientTracer = nullTracer
      }
    , nodeToClientTracers = NodeToClient.Tracers
      { NodeToClient.tChainSyncTracer = nullTracer
      , NodeToClient.tTxSubmissionTracer = nullTracer
      , NodeToClient.tStateQueryTracer = nullTracer
      }
    , nodeToNodeTracers = NodeToNode.Tracers
      { NodeToNode.tChainSyncTracer = nullTracer
      , NodeToNode.tChainSyncSerialisedTracer = nullTracer
      , NodeToNode.tBlockFetchTracer = nullTracer
      , NodeToNode.tBlockFetchSerialisedTracer = nullTracer
      , NodeToNode.tTxSubmissionTracer = nullTracer
      , NodeToNode.tTxSubmission2Tracer = nullTracer
      }
    , ipSubscriptionTracer = nullTracer
    , dnsSubscriptionTracer= nullTracer
    , dnsResolverTracer = nullTracer
    , errorPolicyTracer = nullTracer
    , localErrorPolicyTracer = nullTracer
    , acceptPolicyTracer = nullTracer
    , muxTracer = nullTracer
    , muxLocalTracer = nullTracer
    , handshakeTracer = nullTracer
    , localHandshakeTracer = nullTracer
    , diffusionInitializationTracer = nullTracer
  }

mkDispatchTracers blockConfig tOpts tr nodeKern ekgDirect _ =
  mkTracers blockConfig tOpts tr nodeKern ekgDirect
