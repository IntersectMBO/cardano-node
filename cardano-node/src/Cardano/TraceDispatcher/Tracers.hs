
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}


{-# OPTIONS_GHC -Wno-unused-imports  #-}


module Cardano.TraceDispatcher.Tracers
  ( mkDispatchTracers
  , docTracers
  ) where

import           Data.Aeson (ToJSON)
import qualified Data.Text.IO as T

import           Cardano.Logging
import           Cardano.Prelude hiding (trace)
import           Cardano.TraceDispatcher.ChainDB.Combinators
import           Cardano.TraceDispatcher.ChainDB.Docu
import           Cardano.TraceDispatcher.ChainDB.Formatting
import           Cardano.TraceDispatcher.Consensus.Combinators
import           Cardano.TraceDispatcher.Consensus.Docu
import           Cardano.TraceDispatcher.Consensus.Formatting
import           Cardano.TraceDispatcher.Consensus.StateInfo
import           Cardano.TraceDispatcher.Network.Combinators
import           Cardano.TraceDispatcher.Network.Docu
import           Cardano.TraceDispatcher.Network.Formatting
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

import           Ouroboros.Consensus.Block (CannotForge, ConvertRawHash,
                     ForgeStateInfo, ForgeStateUpdateError, HasHeader, Header,
                     Point)
import           Ouroboros.Consensus.Block.Forging (ForgeStateInfo)
import           Ouroboros.Consensus.BlockchainTime.WallClock.Util
                     (TraceBlockchainTimeEvent (..))
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock, Query)
import           Ouroboros.Consensus.Byron.Ledger.Config (BlockConfig)
import           Ouroboros.Consensus.Ledger.Inspect (InspectLedger,
                     LedgerUpdate, LedgerWarning)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx,
                     GenTxId, HasTxId, TxId)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import           Ouroboros.Consensus.Mempool.API (TraceEventMempool (..))
import           Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
                     (TraceBlockFetchServerEvent (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
                     (TraceChainSyncClientEvent)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Server
                     (TraceChainSyncServerEvent)
import           Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server
                     (TraceLocalTxSubmissionServerEvent (..))
import qualified Ouroboros.Consensus.Network.NodeToClient as NodeToClient
import qualified Ouroboros.Consensus.Network.NodeToNode as NodeToNode
import qualified Ouroboros.Consensus.Node.Run as Consensus
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import           Ouroboros.Consensus.Shelley.Ledger.Block
import qualified Ouroboros.Consensus.Shelley.Protocol.HotKey as HotKey
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Storage.Serialisation (SerialisedHeader)

import           Ouroboros.Network.Block (Serialised, Tip)
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.BlockFetch.Decision
import           Ouroboros.Network.Driver.Simple (TraceSendRecv)
import           Ouroboros.Network.KeepAlive (TraceKeepAliveClient (..))
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
                     (LocalStateQuery)
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type as LTS
import           Ouroboros.Network.TxSubmission.Inbound
                     (TraceTxSubmissionInbound)
import           Ouroboros.Network.TxSubmission.Outbound
                     (TraceTxSubmissionOutbound)

mkStandardTracer ::
     LogFormatting evt
  => Text
  -> (evt -> [Text])
  -> (evt -> SeverityS)
  -> Trace IO FormattedMessage
  -> IO (Trace IO evt)
mkStandardTracer name namesFor severityFor trBase = do
  tr <- humanFormatter True "Cardano" trBase
  let trNs = appendName name $ appendName "Node" tr
  pure $ withNamesAppended namesFor
          $ withSeverity severityFor trNs

docTracers :: forall blk remotePeer peer t.
  ( Show remotePeer
  , Show peer
  , Show (GenTx blk)
  , Show (ApplyTxErr blk)
  , Show (Header blk)
  , Show t
  , forall result. Show (Query blk result)
  , LogFormatting (LedgerUpdate blk)
  , LogFormatting (LedgerWarning blk)
  , LogFormatting (ApplyTxErr blk)
  , LogFormatting (CannotForge blk)
  , LogFormatting (Header blk)
  , LogFormatting (ForgeStateUpdateError blk)
  , LogFormatting (ChainDB.InvalidBlockReason blk)
  , LogFormatting (GenTx blk)
  , ToJSON (GenTxId blk)
  , HasTxId (GenTx blk)
  , LedgerSupportsProtocol blk
  , InspectLedger blk
  , Consensus.SerialiseNodeToNodeConstraints blk
  , Show (ForgeStateUpdateError blk)
  , Show (CannotForge blk)
  )
  => Proxy blk -> IO ()
docTracers _ = do
  trBase <- standardTracer Nothing
  cdbmTr <- mkStandardTracer
              "ChainDB"
              namesForChainDBTraceEvents
              severityChainDB
              trBase
  cscTr  <- mkStandardTracer
              "ChainSyncClient"
              namesForChainSyncClientEvent
              severityChainSyncClientEvent
              trBase
  csshTr <- mkStandardTracer
              "ChainSyncServerHeader"
              namesForChainSyncServerEvent
              severityChainSyncServerEvent
              trBase
  cssbTr <- mkStandardTracer
              "ChainSyncServerBlock"
              namesForChainSyncServerEvent
              severityChainSyncServerEvent
              trBase
  bfdTr  <- mkStandardTracer
              "BlockFetchDecision"
              namesForBlockFetchDecision
              severityBlockFetchDecision
              trBase
  bfcTr  <- mkStandardTracer
              "BlockFetchClient"
              namesForBlockFetchClient
              severityBlockFetchClient
              trBase
  bfsTr  <- mkStandardTracer
              "BlockFetchServer"
              namesForBlockFetchServer
              severityBlockFetchServer
              trBase
  fsiTr  <- mkStandardTracer
              "ForgeStateInfo"
              namesForStateInfo
              severityStateInfo
              trBase
  txiTr  <- mkStandardTracer
              "TxInbound"
              namesForTxInbound
              severityTxInbound
              trBase
  txoTr  <- mkStandardTracer
              "TxOutbound"
              namesForTxOutbound
              severityTxOutbound
              trBase
  ltxsTr <- mkStandardTracer
              "LocalTxSubmissionServer"
              namesForLocalTxSubmissionServer
              severityLocalTxSubmissionServer
              trBase
  mpTr   <- mkStandardTracer
              "Mempool"
              namesForMempool
              severityMempool
              trBase
  fTr    <- mkStandardTracer
              "Forge"
              namesForForge
              severityForge
              trBase
  btTr   <- mkStandardTracer
              "BlockchainTime"
              namesForBlockchainTime
              severityBlockchainTime
              trBase
  kacTr  <- mkStandardTracer
              "KeepAliveClient"
              namesForKeepAliveClient
              severityKeepAliveClient
              trBase
  tcsTr  <-  mkStandardTracer
              "TChainSync"
              namesForTChainSync
              severityTChainSync
              trBase
  ttsTr  <-  mkStandardTracer
              "TTxSubmission"
              namesForTTxSubmission
              severityTTxSubmission
              trBase
  tsqTr  <-  mkStandardTracer
              "TStateQuery"
              namesForTStateQuery
              severityTStateQuery
              trBase
  tcsnTr <-  mkStandardTracer
              "TChainSyncNode"
              namesForTChainSyncNode
              severityTChainSyncNode
              trBase
  tcssTr <-  mkStandardTracer
              "TChainSyncSerialised"
              namesForTChainSyncSerialised
              severityTChainSyncSerialised
              trBase
  cdbmTrDoc    <- documentMarkdown
              (docChainDBTraceEvent :: Documented
                (ChainDB.TraceEvent blk))
              [cdbmTr]
  cscTrDoc    <- documentMarkdown
              (docChainSyncClientEvent :: Documented
                (TraceChainSyncClientEvent blk))
              [cscTr]
  csshTrDoc    <- documentMarkdown
              (docChainSyncServerEvent :: Documented
                (TraceChainSyncServerEvent blk))
              [csshTr]
  cssbTrDoc    <- documentMarkdown
              (docChainSyncServerEvent :: Documented
                (TraceChainSyncServerEvent blk))
              [cssbTr]
  bfdTrDoc    <- documentMarkdown
              (docBlockFetchDecision :: Documented
                [BlockFetch.TraceLabelPeer remotePeer (FetchDecision [Point (Header blk)])])
              [bfdTr]
  bfcTrDoc    <- documentMarkdown
              (docBlockFetchClient :: Documented
                (BlockFetch.TraceLabelPeer peer (BlockFetch.TraceFetchClientState (Header blk))))
              [bfcTr]
  bfsTrDoc    <- documentMarkdown
              (docBlockFetchServer :: Documented
                (TraceBlockFetchServerEvent blk))
              [bfsTr]
  fsiTrDoc    <- documentMarkdown
              (docForgeStateInfo :: Documented
                (Consensus.TraceLabelCreds HotKey.KESInfo))
              [fsiTr]
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
  ltxsTrDoc    <- documentMarkdown
              (docLocalTxSubmissionServer :: Documented
                (TraceLocalTxSubmissionServerEvent blk))
              [ltxsTr]
  mpTrDoc    <- documentMarkdown
              (docMempool :: Documented
                (TraceEventMempool blk))
              [mpTr]
  fTrDoc    <- documentMarkdown
              (docForge :: Documented
                (Consensus.TraceLabelCreds (Consensus.TraceForgeEvent blk)))
              [fTr]
  btTrDoc   <- documentMarkdown
              (docBlockchainTime :: Documented
                (TraceBlockchainTimeEvent t))
              [btTr]
  kacTrDoc  <- documentMarkdown
              (docKeepAliveClient :: Documented
                (TraceKeepAliveClient remotePeer))
              [kacTr]
  tcsTrDoc  <- documentMarkdown
              (docTChainSync :: Documented
                (BlockFetch.TraceLabelPeer peer
                  (TraceSendRecv
                    (ChainSync (Serialised blk) (Point blk) (Tip blk)))))
              [tcsTr]
  ttsTrDoc  <-  documentMarkdown
              (docTTxSubmission :: Documented
                 (BlockFetch.TraceLabelPeer
                    peer
                    (TraceSendRecv
                       (LTS.LocalTxSubmission
                          (GenTx blk) (ApplyTxErr blk)))))
              [ttsTr]
  tsqTrDoc  <-  documentMarkdown
              (docTStateQuery :: Documented
                 (BlockFetch.TraceLabelPeer peer
                  (TraceSendRecv
                    (LocalStateQuery blk (Point blk) (Query blk)))))
              [tsqTr]
  tcsnTrDoc  <-  documentMarkdown
              (docTChainSync :: Documented
                (BlockFetch.TraceLabelPeer peer
                  (TraceSendRecv
                    (ChainSync (Header blk) (Point blk) (Tip blk)))))
              [tcsnTr]
  tcssTrDoc  <-  documentMarkdown
              (docTChainSync :: Documented
                (BlockFetch.TraceLabelPeer peer
                  (TraceSendRecv
                    (ChainSync (SerialisedHeader blk) (Point blk) (Tip blk)))))
              [tcssTr]

  let bl = cdbmTrDoc
          ++ cscTrDoc
          ++ csshTrDoc
          ++ cssbTrDoc
          ++ bfdTrDoc
          ++ bfcTrDoc
          ++ bfsTrDoc
          ++ fsiTrDoc
          ++ txiTrDoc
          ++ txoTrDoc
          ++ ltxsTrDoc
          ++ mpTrDoc
          ++ fTrDoc
          ++ btTrDoc
          ++ kacTrDoc
          ++ tcsTrDoc
          ++ ttsTrDoc
          ++ tsqTrDoc
          ++ tcsnTrDoc
          ++ tcssTrDoc
  res <- buildersToText bl
  T.writeFile "/home/yupanqui/IOHK/CardanoLogging.md" res
  pure ()

-- | Tracers for all system components.
--
mkDispatchTracers
  :: forall peer localPeer blk.
  ( Consensus.RunNode blk
  , LogFormatting (ChainDB.InvalidBlockReason blk)
  , HasKESMetricsData blk
  , HasKESInfo blk
  , GetKESInfo blk
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
  cdbmTr <- mkStandardTracer
              "ChainDB"
              namesForChainDBTraceEvents
              severityChainDB
              trBase
  cscTr  <- mkStandardTracer
              "ChainSyncClient"
              namesForChainSyncClientEvent
              severityChainSyncClientEvent
              trBase
  csshTr <- mkStandardTracer
              "ChainSyncServerHeader"
              namesForChainSyncServerEvent
              severityChainSyncServerEvent
              trBase
  cssbTr <- mkStandardTracer
              "ChainSyncServerBlock"
              namesForChainSyncServerEvent
              severityChainSyncServerEvent
              trBase
  bfdTr  <- mkStandardTracer
              "BlockFetchDecision"
              namesForBlockFetchDecision
              severityBlockFetchDecision
              trBase
  bfcTr  <- mkStandardTracer
              "BlockFetchClient"
              namesForBlockFetchClient
              severityBlockFetchClient
              trBase
  bfsTr  <- mkStandardTracer
              "BlockFetchServer"
              namesForBlockFetchServer
              severityBlockFetchServer
              trBase
  fsiTr  <- mkStandardTracer
              "ForgeStateInfo"
              namesForStateInfo
              severityStateInfo
              trBase
  txiTr  <- mkStandardTracer
              "TxInbound"
              namesForTxInbound
              severityTxInbound
              trBase
  txoTr  <- mkStandardTracer
              "TxOutbound"
              namesForTxOutbound
              severityTxOutbound
              trBase
  ltxsTr <- mkStandardTracer
              "LocalTxSubmissionServer"
              namesForLocalTxSubmissionServer
              severityLocalTxSubmissionServer
              trBase
  mpTr   <- mkStandardTracer
              "Mempool"
              namesForMempool
              severityMempool
              trBase
  fTr    <- mkStandardTracer
              "Forge"
              namesForForge
              severityForge
              trBase
  btTr   <- mkStandardTracer
              "BlockchainTime"
              namesForBlockchainTime
              severityBlockchainTime
              trBase
  kacTr  <- mkStandardTracer
              "KeepAliveClient"
              namesForKeepAliveClient
              severityKeepAliveClient
              trBase
  tcsTr  <-  mkStandardTracer
              "TChainSyncClient"
              namesForTChainSync
              severityTChainSync
              trBase
  ttsTr  <-  mkStandardTracer
              "TTxSubmissionClient"
              namesForTTxSubmission
              severityTTxSubmission
              trBase
  tsqTr  <-  mkStandardTracer
              "TStateQueryClient"
              namesForTStateQuery
              severityTStateQuery
              trBase
  tcsnTr <-  mkStandardTracer
              "TChainSyncNode"
              namesForTChainSyncNode
              severityTChainSyncNode
              trBase
  tcssTr <-  mkStandardTracer
              "TChainSyncSerialised"
              namesForTChainSyncSerialised
              severityTChainSyncSerialised
              trBase
  configureTracers emptyTraceConfig docChainDBTraceEvent [cdbmTr]
  configureTracers emptyTraceConfig docChainSyncClientEvent [cscTr]
  configureTracers emptyTraceConfig docChainSyncServerEvent [csshTr]
  configureTracers emptyTraceConfig docChainSyncServerEvent [cssbTr]
  configureTracers emptyTraceConfig docBlockFetchDecision   [bfdTr]
  configureTracers emptyTraceConfig docBlockFetchClient     [bfcTr]
  configureTracers emptyTraceConfig docBlockFetchServer     [bfsTr]
  configureTracers emptyTraceConfig docTxInbound            [txiTr]
  configureTracers emptyTraceConfig docLocalTxSubmissionServer [ltxsTr]
  configureTracers emptyTraceConfig docMempool              [mpTr]
  configureTracers emptyTraceConfig docForge                [fTr]
  configureTracers emptyTraceConfig docBlockchainTime       [btTr]
  configureTracers emptyTraceConfig docKeepAliveClient      [kacTr]
  configureTracers emptyTraceConfig docTChainSync           [tcsTr]
  configureTracers emptyTraceConfig docTTxSubmission        [ttsTr]
  configureTracers emptyTraceConfig docTStateQuery          [tsqTr]
  configureTracers emptyTraceConfig docTChainSync           [tcsnTr]
  configureTracers emptyTraceConfig docTChainSync           [tcssTr]

  pure Tracers
    { chainDBTracer = Tracer (traceWith cdbmTr)
    , consensusTracers = Consensus.Tracers
      { Consensus.chainSyncClientTracer = Tracer (traceWith cscTr)
      , Consensus.chainSyncServerHeaderTracer = Tracer (traceWith csshTr)
      , Consensus.chainSyncServerBlockTracer = Tracer (traceWith cssbTr)
      , Consensus.blockFetchDecisionTracer = Tracer (traceWith bfdTr)
      , Consensus.blockFetchClientTracer = Tracer (traceWith bfcTr)
      , Consensus.blockFetchServerTracer = Tracer (traceWith bfsTr)
      , Consensus.forgeStateInfoTracer =
          Tracer (traceWith (traceAsKESInfo (Proxy @blk) fsiTr))
      , Consensus.txInboundTracer = Tracer (traceWith txiTr)
      , Consensus.txOutboundTracer = Tracer (traceWith txoTr)
      , Consensus.localTxSubmissionServerTracer = Tracer (traceWith ltxsTr)
      , Consensus.mempoolTracer = Tracer (traceWith mpTr)
      , Consensus.forgeTracer = Tracer (traceWith fTr)
      , Consensus.blockchainTimeTracer = Tracer (traceWith btTr)
      , Consensus.keepAliveClientTracer = Tracer (traceWith kacTr)
      }
    , nodeToClientTracers = NodeToClient.Tracers
      { NodeToClient.tChainSyncTracer = Tracer (traceWith tcsTr)
      , NodeToClient.tTxSubmissionTracer = Tracer (traceWith ttsTr)
      , NodeToClient.tStateQueryTracer = Tracer (traceWith tsqTr)
      }
    , nodeToNodeTracers = NodeToNode.Tracers
      { NodeToNode.tChainSyncTracer = Tracer (traceWith tcsnTr)
      , NodeToNode.tChainSyncSerialisedTracer = Tracer (traceWith tcssTr)
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
