
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards       #-}
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
import           Cardano.TraceDispatcher.Common.ConvertTxId
import           Cardano.TraceDispatcher.Common.Formatting
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
import           Cardano.Tracing.Kernel (NodeKernelData)
import           Cardano.Tracing.Metrics (HasKESInfo, HasKESMetricsData)
import           Cardano.Tracing.Tracers (Tracers (..), mkTracers)
import qualified "contra-tracer" Control.Tracer as T

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
                     GenTxId, HasTxId, HasTxs, TxId)
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
import           Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch)
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
                     (LocalStateQuery)
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type as LTS
import           Ouroboros.Network.Protocol.TxSubmission.Type (TxSubmission)
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

-- | Tracers for all system components.
--
mkDispatchTracers
  :: forall peer localPeer blk.
  ( Consensus.RunNode blk
  , LogFormatting (ChainDB.InvalidBlockReason blk)
  , GetKESInfo blk
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
  tracers <- mkDispatchTracers' trBase
  configTracers emptyTraceConfig tracers
  pure tracers
mkDispatchTracers blockConfig tOpts tr nodeKern ekgDirect _ =
  mkTracers blockConfig tOpts tr nodeKern ekgDirect

mkDispatchTracers'
  :: forall peer localPeer blk.
  ( Consensus.RunNode blk
  , LogFormatting (ChainDB.InvalidBlockReason blk)
  , GetKESInfo blk
  , TraceConstraints blk
  , Show peer
  , Show localPeer
  )
  => Trace IO FormattedMessage
  -> IO (Tracers peer localPeer blk)
mkDispatchTracers' trBase = do
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
                "ChainSyncClient"
                namesForTChainSync
                severityTChainSync
                trBase
    ttsTr  <-  mkStandardTracer
                "TxSubmissionClient"
                namesForTTxSubmission
                severityTTxSubmission
                trBase
    tsqTr  <-  mkStandardTracer
                "StateQueryClient"
                namesForTStateQuery
                severityTStateQuery
                trBase
    tcsnTr <-  mkStandardTracer
                "ChainSyncNode"
                namesForTChainSyncNode
                severityTChainSyncNode
                trBase
    tcssTr <-  mkStandardTracer
                "ChainSyncSerialised"
                namesForTChainSyncSerialised
                severityTChainSyncSerialised
                trBase
    tbfTr  <-  mkStandardTracer
                "BlockFetch"
                namesForTBlockFetch
                severityTBlockFetch
                trBase
    tbfsTr <-  mkStandardTracer
                "BlockFetchSerialised"
                namesForTBlockFetchSerialised
                severityTBlockFetchSerialised
                trBase
    tsnTr  <-  mkStandardTracer
                "TxSubmission"
                namesForTxSubmissionNode
                severityTxSubmissionNode
                trBase
    ts2nTr  <-  mkStandardTracer
                "TxSubmission2"
                namesForTxSubmission2Node
                severityTxSubmission2Node
                trBase
    ipsTr   <-  mkStandardTracer
                "IpSubscription"
                namesForIpSubscription
                severityIpSubscription
                trBase
    pure Tracers
      { chainDBTracer = T.Tracer (traceWith cdbmTr)
      , consensusTracers = Consensus.Tracers
        { Consensus.chainSyncClientTracer = T.Tracer (traceWith cscTr)
        , Consensus.chainSyncServerHeaderTracer = T.Tracer (traceWith csshTr)
        , Consensus.chainSyncServerBlockTracer = T.Tracer (traceWith cssbTr)
        , Consensus.blockFetchDecisionTracer = T.Tracer (traceWith bfdTr)
        , Consensus.blockFetchClientTracer = T.Tracer (traceWith bfcTr)
        , Consensus.blockFetchServerTracer = T.Tracer (traceWith bfsTr)
        , Consensus.forgeStateInfoTracer =
            T.Tracer (traceWith (traceAsKESInfo (Proxy @blk) fsiTr))
        , Consensus.txInboundTracer = T.Tracer (traceWith txiTr)
        , Consensus.txOutboundTracer = T.Tracer (traceWith txoTr)
        , Consensus.localTxSubmissionServerTracer = T.Tracer (traceWith ltxsTr)
        , Consensus.mempoolTracer = T.Tracer (traceWith mpTr)
        , Consensus.forgeTracer = T.Tracer (traceWith fTr)
        , Consensus.blockchainTimeTracer = T.Tracer (traceWith btTr)
        , Consensus.keepAliveClientTracer = T.Tracer (traceWith kacTr)
        }
      , nodeToClientTracers = NodeToClient.Tracers
        { NodeToClient.tChainSyncTracer = T.Tracer (traceWith tcsTr)
        , NodeToClient.tTxSubmissionTracer = T.Tracer (traceWith ttsTr)
        , NodeToClient.tStateQueryTracer = T.Tracer (traceWith tsqTr)
        }
      , nodeToNodeTracers = NodeToNode.Tracers
        { NodeToNode.tChainSyncTracer = T.Tracer (traceWith tcsnTr)
        , NodeToNode.tChainSyncSerialisedTracer = T.Tracer (traceWith tcssTr)
        , NodeToNode.tBlockFetchTracer = T.Tracer (traceWith tbfTr)
        , NodeToNode.tBlockFetchSerialisedTracer = T.Tracer (traceWith tbfsTr)
        , NodeToNode.tTxSubmissionTracer = T.Tracer (traceWith tsnTr)
        , NodeToNode.tTxSubmission2Tracer = T.Tracer (traceWith ts2nTr)
        }
      , ipSubscriptionTracer = T.Tracer (traceWith ipsTr)
      , dnsSubscriptionTracer= T.nullTracer
      , dnsResolverTracer = T.nullTracer
      , errorPolicyTracer = T.nullTracer
      , localErrorPolicyTracer = T.nullTracer
      , acceptPolicyTracer = T.nullTracer
      , muxTracer = T.nullTracer
      , muxLocalTracer = T.nullTracer
      , handshakeTracer = T.nullTracer
      , localHandshakeTracer = T.nullTracer
      , diffusionInitializationTracer = T.nullTracer
    }

traceTrans ::
  Applicative m
  => T.Tracer m a
  -> Trace m a
traceTrans tr = Trace $
   let tr' = contramap (\(_, _, v) -> v) tr
   in  arrow (emit (T.traceWith tr'))

configTracers :: forall blk peer localPeer.
     TraceConfig
  -> Tracers peer localPeer blk
  -> IO ()
configTracers config Tracers {..} = do
    configureTracers config docChainDBTraceEvent
      [traceTrans chainDBTracer]
    configureTracers config docChainSyncClientEvent
      [traceTrans (Consensus.chainSyncClientTracer consensusTracers)]
    configureTracers config docChainSyncServerEvent
      [traceTrans (Consensus.chainSyncServerHeaderTracer consensusTracers)]
    configureTracers config docChainSyncServerEvent
      [traceTrans (Consensus.chainSyncServerBlockTracer consensusTracers)]
    configureTracers config docBlockFetchDecision
      [traceTrans (Consensus.blockFetchDecisionTracer consensusTracers)]
    configureTracers config docBlockFetchClient
      [traceTrans (Consensus.blockFetchClientTracer consensusTracers)]
    configureTracers config docBlockFetchServer
      [traceTrans (Consensus.blockFetchServerTracer consensusTracers)]
    -- TODO
    -- configureTracers config docForgeStateInfo
    --   [traceAsKESInfo (Proxy @blk) (traceTrans (Consensus.forgeStateInfoTracer consensusTracers))]
    configureTracers config docTxInbound
      [traceTrans (Consensus.txInboundTracer consensusTracers)]
    configureTracers config docTxOutbound
      [traceTrans (Consensus.txOutboundTracer consensusTracers)]
    configureTracers config docLocalTxSubmissionServer
      [traceTrans (Consensus.localTxSubmissionServerTracer consensusTracers)]
    -- configureTracers config docMempool
    --   [traceTrans (Consensus.mempoolTracer consensusTracers)]
    configureTracers config docForge
      [traceTrans (Consensus.forgeTracer consensusTracers)]
    configureTracers config docBlockchainTime
      [traceTrans (Consensus.blockchainTimeTracer consensusTracers)]
    configureTracers config docKeepAliveClient
      [traceTrans (Consensus.keepAliveClientTracer consensusTracers)]
    configureTracers config docTChainSync

      [traceTrans (NodeToClient.tChainSyncTracer nodeToClientTracers)]
    configureTracers config docTTxSubmission
      [traceTrans (NodeToClient.tTxSubmissionTracer nodeToClientTracers)]
    configureTracers config docTStateQuery
      [traceTrans (NodeToClient.tStateQueryTracer nodeToClientTracers)]
    configureTracers config docTChainSync

      [traceTrans (NodeToNode.tChainSyncTracer nodeToNodeTracers)]
    configureTracers config docTChainSync
      [traceTrans (NodeToNode.tChainSyncSerialisedTracer nodeToNodeTracers)]
    configureTracers config docTBlockFetch
      [traceTrans (NodeToNode.tBlockFetchTracer nodeToNodeTracers)]
    configureTracers config docTBlockFetch
      [traceTrans (NodeToNode.tBlockFetchSerialisedTracer nodeToNodeTracers)]
    configureTracers config docTTxSubmissionNode
      [traceTrans (NodeToNode.tTxSubmissionTracer nodeToNodeTracers)]
    configureTracers config docTTxSubmission2Node
      [traceTrans (NodeToNode.tTxSubmission2Tracer nodeToNodeTracers)]
    configureTracers config docIpSubscriptionTracer
      [traceTrans ipSubscriptionTracer]      
    pure ()

docTracers :: forall peer localPeer blk.
  ( Consensus.RunNode blk
  , LogFormatting (ChainDB.InvalidBlockReason blk)
  , GetKESInfo blk
  , TraceConstraints blk
  , Show peer
  , Show localPeer
  )
  => Proxy blk
  -> IO ()
docTracers prx = do
  trBase <- standardTracer Nothing
  tracers :: Tracers peer localPeer blk <- mkDispatchTracers' trBase
  docTracers' prx tracers

docTracers' :: forall blk localPeer peer.
  Proxy blk -> Tracers peer localPeer blk -> IO ()
docTracers' _ Tracers {..} = do
  cdbmTrDoc    <- documentMarkdown docChainDBTraceEvent
                    [traceTrans chainDBTracer]
  cscTrDoc     <- documentMarkdown
                    docChainSyncClientEvent
                    [traceTrans (Consensus.chainSyncClientTracer consensusTracers)]
  csshTrDoc    <- documentMarkdown
                    docChainSyncServerEvent
                    [traceTrans (Consensus.chainSyncServerHeaderTracer consensusTracers)]
  cssbTrDoc    <- documentMarkdown
                    docChainSyncServerEvent
                    [traceTrans (Consensus.chainSyncServerBlockTracer consensusTracers)]
  bfdTrDoc    <- documentMarkdown
                    docBlockFetchDecision
                    [traceTrans (Consensus.blockFetchDecisionTracer consensusTracers)]
  bfcTrDoc    <- documentMarkdown
                    docBlockFetchClient
                    [traceTrans (Consensus.blockFetchClientTracer consensusTracers)]
  bfsTrDoc    <- documentMarkdown
                    docBlockFetchServer
                    [traceTrans (Consensus.blockFetchServerTracer consensusTracers)]
  -- TODO
  -- fsiTrDoc    <- documentMarkdown
  --             (docForgeStateInfo :: Documented
  --               (Consensus.TraceLabelCreds HotKey.KESInfo))
  --             [fsiTr]
  txiTrDoc    <- documentMarkdown
                    docTxInbound
                    [traceTrans (Consensus.txInboundTracer consensusTracers)]
  txoTrDoc    <- documentMarkdown
                    docTxOutbound
                    [traceTrans (Consensus.txOutboundTracer consensusTracers)]
  ltxsTrDoc    <- documentMarkdown
                    docLocalTxSubmissionServer
                    [traceTrans (Consensus.localTxSubmissionServerTracer consensusTracers)]
  -- TODO
  -- mpTrDoc    <- documentMarkdown
  --             (docMempool :: Documented
  --               (TraceEventMempool blk))
  --             [traceTrans (Consensus.mempoolTracer consensusTracers)]
  fTrDoc    <- documentMarkdown
                  docForge
                  [traceTrans (Consensus.forgeTracer consensusTracers)]
  btTrDoc   <- documentMarkdown
                  docBlockchainTime
                  [traceTrans (Consensus.blockchainTimeTracer consensusTracers)]
  kacTrDoc  <- documentMarkdown
                  docKeepAliveClient
                  [traceTrans (Consensus.keepAliveClientTracer consensusTracers)]
  tcsTrDoc  <- documentMarkdown
                  docTChainSync
                  [traceTrans (NodeToClient.tChainSyncTracer nodeToClientTracers)]
  ttsTrDoc  <-  documentMarkdown
                  docTTxSubmission
                  [traceTrans (NodeToClient.tTxSubmissionTracer nodeToClientTracers)]
  tsqTrDoc  <-  documentMarkdown
                  docTStateQuery
                  [traceTrans (NodeToClient.tStateQueryTracer nodeToClientTracers)]
  tcsnTrDoc  <-  documentMarkdown
                  docTChainSync
                  [traceTrans (NodeToNode.tChainSyncTracer nodeToNodeTracers)]
  tcssTrDoc  <-  documentMarkdown
                  docTChainSync
                  [traceTrans (NodeToNode.tChainSyncSerialisedTracer nodeToNodeTracers)]
  tbfTrDoc  <-  documentMarkdown
                  docTBlockFetch
                  [traceTrans (NodeToNode.tBlockFetchTracer nodeToNodeTracers)]
  tbfsTrDoc  <-  documentMarkdown
                  docTBlockFetch
                  [traceTrans (NodeToNode.tBlockFetchSerialisedTracer nodeToNodeTracers)]
  tsnTrDoc   <-  documentMarkdown
                  docTTxSubmissionNode
                  [traceTrans (NodeToNode.tTxSubmissionTracer nodeToNodeTracers)]
  ts2nTrDoc  <-  documentMarkdown
                  docTTxSubmission2Node
                  [traceTrans (NodeToNode.tTxSubmission2Tracer nodeToNodeTracers)]

  let bl = cdbmTrDoc
          ++ cscTrDoc
          ++ csshTrDoc
          ++ cssbTrDoc
          ++ bfdTrDoc
          ++ bfcTrDoc
          ++ bfsTrDoc
--          ++ fsiTrDoc
          ++ txiTrDoc
          ++ txoTrDoc
          ++ ltxsTrDoc
--          ++ mpTrDoc
          ++ fTrDoc
          ++ btTrDoc
          ++ kacTrDoc
          ++ tcsTrDoc
          ++ ttsTrDoc
          ++ tsqTrDoc
          ++ tcsnTrDoc
          ++ tcssTrDoc
          ++ tbfTrDoc
          ++ tbfsTrDoc
          ++ tsnTrDoc
          ++ ts2nTrDoc

  res <- buildersToText bl
  T.writeFile "/home/yupanqui/IOHK/CardanoLogging.md" res
  pure ()
