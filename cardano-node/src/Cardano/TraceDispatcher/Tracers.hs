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
{-# OPTIONS_GHC -Wno-deprecations  #-}


module Cardano.TraceDispatcher.Tracers
  ( mkDispatchTracers
  , docTracers
  ) where

import qualified Data.Text.IO as T
import           Network.Mux (MuxTrace (..), WithMuxBearer (..))
import qualified Network.Socket as Socket

import           Cardano.Logging
import           Cardano.Logging.Resources
import           Cardano.Logging.Resources.Types
import           Cardano.Prelude hiding (trace)
import           Cardano.TraceDispatcher.BasicInfo.Combinators
import           Cardano.TraceDispatcher.BasicInfo.Types (BasicInfo)
import           Cardano.TraceDispatcher.ChainDB.Combinators
import           Cardano.TraceDispatcher.ChainDB.Docu
import           Cardano.TraceDispatcher.Consensus.Combinators
import           Cardano.TraceDispatcher.Consensus.Docu
import           Cardano.TraceDispatcher.Consensus.ForgingThreadStats
                     (docForgeStats, forgeThreadStats, ForgeThreadStats)
import           Cardano.TraceDispatcher.Consensus.StateInfo
import           Cardano.TraceDispatcher.Formatting ()
import           Cardano.TraceDispatcher.Network.Combinators
import           Cardano.TraceDispatcher.Network.Docu
import           Cardano.TraceDispatcher.Peer
import           Cardano.TraceDispatcher.Network.Formatting ()
import           Cardano.TraceDispatcher.Resources (namesForResources,
                     severityResources, startResourceTracer)
import qualified "trace-dispatcher" Control.Tracer as NT
-- import           Cardano.TraceDispatcher.Consensus.StartLeadershipCheck


import           Cardano.Node.Configuration.Logging (EKGDirect)

import qualified Cardano.BM.Data.Trace as Old
import           Cardano.Tracing.Config (TraceOptions (..))
import           Cardano.Tracing.Constraints (TraceConstraints)
import           Cardano.Tracing.Kernel (NodeKernelData)
import           Cardano.Tracing.OrphanInstances.Common (ToObject)
import           Cardano.Tracing.Tracers
import           "contra-tracer" Control.Tracer (Tracer (..))

import           Ouroboros.Consensus.Block.Forging
import           Ouroboros.Consensus.BlockchainTime.WallClock.Util
                     (TraceBlockchainTimeEvent (..))
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import           Ouroboros.Consensus.Byron.Ledger.Config (BlockConfig)
import           Ouroboros.Consensus.Ledger.Query (Query)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx,
                     GenTxId)
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
import qualified Ouroboros.Consensus.Network.NodeToClient as NtC
import qualified Ouroboros.Consensus.Network.NodeToNode as NtN
import qualified Ouroboros.Consensus.Node.Run as Consensus
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import           Ouroboros.Consensus.Shelley.Ledger.Block
import qualified Ouroboros.Consensus.Shelley.Protocol.HotKey as HotKey
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Storage.Serialisation (SerialisedHeader)


import           Ouroboros.Network.Block (Point (..), Serialised, Tip)
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.BlockFetch.Decision
import qualified Ouroboros.Network.Diffusion as ND
import           Ouroboros.Network.Driver.Simple (TraceSendRecv)
import           Ouroboros.Network.KeepAlive (TraceKeepAliveClient (..))
import qualified Ouroboros.Network.NodeToClient as NtC
import           Ouroboros.Network.NodeToNode (ErrorPolicyTrace (..),
                     WithAddr (..))
import qualified Ouroboros.Network.NodeToNode as NtN
import           Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch)
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
                     (LocalStateQuery)
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type as LTS
import           Ouroboros.Network.Protocol.TxSubmission.Type (TxSubmission)
import           Ouroboros.Network.Protocol.TxSubmission2.Type (TxSubmission2)
import           Ouroboros.Network.Snocket (LocalAddress (..))
import           Ouroboros.Network.Subscription.Dns (DnsTrace (..),
                     WithDomainName (..))
import           Ouroboros.Network.Subscription.Ip (WithIPList (..))
import           Ouroboros.Network.Subscription.Worker (SubscriptionTrace (..))
import           Ouroboros.Network.TxSubmission.Inbound
                     (TraceTxSubmissionInbound)
import           Ouroboros.Network.TxSubmission.Outbound
                     (TraceTxSubmissionOutbound)

import           Debug.Trace

type Peer = NtN.ConnectionId Socket.SockAddr

-- | Construct tracers for all system components.
--
mkDispatchTracers
  :: forall peer localPeer blk.
  ( Consensus.RunNode blk
  , LogFormatting (ChainDB.InvalidBlockReason blk)
  , TraceConstraints blk
  , Show peer, Eq peer
  , Show localPeer
  , ToObject peer
  , ToObject localPeer
  , LogFormatting peer
  , LogFormatting localPeer
  )
  => BlockConfig blk
  -> TraceOptions
  -> Old.Trace IO Text
  -> NodeKernelData blk
  -> Maybe EKGDirect
  -> Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> TraceConfig
  -> [BasicInfo]
  -> IO (Tracers peer localPeer blk)
mkDispatchTracers _blockConfig (TraceDispatcher _trSel) _tr nodeKernel _ekgDirect
  trBase trForward mbTrEKG trConfig basicInfos = do
    trace ("TraceConfig " <> show trConfig) $ pure ()
    cdbmTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainDB"
                namesForChainDBTraceEvents
                severityChainDB
                allPublic
    cscTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncClient"
                namesForChainSyncClientEvent
                severityChainSyncClientEvent
                allPublic
    csshTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncServerHeader"
                namesForChainSyncServerEvent
                severityChainSyncServerEvent
                allPublic
    cssbTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncServerBlock"
                namesForChainSyncServerEvent
                severityChainSyncServerEvent
                allPublic
    bfdTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockFetchDecision"
                namesForBlockFetchDecision
                severityBlockFetchDecision
                allConfidential
    bfcTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockFetchClient"
                namesForBlockFetchClient
                severityBlockFetchClient
                allPublic
    bfsTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockFetchServer"
                namesForBlockFetchServer
                severityBlockFetchServer
                allPublic
    fsiTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "ForgeStateInfo"
                namesForStateInfo
                severityStateInfo
                allPublic
    txiTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "TxInbound"
                namesForTxInbound
                severityTxInbound
                allPublic
    txoTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "TxOutbound"
                namesForTxOutbound
                severityTxOutbound
                allPublic
    ltxsTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                "LocalTxSubmissionServer"
                namesForLocalTxSubmissionServer
                severityLocalTxSubmissionServer
                allPublic
    mpTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                "Mempool"
                namesForMempool
                severityMempool
                allPublic
    fTr    <- mkCardanoTracer'
                trBase trForward mbTrEKG
                "Forge"
                namesForForge
                severityForge
                allPublic
                (forgeTracerTransform nodeKernel)
    fSttTr <- mkCardanoTracer'
                trBase trForward mbTrEKG
                "ForgeStats"
                namesForForge
                severityForge
                allPublic
                forgeThreadStats
    btTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockchainTime"
                namesForBlockchainTime
                severityBlockchainTime
                allPublic
    kacTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "KeepAliveClient"
                namesForKeepAliveClient
                severityKeepAliveClient
                allPublic
    tcsTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncClient"
                namesForTChainSync
                severityTChainSync
                allPublic
    ttsTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "TxSubmissionClient"
                namesForTTxSubmission
                severityTTxSubmission
                allPublic
    tsqTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "StateQueryClient"
                namesForTStateQuery
                severityTStateQuery
                allPublic
    tcsnTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncNode"
                namesForTChainSyncNode
                severityTChainSyncNode
                allPublic
    tcssTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncSerialised"
                namesForTChainSyncSerialised
                severityTChainSyncSerialised
                allPublic
    tbfTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockFetch"
                namesForTBlockFetch
                severityTBlockFetch
                allPublic
    tbfsTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockFetchSerialised"
                namesForTBlockFetchSerialised
                severityTBlockFetchSerialised
                allPublic
    tsnTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "TxSubmissionTracer"
                namesForTxSubmissionNode
                severityTxSubmissionNode
                allPublic
    ts2nTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "TxSubmission2"
                namesForTxSubmission2Node
                severityTxSubmission2Node
                allPublic
    ipsTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "IpSubscription"
                namesForIPSubscription
                severityIPSubscription
                allPublic
    dnssTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "DnsSubscription"
                namesForDNSSubscription
                severityDNSSubscription
                allPublic
    dnsrTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "DNSResolver"
                namesForDNSResolver
                severityDNSResolver
                allPublic
    errpTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "ErrorPolicy"
                namesForErrorPolicy
                severityErrorPolicy
                allPublic
    lerrpTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "LocalErrorPolicy"
                namesForLocalErrorPolicy
                severityLocalErrorPolicy
                allPublic
    apTr    <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "AcceptPolicy"
                namesForAcceptPolicy
                severityAcceptPolicy
                allPublic
    muxTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "Mux"
                namesForMux
                severityMux
                allPublic
    muxLTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "MuxLocal"
                namesForMux
                severityMux
                allPublic
    hsTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "Handshake"
                namesForHandshake
                severityHandshake
                allPublic
    lhsTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "LocalHandshake"
                namesForLocalHandshake
                severityLocalHandshake
                allPublic
    diTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "DiffusionInit"
                namesForDiffusionInit
                severityDiffusionInit
                allPublic
    rsTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                "Resources"
                (\ _ -> [])
                (\ _ -> Info)
                allPublic
    biTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                "BasicInfo"
                namesForBasicInfo
                severityBasicInfo
                allPublic
    pTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                "Peers"
                namesForPeers
                severityPeers
                allPublic

    configureTracers trConfig docChainDBTraceEvent    [cdbmTr]
    configureTracers trConfig docChainSyncClientEvent [cscTr]
    configureTracers trConfig docChainSyncServerEvent [csshTr]
    configureTracers trConfig docChainSyncServerEvent [cssbTr]
    configureTracers trConfig docBlockFetchDecision   [bfdTr]
    configureTracers trConfig docBlockFetchClient     [bfcTr]
    configureTracers trConfig docBlockFetchServer     [bfsTr]
    configureTracers trConfig docForgeStateInfo       [fsiTr]
    configureTracers trConfig docTxInbound            [txiTr]
    configureTracers trConfig docTxOutbound           [txoTr]
    configureTracers trConfig docLocalTxSubmissionServer [ltxsTr]
    configureTracers trConfig docMempool              [mpTr]
    configureTracers trConfig docForge                [fTr, fSttTr]
    configureTracers trConfig docBlockchainTime       [btTr]
    configureTracers trConfig docKeepAliveClient      [kacTr]
    configureTracers trConfig docTChainSync           [tcsTr]
    configureTracers trConfig docTTxSubmission        [ttsTr]
    configureTracers trConfig docTStateQuery          [tsqTr]
    configureTracers trConfig docTChainSync           [tcsnTr]
    configureTracers trConfig docTChainSync           [tcssTr]
    configureTracers trConfig docTBlockFetch          [tbfTr]
    configureTracers trConfig docTBlockFetch          [tbfsTr]
    configureTracers trConfig docTTxSubmissionNode    [tsnTr]
    configureTracers trConfig docTTxSubmission2Node   [ts2nTr]
    configureTracers trConfig docIPSubscription       [ipsTr]
    configureTracers trConfig docDNSSubscription      [dnssTr]
    configureTracers trConfig docDNSResolver          [dnsrTr]
    configureTracers trConfig docErrorPolicy          [errpTr]
    configureTracers trConfig docLocalErrorPolicy     [lerrpTr]
    configureTracers trConfig docAcceptPolicy         [apTr]
    configureTracers trConfig docMux                  [muxTr]
    configureTracers trConfig docMux                  [muxLTr]
    configureTracers trConfig docHandshake            [hsTr]
    configureTracers trConfig docLocalHandshake       [lhsTr]
    configureTracers trConfig docDiffusionInit        [diTr]
    configureTracers trConfig docResourceStats        [rsTr]
    configureTracers trConfig docBasicInfo            [biTr]
    configureTracers trConfig docPeers                [pTr]

-- -- TODO JNF Code for debugging frequency limiting
--     void . forkIO $
--       sendContinously
--         0.1
--         cdbmTr
--         (ChainDB.TraceOpenEvent
--           (ChainDB.OpenedDB (Point Origin) (Point Origin)))
-- -- End of  debugging code

    mapM_ (traceWith biTr) basicInfos
    startResourceTracer rsTr
    startPeerTracer pTr nodeKernel

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
        , Consensus.forgeTracer =
            Tracer (traceWith (contramap Left fTr))
            <> Tracer (traceWith (contramap Left fSttTr))
        , Consensus.blockchainTimeTracer = Tracer (traceWith btTr)
        , Consensus.keepAliveClientTracer = Tracer (traceWith kacTr)
        }
      , nodeToClientTracers = NtC.Tracers
        { NtC.tChainSyncTracer = Tracer (traceWith tcsTr)
        , NtC.tTxSubmissionTracer = Tracer (traceWith ttsTr)
        , NtC.tStateQueryTracer = Tracer (traceWith tsqTr)
        }
      , nodeToNodeTracers = NtN.Tracers
        { NtN.tChainSyncTracer = Tracer (traceWith tcsnTr)
        , NtN.tChainSyncSerialisedTracer = Tracer (traceWith tcssTr)
        , NtN.tBlockFetchTracer = Tracer (traceWith tbfTr)
        , NtN.tBlockFetchSerialisedTracer = Tracer (traceWith tbfsTr)
        , NtN.tTxSubmissionTracer = Tracer (traceWith tsnTr)
        , NtN.tTxSubmission2Tracer = Tracer (traceWith ts2nTr)
        }
      , ipSubscriptionTracer = Tracer (traceWith ipsTr)
      , dnsSubscriptionTracer= Tracer (traceWith dnssTr)
      , dnsResolverTracer = Tracer (traceWith dnsrTr)
      , errorPolicyTracer = Tracer (traceWith errpTr)
      , localErrorPolicyTracer = Tracer (traceWith lerrpTr)
      , acceptPolicyTracer = Tracer (traceWith apTr)
      , muxTracer = Tracer (traceWith muxTr)
      , muxLocalTracer = Tracer (traceWith muxLTr)
      , handshakeTracer = Tracer (traceWith hsTr)
      , localHandshakeTracer = Tracer (traceWith lhsTr)
      , diffusionInitializationTracer = Tracer (traceWith diTr)
      , basicInfoTracer = Tracer (traceWith biTr)
    }

mkDispatchTracers blockConfig tOpts tr nodeKern ekgDirect _ _ _ _ _ =
  mkTracers blockConfig tOpts tr nodeKern ekgDirect

-- -- TODO JNF Code for debugging frequency limiting
-- sendContinously ::
--      Double
--   -> Trace IO m
--   -> m
--   -> IO ()
-- sendContinously delay tracer message = do
--   threadDelay (round (delay * 1000000.0))
--   traceWith tracer message
--   sendContinously delay tracer message
-- -- End of  debugging code

docTracers :: forall blk t.
  ( Show t
  , forall result. Show (Query blk result)
  , TraceConstraints blk
  , LogFormatting (ChainDB.InvalidBlockReason blk)
  , LedgerSupportsProtocol blk
  , Consensus.RunNode blk
  )
  => FilePath
  -> FilePath
  -> Proxy blk
  -> IO ()
docTracers configFileName outputFileName _ = do
    trConfig   <- readConfiguration configFileName
    trBase     <- docTracer (Stdout HumanFormatColoured)
    trForward  <- docTracer Forwarder
    mbTrEKG :: Maybe (Trace IO FormattedMessage) <-
                  liftM Just (docTracer EKGBackend)
    cdbmTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainDB"
                namesForChainDBTraceEvents
                severityChainDB
                allPublic
    cscTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncClient"
                namesForChainSyncClientEvent
                severityChainSyncClientEvent
                allPublic
    csshTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncServerHeader"
                namesForChainSyncServerEvent
                severityChainSyncServerEvent
                allPublic
    cssbTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncServerBlock"
                namesForChainSyncServerEvent
                severityChainSyncServerEvent
                allPublic
    bfdTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockFetchDecision"
                namesForBlockFetchDecision
                severityBlockFetchDecision
                allConfidential
    bfcTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockFetchClient"
                namesForBlockFetchClient
                severityBlockFetchClient
                allPublic
    bfsTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockFetchServer"
                namesForBlockFetchServer
                severityBlockFetchServer
                allPublic
    fsiTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "ForgeStateInfo"
                namesForStateInfo
                severityStateInfo
                allPublic
    txiTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "TxInbound"
                namesForTxInbound
                severityTxInbound
                allPublic
    txoTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "TxOutbound"
                namesForTxOutbound
                severityTxOutbound
                allPublic
    ltxsTr <- mkCardanoTracer
                trBase trForward mbTrEKG
                "LocalTxSubmissionServer"
                namesForLocalTxSubmissionServer
                severityLocalTxSubmissionServer
                allPublic
    -- mpTr   <- mkCardanoTracer
    --             "Mempool"
    --             namesForMempool
    --             severityMempool
    --             allPublic
    --             trBase trForward mbTrEKG
    fTr    <- mkCardanoTracer
                trBase trForward mbTrEKG
                "Forge"
                namesForForge
                severityForge
                allPublic
    fSttTr <- mkCardanoTracer'
                trBase trForward mbTrEKG
                "ForgeStats"
                namesForForge
                severityForge
                allPublic
                forgeThreadStats
    btTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockchainTime"
                namesForBlockchainTime
                severityBlockchainTime
                allPublic
    kacTr  <- mkCardanoTracer
                trBase trForward mbTrEKG
                "KeepAliveClient"
                namesForKeepAliveClient
                severityKeepAliveClient
                allPublic
    tcsTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncClient"
                namesForTChainSync
                severityTChainSync
                allPublic
    ttsTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "TxSubmissionClient"
                namesForTTxSubmission
                severityTTxSubmission
                allPublic
    tsqTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "StateQueryClient"
                namesForTStateQuery
                severityTStateQuery
                allPublic
    tcsnTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncNode"
                namesForTChainSyncNode
                severityTChainSyncNode
                allPublic
    tcssTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "ChainSyncSerialised"
                namesForTChainSyncSerialised
                severityTChainSyncSerialised
                allPublic
    tbfTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockFetch"
                namesForTBlockFetch
                severityTBlockFetch
                allPublic
    tbfsTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "BlockFetchSerialised"
                namesForTBlockFetchSerialised
                severityTBlockFetchSerialised
                allPublic
    tsnTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "TxSubmissionTracer"
                namesForTxSubmissionNode
                severityTxSubmissionNode
                allPublic
    ts2nTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "TxSubmission2"
                namesForTxSubmission2Node
                severityTxSubmission2Node
                allPublic
    ipsTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "IpSubscription"
                namesForIPSubscription
                severityIPSubscription
                allPublic
    dnssTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "DnsSubscription"
                namesForDNSSubscription
                severityDNSSubscription
                allPublic
    dnsrTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "DNSResolver"
                namesForDNSResolver
                severityDNSResolver
                allPublic
    errpTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "ErrorPolicy"
                namesForErrorPolicy
                severityErrorPolicy
                allPublic
    lerrpTr <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "LocalErrorPolicy"
                namesForLocalErrorPolicy
                severityLocalErrorPolicy
                allPublic
    apTr    <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "AcceptPolicy"
                namesForAcceptPolicy
                severityAcceptPolicy
                allPublic
    muxTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "Mux"
                namesForMux
                severityMux
                allPublic
    muxLTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "MuxLocal"
                namesForMux
                severityMux
                allPublic
    hsTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "Handshake"
                namesForHandshake
                severityHandshake
                allPublic
    lhsTr  <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "LocalHandshake"
                namesForLocalHandshake
                severityLocalHandshake
                allPublic
    diTr   <-  mkCardanoTracer
                trBase trForward mbTrEKG
                "DiffusionInit"
                namesForDiffusionInit
                severityDiffusionInit
                allPublic
    rsTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                "Resources"
                namesForResources
                severityResources
                allPublic
    biTr   <- mkCardanoTracer
                trBase trForward mbTrEKG
                "BasicInfo"
                namesForBasicInfo
                severityBasicInfo
                allPublic

    configureTracers trConfig docChainDBTraceEvent    [cdbmTr]
    configureTracers trConfig docChainSyncClientEvent [cscTr]
    configureTracers trConfig docChainSyncServerEvent [csshTr]
    configureTracers trConfig docChainSyncServerEvent [cssbTr]
    configureTracers trConfig docBlockFetchDecision   [bfdTr]
    configureTracers trConfig docBlockFetchClient     [bfcTr]
    configureTracers trConfig docBlockFetchServer     [bfsTr]
    configureTracers trConfig docForgeStateInfo       [fsiTr]
    configureTracers trConfig docTxInbound            [txiTr]
    configureTracers trConfig docTxOutbound           [txoTr]
    configureTracers trConfig docLocalTxSubmissionServer [ltxsTr]
--    configureTracers trConfig docMempool              [mpTr]
    configureTracers trConfig docForge                [fTr, fSttTr]
    configureTracers trConfig docBlockchainTime       [btTr]
    configureTracers trConfig docKeepAliveClient      [kacTr]
    configureTracers trConfig docTChainSync           [tcsTr]
    configureTracers trConfig docTTxSubmission        [ttsTr]
    configureTracers trConfig docTStateQuery          [tsqTr]
    configureTracers trConfig docTChainSync           [tcsnTr]
    configureTracers trConfig docTChainSync           [tcssTr]
    configureTracers trConfig docTBlockFetch          [tbfTr]
    configureTracers trConfig docTBlockFetch          [tbfsTr]
    configureTracers trConfig docTTxSubmissionNode    [tsnTr]
    configureTracers trConfig docTTxSubmission2Node   [ts2nTr]
    configureTracers trConfig docIPSubscription       [ipsTr]
    configureTracers trConfig docDNSSubscription      [dnssTr]
    configureTracers trConfig docDNSResolver          [dnsrTr]
    configureTracers trConfig docErrorPolicy          [errpTr]
    configureTracers trConfig docLocalErrorPolicy     [lerrpTr]
    configureTracers trConfig docAcceptPolicy         [apTr]
    configureTracers trConfig docMux                  [muxTr]
    configureTracers trConfig docMux                  [muxLTr]
    configureTracers trConfig docHandshake            [hsTr]
    configureTracers trConfig docLocalHandshake       [lhsTr]
    configureTracers trConfig docDiffusionInit        [diTr]
    configureTracers trConfig docResourceStats        [rsTr]
    configureTracers trConfig docBasicInfo            [biTr]

    cdbmTrDoc    <- documentMarkdown
                      (docChainDBTraceEvent :: Documented
                        (ChainDB.TraceEvent blk))
                      [cdbmTr]
    cscTrDoc    <- documentMarkdown
                (docChainSyncClientEvent :: Documented
                  (BlockFetch.TraceLabelPeer Peer
                    (TraceChainSyncClientEvent blk)))
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
                  [BlockFetch.TraceLabelPeer Peer (FetchDecision [Point (Header blk)])])
                [bfdTr]
    bfcTrDoc    <- documentMarkdown
                (docBlockFetchClient :: Documented
                  (BlockFetch.TraceLabelPeer Peer (BlockFetch.TraceFetchClientState (Header blk))))
                [bfcTr]
    bfsTrDoc    <- documentMarkdown
                (docBlockFetchServer :: Documented
                  (TraceBlockFetchServerEvent blk))
                [bfsTr]
    -- fsiTrDoc    <- documentMarkdown
    --             (docForgeStateInfo :: Documented
    --               (Consensus.TraceLabelCreds HotKey.KESInfo))
    --             [fsiTr]
    txiTrDoc    <- documentMarkdown
                (docTxInbound :: Documented
                  (BlockFetch.TraceLabelPeer Peer
                    (TraceTxSubmissionInbound (GenTxId blk) (GenTx blk))))
                [txiTr]
    txoTrDoc    <- documentMarkdown
                (docTxOutbound :: Documented
                  (BlockFetch.TraceLabelPeer Peer
                    (TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk))))
                [txoTr]
    ltxsTrDoc    <- documentMarkdown
                (docLocalTxSubmissionServer :: Documented
                  (TraceLocalTxSubmissionServerEvent blk))
                [ltxsTr]
    -- mpTrDoc    <- documentMarkdown
    --             (docMempool :: Documented
    --               (TraceEventMempool blk))
    --             [mpTr]
    fTrDoc    <- documentMarkdown
                (docForge :: Documented
                  (ForgeTracerType blk))
                [fTr]
    -- TODO JNF Docu for forgeThreadStats
    -- fSttTr'      <- forgeThreadStats fSttTr
    -- fSttTrDoc    <- documentMarkdown
    --             (docForgeStats :: Documented
    --               ForgeThreadStats)
    --             [unfold fSttTr]
    btTrDoc   <- documentMarkdown
                (docBlockchainTime :: Documented
                  (TraceBlockchainTimeEvent t))
                [btTr]
    kacTrDoc  <- documentMarkdown
                (docKeepAliveClient :: Documented
                  (TraceKeepAliveClient Peer))
                [kacTr]
    tcsTrDoc  <- documentMarkdown
                (docTChainSync :: Documented
                  (BlockFetch.TraceLabelPeer Peer
                    (TraceSendRecv
                      (ChainSync (Serialised blk) (Point blk) (Tip blk)))))
                [tcsTr]
    ttsTrDoc  <-  documentMarkdown
                (docTTxSubmission :: Documented
                   (BlockFetch.TraceLabelPeer
                      Peer
                      (TraceSendRecv
                         (LTS.LocalTxSubmission
                            (GenTx blk) (ApplyTxErr blk)))))
                [ttsTr]
    tsqTrDoc  <-  documentMarkdown
                (docTStateQuery :: Documented
                   (BlockFetch.TraceLabelPeer Peer
                    (TraceSendRecv
                      (LocalStateQuery blk (Point blk) (Query blk)))))
                [tsqTr]
    tcsnTrDoc  <-  documentMarkdown
                (docTChainSync :: Documented
                  (BlockFetch.TraceLabelPeer Peer
                    (TraceSendRecv
                      (ChainSync (Header blk) (Point blk) (Tip blk)))))
                [tcsnTr]
    tcssTrDoc  <-  documentMarkdown
                (docTChainSync :: Documented
                  (BlockFetch.TraceLabelPeer Peer
                    (TraceSendRecv
                      (ChainSync (SerialisedHeader blk) (Point blk) (Tip blk)))))
                [tcssTr]
    tbfTrDoc  <-  documentMarkdown
                (docTBlockFetch :: Documented
                  (BlockFetch.TraceLabelPeer Peer
                    (TraceSendRecv
                      (BlockFetch blk (Point blk)))))
                [tbfTr]
    tbfsTrDoc  <-  documentMarkdown
                (docTBlockFetch :: Documented
                  (BlockFetch.TraceLabelPeer Peer
                    (TraceSendRecv
                      (BlockFetch (Serialised blk) (Point blk)))))
                [tbfsTr]
    tsnTrDoc   <-  documentMarkdown
                (docTTxSubmissionNode :: Documented
                  (BlockFetch.TraceLabelPeer Peer
                    (TraceSendRecv
                      (TxSubmission (GenTxId blk) (GenTx blk)))))
                [tsnTr]
    ts2nTrDoc  <-  documentMarkdown
                    (docTTxSubmission2Node :: Documented
                      (BlockFetch.TraceLabelPeer Peer
                        (TraceSendRecv
                          (TxSubmission2 (GenTxId blk) (GenTx blk)))))
                    [ts2nTr]
    ipsTrDoc   <-  documentMarkdown
                    (docIPSubscription :: Documented
                      (WithIPList (SubscriptionTrace Socket.SockAddr)))
                    [ipsTr]
    dnssTrDoc   <-  documentMarkdown
                    (docDNSSubscription :: Documented
                      (WithDomainName (SubscriptionTrace Socket.SockAddr)))
                    [dnssTr]
    dnsrTrDoc   <-  documentMarkdown
                    (docDNSResolver :: Documented (WithDomainName DnsTrace))
                    [dnsrTr]
    errpTrDoc   <-  documentMarkdown
                    (docErrorPolicy :: Documented
                      (WithAddr Socket.SockAddr ErrorPolicyTrace))
                    [errpTr]
    lerrpTrDoc  <-  documentMarkdown
                    (docLocalErrorPolicy :: Documented
                      (WithAddr LocalAddress ErrorPolicyTrace))
                    [lerrpTr]
    apTrDoc     <-  documentMarkdown
                    (docAcceptPolicy :: Documented
                       NtN.AcceptConnectionsPolicyTrace)
                    [apTr]
    muxTrDoc     <-  documentMarkdown
                    (docMux :: Documented
                      (WithMuxBearer Peer MuxTrace))
                    [muxTr]
    muxLTrDoc    <-  documentMarkdown
                    (docMux :: Documented
                      (WithMuxBearer Peer MuxTrace))
                    [muxLTr]
    hsTrDoc      <-  documentMarkdown
                    (docHandshake :: Documented NtN.HandshakeTr)
                    [hsTr]
    lhsTrDoc     <-  documentMarkdown
                    (docLocalHandshake :: Documented NtC.HandshakeTr)
                    [lhsTr]
    diTrDoc      <-  documentMarkdown
                    (docDiffusionInit :: Documented ND.DiffusionInitializationTracer)
                    [diTr]
    rsTrDoc      <-  documentMarkdown
                    (docResourceStats :: Documented ResourceStats)
                    [rsTr]
    biTrDoc      <-  documentMarkdown
                    (docBasicInfo :: Documented BasicInfo)
                    [biTr]

    let bl = cdbmTrDoc
            ++ cscTrDoc
            ++ csshTrDoc
            ++ cssbTrDoc
            ++ bfdTrDoc
            ++ bfcTrDoc
            ++ bfsTrDoc
--            ++ fsiTrDoc
            ++ txiTrDoc
            ++ txoTrDoc
            ++ ltxsTrDoc
--            ++ mpTrDoc
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
            ++ ipsTrDoc
            ++ dnssTrDoc
            ++ dnsrTrDoc
            ++ errpTrDoc
            ++ lerrpTrDoc
            ++ apTrDoc
            ++ muxTrDoc
            ++ muxLTrDoc
            ++ hsTrDoc
            ++ lhsTrDoc
            ++ diTrDoc
            ++ rsTrDoc
            ++ biTrDoc

    res <- buildersToText bl trConfig
    T.writeFile outputFileName res
    pure ()
