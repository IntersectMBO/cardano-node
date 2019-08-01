{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE NumericUnderscores   #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
-- required for 'Show' instance of 'WithTip'
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

module Run (
      runNode
    ) where

import           Codec.CBOR.Read (DeserialiseFailure)
import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.Serialise as Serialise (decode, encode)
import           Codec.SerialiseTerm
import qualified Control.Concurrent.Async as Async
import           Control.Exception
import           Control.Monad
import           Control.Tracer
import           Crypto.Random
import qualified Data.ByteString.Char8 as BSC
import           Data.ByteString.Lazy (ByteString)
import           Data.Either (partitionEithers)
import           Data.Functor.Contravariant (contramap)
import qualified Data.List as List
import           Data.Proxy (Proxy (..))
import           Data.Semigroup ((<>))
import           Data.Text (Text, pack)
import           Data.Time.Clock (DiffTime, secondsToDiffTime)
import           Network.Socket as Socket
import           System.Directory (removeFile)
import           System.IO.Error (isDoesNotExistError)

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM

import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Data.Aggregated (Measurable (PureI))
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.BackendKind (BackendKind (TraceForwarderBK))
import           Cardano.BM.Data.LogItem (LOContent (LogValue), LogObject (..),
                                          PrivacyAnnotation (Confidential),
                                          mkLOMeta)
import           Cardano.BM.Data.Severity (Severity (..))
import           Cardano.BM.Data.Tracer (ToLogObject (..))
import           Cardano.BM.Trace (appendName, traceNamedObject)
import           Cardano.Shell.Constants.Types (CardanoConfiguration (..),)
import           Cardano.Shell.Features.Logging (LoggingLayer (..))

import           Network.TypedProtocol.Driver (TraceSendRecv)

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.NodeToClient as NodeToClient
import           Ouroboros.Network.NodeToNode as NodeToNode
import           Ouroboros.Network.Socket
import           Ouroboros.Network.Subscription.Common
import           Ouroboros.Network.Subscription.Dns
import           Ouroboros.Network.TxSubmission.Inbound (TraceTxSubmissionInbound)
import           Ouroboros.Network.TxSubmission.Outbound (TraceTxSubmissionOutbound)
import           Ouroboros.Network.BlockFetch.Decision (FetchDecision)
import           Ouroboros.Network.BlockFetch.ClientState (TraceFetchClientState, TraceLabelPeer)

import           Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch)
import           Ouroboros.Network.Protocol.BlockFetch.Codec
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import           Ouroboros.Network.Protocol.ChainSync.Codec
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.Protocol.LocalTxSubmission.Codec
import           Ouroboros.Network.Protocol.TxSubmission.Codec

import           Ouroboros.Consensus.Block (BlockProtocol, Header)
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.ChainSyncClient (ClockSkew (..))
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState)
import           Ouroboros.Consensus.Mempool.API (GenTx, GenTxId, TraceEventMempool (..))
import           Ouroboros.Consensus.Node
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Tracers
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.NodeNetwork
import           Ouroboros.Consensus.Protocol hiding (Protocol)
import qualified Ouroboros.Consensus.Protocol as Consensus
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.STM
import           Ouroboros.Consensus.Util.ThreadRegistry

import           Ouroboros.Storage.ChainDB (ChainDB)
import qualified Ouroboros.Storage.ChainDB as ChainDB
import           Ouroboros.Storage.Common
import           Ouroboros.Storage.ImmutableDB (ValidationPolicy (..))
import           Ouroboros.Storage.LedgerDB.DiskPolicy (defaultDiskPolicy)
import           Ouroboros.Storage.LedgerDB.MemPolicy (defaultMemPolicy)
import qualified Ouroboros.Storage.LedgerDB.OnDisk as LedgerDB

import           Cardano.Node.CLI
import           CLI
import           Topology
import           TraceAcceptor
import           TxSubmission
#ifdef UNIX
import           LiveView
#endif


-- | Peer identifier used in consensus application
--
data Peer = Peer { localAddr  :: SockAddr
                 , remoteAddr :: SockAddr }
  deriving (Eq, Ord, Show)

instance Condense Peer where
    condense (Peer localAddr remoteAddr) = (show localAddr) ++ (show remoteAddr)


runNode :: NodeCLIArguments -> LoggingLayer -> CardanoConfiguration -> IO ()
runNode nodeCli@NodeCLIArguments{..} loggingLayer cc = do
    let !tr = llAppendName loggingLayer "node" (llBasicTrace loggingLayer)
    -- If the user asked to submit a transaction, we don't have to spin up a
    -- full node, we simply transmit it and exit.
    case command of

      TxSubmitter topology tx protocol -> do
        let trace'      = appendName (pack (show (node topology))) tr
        let tracer      = contramap pack $ toLogObject trace'
        SomeProtocol p  <- fromProtocol cc protocol
        handleTxSubmission p topology tx tracer

      TraceAcceptor -> do
        let trace'      = appendName "acceptor" tr
        let tracer      = contramap pack $ toLogObject trace'
        handleTraceAcceptor tracer

      SimpleNode topology myNodeAddress protocol viewMode traceOptions -> do
        let trace'      = appendName (pack $ show $ node topology) tr
        SomeProtocol p  <- fromProtocol cc protocol
        case viewMode of
          SimpleView -> handleSimpleNode p nodeCli myNodeAddress topology trace' (getNodeTraces traceOptions trace') cc
          LiveView   -> do
#ifdef UNIX
            let c = llConfiguration loggingLayer
            -- We run 'handleSimpleNode' as usual and run TUI thread as well.
            -- turn off logging to the console, only forward it through a pipe to a central logging process
            CM.setDefaultBackends c [TraceForwarderBK, UserDefinedBK "LiveViewBackend"]
            -- User will see a terminal graphics and will be able to interact with it.
            nodeThread <- Async.async $ handleSimpleNode p nodeCli myNodeAddress topology trace' (getNodeTraces traceOptions trace') cc

            be :: LiveViewBackend Text <- realize c
            let lvbe = MkBackend { bEffectuate = effectuate be, bUnrealize = unrealize be }
            llAddBackend loggingLayer lvbe "LiveViewBackend"
            setTopology be topology
            setNodeThread be nodeThread
            captureCounters be tr

            void $ Async.waitAny [nodeThread]
#else
            handleSimpleNode p nodeCli myNodeAddress topology trace' (getNodeTraces traceOptions trace') cc
#endif

-- | Sets up a simple node, which will run the chain sync protocol and block
-- fetch protocol, and, if core, will also look at the mempool when trying to
-- create a new block.
handleSimpleNode :: forall blk. (RunNode blk, TraceConstraints blk, Show (GenTxId blk))
                 => Consensus.Protocol blk
                 -> NodeCLIArguments
                 -> NodeAddress
                 -> TopologyInfo
                 -> Tracer IO (LogObject Text)
                 -> Traces Peer blk
                 -> CardanoConfiguration
                 -> IO ()
handleSimpleNode p NodeCLIArguments{..}
                 myNodeAddress
                 (TopologyInfo myNodeId topologyFile)
                 trace
                 nodeTraces
                 CardanoConfiguration{..}
    = do
    let tracer = contramap pack $ toLogObject trace
    traceWith tracer $ "System started at " <> show systemStart
    NetworkTopology nodeSetups <-
      either error id <$> readTopologyFile topologyFile

    let producers' = case List.lookup myNodeAddress $ map (\ns -> (nodeAddress ns, producers ns)) nodeSetups of
          Just ps -> ps
          Nothing -> error ("handleSimpleNode: own address " ++ show myNodeAddress ++ " not found in topology")

    traceWith tracer $ "**************************************"
    traceWith tracer $ "I am Node = " <> show myNodeAddress
    traceWith tracer $ "My producers are " <> show producers'
    traceWith tracer $ "**************************************"

    let ProtocolInfo{pInfoConfig, pInfoInitLedger, pInfoInitState} =
          protocolInfo (NumCoreNodes (length nodeSetups)) (CoreNodeId nid) p

    withThreadRegistry $ \registry -> do

      let callbacks :: NodeCallbacks IO blk
          callbacks = NodeCallbacks {
              produceDRG   = drgNew
            , produceBlock = \proof _l slot prevPoint prevBlockNo txs -> do
                let curNo :: BlockNo
                    curNo = succ prevBlockNo

                    prevHash :: ChainHash blk
                    prevHash = castHash (Block.pointHash prevPoint)

                 -- The transactions we get are consistent; the only reason not
                 -- to include all of them would be maximum block size, which
                 -- we ignore for now.
                nodeForgeBlock pInfoConfig
                               slot
                               curNo
                               prevHash
                               txs
                               proof
          }

      varTip <- atomically $ newTVar GenesisPoint
      let chainDbArgs = mkChainDbArgs
                          pInfoConfig
                          pInfoInitLedger
                          registry
                          (CoreNodeId nid)
                          (withTip varTip $ tracerChainDB nodeTraces)
                          slotDuration
      chainDB :: ChainDB IO blk <- ChainDB.openDB chainDbArgs

      -- Watch the tip of the chain and store it in @varTip@ so we can include
      -- it in trace messages.
      onEachChange registry id GenesisPoint (ChainDB.getTipPoint chainDB)
        (atomically . writeTVar varTip)

      btime  <- realBlockchainTime registry slotDuration systemStart
      let nodeParams :: NodeParams IO Peer blk
          nodeParams = NodeParams
            { tracers            = showTracers $ withTip varTip
                                    (contramap show $ tracerConsensus nodeTraces)
            , threadRegistry     = registry
            , maxClockSkew       = ClockSkew 1
            , cfg                = pInfoConfig
            , initState          = pInfoInitState
            , btime
            , chainDB
            , callbacks
            , blockFetchSize     = nodeBlockFetchSize
            , blockMatchesHeader = nodeBlockMatchesHeader
            , maxUnackTxs        = 100 -- TODO
            }

      kernel <- nodeKernel nodeParams

      let networkApps :: NetworkApplication
                           IO Peer
                           ByteString ByteString ByteString
                           ByteString ByteString ()
          networkApps =
            consensusNetworkApps
              kernel
              (showProtocolTracers tracer)
              ProtocolCodecs
                { pcChainSyncCodec =
                    codecChainSync
                      (nodeEncodeHeader pInfoConfig)
                      (nodeDecodeHeader pInfoConfig)
                       encodePoint'
                       decodePoint'

                , pcBlockFetchCodec =
                    codecBlockFetch
                      (nodeEncodeBlock pInfoConfig)
                      (nodeEncodeHeaderHash (Proxy @blk))
                      (nodeDecodeBlock pInfoConfig)
                      (nodeDecodeHeaderHash (Proxy @blk))

                , pcTxSubmissionCodec =
                    codecTxSubmission
                      nodeEncodeGenTxId
                      nodeDecodeGenTxId
                      nodeEncodeGenTx
                      nodeDecodeGenTx

                , pcLocalChainSyncCodec =
                    codecChainSync
                      (nodeEncodeBlock pInfoConfig)
                      (nodeDecodeBlock pInfoConfig)
                       encodePoint'
                       decodePoint'

                , pcLocalTxSubmissionCodec =
                    codecLocalTxSubmission
                      nodeEncodeGenTx
                      nodeDecodeGenTx
                      Serialise.encode
                      Serialise.decode

                }
              (protocolHandlers nodeParams kernel)

      let networkAppNodeToNode :: Versions
                                    NodeToNodeVersion
                                    DictVersion
                                    (NetworkApplication
                                       IO Peer
                                       ByteString ByteString ByteString
                                       ByteString ByteString ())
          networkAppNodeToNode =
            simpleSingletonVersions
              NodeToNodeV_1
              (NodeToNodeVersionData { networkMagic = 0 })
              (DictVersion nodeToNodeCodecCBORTerm)
              networkApps

      let networkAppNodeToClient :: Versions
                                      NodeToClientVersion
                                      DictVersion
                                      (NetworkApplication
                                         IO Peer
                                         ByteString ByteString ByteString
                                         ByteString ByteString ())
          networkAppNodeToClient =
            simpleSingletonVersions
              NodeToClientV_1
              (NodeToClientVersionData { networkMagic = 0 })
              (DictVersion nodeToClientCodecCBORTerm)
              networkApps

      let myLocalSockPath = localSocketFilePath myNodeId
          myLocalAddr     = localSocketAddrInfo myLocalSockPath
      removeStaleLocalSocket myLocalSockPath

      -- serve local clients (including tx submission)
      localServer <-
        forkLinked registry $ do
          connTable <- newConnectionTable
          NodeToClient.withServer
            connTable
            myLocalAddr
            Peer
            (\(DictVersion _) -> acceptEq)
            (localResponderNetworkApplication <$> networkAppNodeToClient)
            wait

      -- serve downstream nodes
      connTable <- newConnectionTable
      peerServer <-
        forkLinked registry $ do
          NodeToNode.withServer
            connTable
            (nodeAddressInfo myNodeAddress)
            Peer
            (\(DictVersion _) -> acceptEq)
            (responderNetworkApplication <$> networkAppNodeToNode)
            wait

      let (ipProducers, dnsProducers) = partitionEithers (map (\ra -> maybe (Right ra) Left $ remoteAddressToNodeAddress ra) producers')

      ipSubscriptions <- forkLinked registry $
        ipSubscriptionWorker
          connTable
          (contramap show $ traceIpSubscription nodeTraces)
            -- the comments in dnsSbuscriptionWorker call apply
            (Just (Socket.SockAddrInet 0 0))
            (Just (Socket.SockAddrInet6 0 0 (0, 0, 0, 1) 0))
            (const Nothing)
            (IPSubscriptionTarget {
                ispIps = nodeAddressToSockAddr `map` ipProducers,
                ispValency = length ipProducers
              })
            (\sock -> do
              connectToNode'
                (\(DictVersion codec) -> encodeTerm codec)
                (\(DictVersion codec) -> decodeTerm codec)
                Peer
                (initiatorNetworkApplication <$> networkAppNodeToNode)
                sock)
            wait


      -- dns subscription managers
      dnsSubscriptions <- forM dnsProducers
        $ \RemoteAddress {raAddress, raPort, raValency} ->
        forkLinked registry $
          dnsSubscriptionWorker
            connTable
            (contramap show $ traceDnsSubscription nodeTraces)
            (contramap show $ traceDnsResolver nodeTraces)
            -- IPv4 address
            --
            -- We can't share portnumber with our server since we run separate
            -- 'MuxInitiatorApplication' and 'MuxResponderApplication'
            -- applications instead of a 'MuxInitiatorAndResponderApplication'.
            -- This means we don't utilise full duplex connection.
            (Just (Socket.SockAddrInet 0 0))
            -- IPv6 address
            (Just (Socket.SockAddrInet6 0 0 (0, 0, 0, 1) 0))
            (const Nothing)
            DnsSubscriptionTarget {
                dstDomain  = BSC.pack raAddress
              , dstPort    = raPort
              , dstValency = raValency
              }
            (\sock ->
              connectToNode'
                (\(DictVersion codec) -> encodeTerm codec)
                (\(DictVersion codec) -> decodeTerm codec)
                Peer
                (initiatorNetworkApplication <$> networkAppNodeToNode)
                sock)
            wait

      void $ Async.waitAny (localServer : peerServer : ipSubscriptions : dnsSubscriptions)

  where
      nid :: Int
      nid = case myNodeId of
              CoreId  n -> n
              RelayId _ -> error "Non-core nodes currently not supported"

      encodePoint' ::  Point blk -> Encoding
      encodePoint' =
          Block.encodePoint (nodeEncodeHeaderHash (Proxy @blk))

      decodePoint' :: forall s. Decoder s (Point blk)
      decodePoint' =
          Block.decodePoint (nodeDecodeHeaderHash (Proxy @blk))

removeStaleLocalSocket :: FilePath -> IO ()
removeStaleLocalSocket socketPath =
    removeFile socketPath
      `catch` \e ->
        if isDoesNotExistError e
          then return ()
          else throwIO e

mkChainDbArgs :: forall blk. (RunNode blk, TraceConstraints blk)
              => NodeConfig (BlockProtocol blk)
              -> ExtLedgerState blk
              -> ThreadRegistry IO
              -> CoreNodeId
              -> Tracer IO (ChainDB.TraceEvent blk)
              -> SlotLength
              -> ChainDB.ChainDbArgs IO blk
mkChainDbArgs cfg initLedger registry (CoreNodeId nid) tracer slotDuration =
    (ChainDB.defaultArgs dbPath)
      { ChainDB.cdbBlocksPerFile    = 10
      , ChainDB.cdbDecodeBlock      = nodeDecodeBlock       cfg
      , ChainDB.cdbDecodeChainState = nodeDecodeChainState  (Proxy @blk)
      , ChainDB.cdbDecodeHash       = nodeDecodeHeaderHash  (Proxy @blk)
      , ChainDB.cdbDecodeLedger     = nodeDecodeLedgerState cfg
      , ChainDB.cdbEncodeBlock      = nodeEncodeBlock       cfg
      , ChainDB.cdbEncodeChainState = nodeEncodeChainState  (Proxy @blk)
      , ChainDB.cdbEncodeHash       = nodeEncodeHeaderHash  (Proxy @blk)
      , ChainDB.cdbEncodeLedger     = nodeEncodeLedgerState cfg
      , ChainDB.cdbEpochSize        = nodeEpochSize         (Proxy @blk)
      , ChainDB.cdbGenesis          = return initLedger
      , ChainDB.cdbDiskPolicy       = defaultDiskPolicy secParam slotDiffTime
      , ChainDB.cdbIsEBB            = \blk -> if nodeIsEBB blk
                                              then Just (blockHash blk)
                                              else Nothing
      , ChainDB.cdbMemPolicy        = defaultMemPolicy secParam
      , ChainDB.cdbNodeConfig       = cfg
      , ChainDB.cdbThreadRegistry   = registry
      , ChainDB.cdbTracer           = tracer
      , ChainDB.cdbValidation       = ValidateMostRecentEpoch
      , ChainDB.cdbGcDelay          = secondsToDiffTime 10
      }
  where
    dbPath = "db-" <> show nid

    secParam = protocolSecurityParam cfg

    -- TODO cleaner way with subsecond precision
    slotDiffTime :: DiffTime
    slotDiffTime = secondsToDiffTime
      (slotLengthToMillisec slotDuration `div` 1000)

-- Converts the trace events from the ChainDB that we're interested in into
-- human-readable trace messages.
readableChainDBTracer
    :: forall m blk.
       (Monad m, Condense (HeaderHash blk), ProtocolLedgerView blk)
    => Tracer m String
    -> Tracer m (WithTip blk (ChainDB.TraceEvent blk))
readableChainDBTracer tracer = Tracer $ \case
    WithTip tip (ChainDB.TraceAddBlockEvent ev) -> case ev of
      ChainDB.StoreButDontChange   pt -> tr $ WithTip tip $
        "Ignoring block: " <> condense pt
      ChainDB.TryAddToCurrentChain pt -> tr $ WithTip tip $
        "Block fits onto the current chain: " <> condense pt
      ChainDB.TrySwitchToAFork pt _   -> tr $ WithTip tip $
        "Block fits onto some fork: " <> condense pt
      ChainDB.SwitchedToChain _ c     -> tr $ WithTip tip $
        "Chain changed, new tip: " <> condense (AF.headPoint c)
      ChainDB.AddBlockValidation ev' -> case ev' of
        ChainDB.InvalidBlock err pt -> tr $ WithTip tip $
          "Invalid block " <> condense pt <> ": " <> show err
        _ -> ignore
      _  -> ignore
    WithTip tip (ChainDB.TraceLedgerEvent ev) -> case ev of
      ChainDB.InitLog ev' -> traceInitLog tip ev'
      ChainDB.TookSnapshot snap pt -> tr $ WithTip tip $
        "Took ledger snapshot " <> show snap <> " at " <> condense pt
      ChainDB.DeletedSnapshot snap -> tr $ WithTip tip $
        "Deleted old snapshot " <> show snap
    WithTip tip (ChainDB.TraceCopyToImmDBEvent ev) -> case ev of
      ChainDB.CopiedBlockToImmDB pt -> tr $ WithTip tip $
        "Copied block " <> condense pt <> " to the ImmutableDB"
      _ -> ignore
    WithTip tip (ChainDB.TraceGCEvent ev) -> case ev of
      ChainDB.PerformedGC slot       -> tr $ WithTip tip $
        "Performed a garbage collection for " <> condense slot
      _ -> ignore
    WithTip tip (ChainDB.TraceOpenEvent ev) -> case ev of
      ChainDB.OpenedDB immTip tip' -> tr $ WithTip tip $
        "Opened with immutable tip at " <> condense immTip <>
        " and tip " <> condense tip'
      _ -> ignore
    _ -> ignore
  where
    tr :: Show a => WithTip blk a -> m ()
    tr = traceWith (contramap show tracer)

    ignore :: m ()
    ignore = return ()

    traceInitLog :: Point blk -> LedgerDB.InitLog (Point blk) -> m ()
    traceInitLog tip = \case
      LedgerDB.InitFromGenesis -> tr $ WithTip tip ("Initialised the ledger from genesis" :: String)
      LedgerDB.InitFromSnapshot snap tip' -> tr $ WithTip tip $
        "Initialised the ledger from snapshot " <> show snap <> " at " <>
        condense (tipToPoint tip')
      LedgerDB.InitFailure snap _failure initLog -> do
          tr $ WithTip tip $ "Snapshot " <> show snap <> " invalid"
          traceInitLog tip initLog

data Traces peer blk = Traces {
    -- | by default we use 'readableChainDB' tracer, if on this it will use more
    -- verbose tracer
    --
      tracerChainDB
      :: (Tracer IO(WithTip blk (ChainDB.TraceEvent blk)))

    -- | consensus tracer
    --
    -- TODO: it should be fixed with #839 ('ouroboros-network')
    --
    , tracerConsensus
      :: (Tracer IO String)

    -- | mempool tracer
    --
    , tracerMempool
      :: (Tracer IO (TraceEventMempool blk))

    -- | trace fetch decisions; it links to 'decisionTracer' in 'NodeParams'
    --

    , tracerFetchDecisions
      :: (Tracer IO [TraceLabelPeer peer (FetchDecision [Point (Header blk)])])

    -- | trace fetch client; it links to 'fetchClientTracer' in 'NodeParams'
    --
    , tracerFetchClient
      :: (Tracer IO (TraceLabelPeer peer (TraceFetchClientState (Header blk))))

    -- | trace tx-submission server; it link to 'txInboundTracer' in 'NodeParams'
    --
    , tracerTxInbound
      :: (Tracer IO (TraceTxSubmissionInbound  (GenTxId blk) (GenTx blk)))

    -- | trace tx-submission client; it link to 'txOutboundTracer' in 'NodeParams'
    --
    , tracerTxOutbound
      :: (Tracer IO (TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk)))

    -- | trace chain syn messages
    --
    , tracerChainSync
      :: (Tracer IO (TraceSendRecv (ChainSync (Header blk) (Point blk)) peer DeserialiseFailure))

    -- | trace tx submission messages
    --
    , tracerTxSubmission
      :: (Tracer IO (TraceSendRecv (BlockFetch blk) peer DeserialiseFailure))

    -- | trace ip subscription manager
    --
    -- TODO: export types in `ouroboros-network`
    , traceIpSubscription
      :: (Tracer IO String)

    -- | trace dns subscription manager
    --
    -- TODO: export types in `ouroboros-network`
    , traceDnsSubscription
      :: (Tracer IO String)

    -- | trace dns resolution
    --
    -- TODO: export types in `ouroboros-network`
    , traceDnsResolver
      :: (Tracer IO String)
    }

-- | Smart constructor of 'NodeTraces'.
--
getNodeTraces :: forall peer blk.
              ( ProtocolLedgerView blk
              , Show blk
              , Show (Header blk)
              , Condense (HeaderHash blk)
              , Show peer
              )
           => TraceOptions
           -> Tracer IO (LogObject Text)
           -> Traces peer blk
getNodeTraces traceOptions tracer = Traces
    { tracerChainDB
        = if traceChainDB traceOptions
            then contramap show tracer'
            else readableChainDBTracer tracer'
    , tracerConsensus
        = tracer'
    , tracerMempool
        = mempoolTraceTransformer tracer
    , tracerChainSync
        = enableTracer (traceChainSync traceOptions)
        $ withName "ChainSyncProtocol" tracer
    , tracerTxSubmission
        = enableTracer (traceTxSubmission traceOptions)
        $ withName "TxSubmissionProtocol" tracer
    , tracerFetchDecisions
        = enableTracer (traceFetchDecisions traceOptions)
        $ withName "FetchDecision" tracer
    , tracerFetchClient
        = enableTracer (traceFetchClient traceOptions)
        $ withName "FetchClient" tracer
    , tracerTxInbound
        = enableTracer (traceTxInbound traceOptions)
        $ withName "TxInbound" tracer
    , tracerTxOutbound
        = enableTracer (traceTxOutbound traceOptions)
        $ withName "TxOutbound" tracer
    , traceIpSubscription
        = withName "IPSubscription" tracer
    , traceDnsSubscription
        = withName "DNSSubscription" tracer
    , traceDnsResolver
        = withName "DNSResolver" tracer
    }
  where
    tracer' :: Tracer IO String
    tracer' = contramap pack $ toLogObject tracer

    enableTracer
      :: Show a
      => Bool
      -> Tracer IO String
      -> Tracer IO a
    enableTracer False = const nullTracer
    enableTracer True  = showTracing

    mempoolTraceTransformer :: Tracer IO (LogObject a)
                            -> Tracer IO (TraceEventMempool blk)
    mempoolTraceTransformer tr = Tracer $ \mempoolEvent -> do
        let logValue = LogValue "txsInMempool" $ PureI $ fromIntegral $ _txsInMempool mempoolEvent
        meta <- mkLOMeta Info Confidential
        traceNamedObject tr (meta, logValue)
        case mempoolEvent of
          TraceMempoolAddTxs      txs _ ->
              let logValue' = LogValue "txsProcessed" $ PureI $ fromIntegral $ length txs in
              traceNamedObject tr (meta, logValue')
          TraceMempoolRejectedTxs txs _ ->
              let logValue' = LogValue "txsProcessed" $ PureI $ fromIntegral $ length txs in
              traceNamedObject tr (meta, logValue')
          _                             -> return ()



--
-- Tracing utils
--

withName :: String
         -> Tracer IO (LogObject Text)
         -> Tracer IO String
withName name tr = contramap pack $ toLogObject $ appendName (pack name) tr

-- | Tracing wrapper which includes current tip in the logs (thus it requires
-- it from the context).
--
-- TODO: this should be moved to `ouroboros-consensus`.  Running in a seprate
-- STM transaction we risk reporting  wrong tip.
--
data WithTip blk a =
    WithTip
      (Point blk)
      -- ^ current tip point
      a
      -- ^ data

instance ( Show a
         , Condense (HeaderHash blk)
         ) => Show (WithTip blk a) where

    show (WithTip tip a) = case pointHash tip of
      GenesisHash -> "[genesis] " ++ show a
      BlockHash h -> mconcat [ "["
                             , take 7 (condense h)
                             , "] "
                             , show a
                             ]


-- | A way to satisfy tracer which requires current tip.  The tip is read from
-- a mutable cell.
--
withTip :: TVar IO (Point blk)
        -> Tracer IO (WithTip blk a)
        -> Tracer IO a
withTip varTip tr = Tracer $ \msg -> do
    tip <- atomically $ readTVar varTip
    traceWith (contramap (WithTip tip) tr) msg
