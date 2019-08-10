{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

module Run (
      runNode
    ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
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
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.BackendKind (BackendKind (TraceForwarderBK))
import           Cardano.BM.Data.LogItem (LogObject (..))
import           Cardano.BM.Data.Tracer (ToLogObject (..))
import           Cardano.BM.Trace (appendName)
import           Cardano.Node.Configuration.Types (CardanoConfiguration (..))
import           Cardano.Node.Features.Logging (LoggingLayer (..))

import           Ouroboros.Network.Block
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.NodeToClient as NodeToClient
import           Ouroboros.Network.NodeToNode as NodeToNode
import           Ouroboros.Network.Socket
import           Ouroboros.Network.Subscription.Common
import           Ouroboros.Network.Subscription.Dns

import           Ouroboros.Network.Protocol.BlockFetch.Codec
import           Ouroboros.Network.Protocol.ChainSync.Codec
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.Protocol.LocalTxSubmission.Codec
import           Ouroboros.Network.Protocol.TxSubmission.Codec

import           Ouroboros.Consensus.Block (BlockProtocol)
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.ChainSyncClient (ClockSkew (..))
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState)
import           Ouroboros.Consensus.Node
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
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
import           Ouroboros.Storage.EpochInfo (EpochInfo, newEpochInfo)
import           Ouroboros.Storage.ImmutableDB (ValidationPolicy (..))
import           Ouroboros.Storage.LedgerDB.DiskPolicy (defaultDiskPolicy)
import           Ouroboros.Storage.LedgerDB.MemPolicy (defaultMemPolicy)

import           Cardano.Node.CLI
import           CLI hiding (TraceOptions (..))
import           Topology
import           TraceAcceptor
import           Tracers
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
        let tracers     = mkTracers traceOptions trace'
        case viewMode of
          SimpleView -> handleSimpleNode p nodeCli myNodeAddress topology trace' tracers cc
          LiveView   -> do
#ifdef UNIX
            let c = llConfiguration loggingLayer
            -- We run 'handleSimpleNode' as usual and run TUI thread as well.
            -- turn off logging to the console, only forward it through a pipe to a central logging process
            CM.setDefaultBackends c [TraceForwarderBK, UserDefinedBK "LiveViewBackend"]
            -- User will see a terminal graphics and will be able to interact with it.
            nodeThread <- Async.async $ handleSimpleNode p nodeCli myNodeAddress topology trace' tracers cc

            be :: LiveViewBackend Text <- realize c
            let lvbe = MkBackend { bEffectuate = effectuate be, bUnrealize = unrealize be }
            llAddBackend loggingLayer lvbe "LiveViewBackend"
            setTopology be topology
            setNodeThread be nodeThread
            captureCounters be tr

            void $ Async.waitAny [nodeThread]
#else
            handleSimpleNode p nodeCli myNodeAddress topology trace' tracers cc
#endif

-- | Sets up a simple node, which will run the chain sync protocol and block
-- fetch protocol, and, if core, will also look at the mempool when trying to
-- create a new block.
handleSimpleNode :: forall blk. (RunNode blk, TraceConstraints blk)
                 => Consensus.Protocol blk
                 -> NodeCLIArguments
                 -> NodeAddress
                 -> TopologyInfo
                 -> Tracer IO (LogObject Text)
                 -> Tracers Peer blk
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
      epochInfo <- newEpochInfo $ nodeEpochSize (Proxy @blk)
      let chainDbArgs = mkChainDbArgs
                          pInfoConfig
                          pInfoInitLedger
                          registry
                          (CoreNodeId nid)
                          (withTip varTip $ chainDBTracer nodeTraces)
                          slotDuration
                          epochInfo
      chainDB :: ChainDB IO blk <- ChainDB.openDB chainDbArgs

      -- Watch the tip of the chain and store it in @varTip@ so we can include
      -- it in trace messages.
      onEachChange registry id Nothing (ChainDB.getTipPoint chainDB)
        (atomically . writeTVar varTip)

      btime  <- realBlockchainTime registry slotDuration systemStart
      let nodeParams :: NodeParams IO Peer blk
          nodeParams = NodeParams
            { tracers            = consensusTracers nodeTraces
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
              nullProtocolTracers
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
                      (nodeEncodeApplyTxError (Proxy @blk))
                      (nodeDecodeApplyTxError (Proxy @blk))

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
          (contramap show $ ipSubscriptionTracer nodeTraces)
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
                nullTracer
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
            (contramap show $ dnsSubscriptionTracer nodeTraces)
            (contramap show $ dnsResolverTracer nodeTraces)
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
                nullTracer
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

mkChainDbArgs :: forall blk. RunNode blk
              => NodeConfig (BlockProtocol blk)
              -> ExtLedgerState blk
              -> ThreadRegistry IO
              -> CoreNodeId
              -> Tracer IO (ChainDB.TraceEvent blk)
              -> SlotLength
              -> EpochInfo IO
              -> ChainDB.ChainDbArgs IO blk
mkChainDbArgs cfg initLedger registry (CoreNodeId nid) tracer slotDuration epochInfo =
    (ChainDB.defaultArgs dbPath)
      { ChainDB.cdbBlocksPerFile    = 1000 --TODO: move definition of default
                                           -- elsewhere and just use it here.
      , ChainDB.cdbDecodeBlock      = nodeDecodeBlock       cfg
      , ChainDB.cdbDecodeChainState = nodeDecodeChainState  (Proxy @blk)
      , ChainDB.cdbDecodeHash       = nodeDecodeHeaderHash  (Proxy @blk)
      , ChainDB.cdbDecodeLedger     = nodeDecodeLedgerState cfg
      , ChainDB.cdbEncodeBlock      = nodeEncodeBlock       cfg
      , ChainDB.cdbEncodeChainState = nodeEncodeChainState  (Proxy @blk)
      , ChainDB.cdbEncodeHash       = nodeEncodeHeaderHash  (Proxy @blk)
      , ChainDB.cdbEncodeLedger     = nodeEncodeLedgerState cfg
      , ChainDB.cdbEpochInfo        = epochInfo
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
