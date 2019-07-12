{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

module Run (
      runNode
    ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.Serialise as Serialise (decode, encode)
import           Codec.SerialiseTerm
import qualified Control.Concurrent.Async as Async
import           Control.Exception
import           Control.Monad
import           Control.Tracer
import           Crypto.Random
import           Data.ByteString.Lazy (ByteString)
import           Data.Functor.Contravariant (contramap)
import qualified Data.List as List
import           Data.Semigroup ((<>))
import           Data.Text (Text, pack)
import           Network.Socket as Socket
import           System.Directory (removeFile)
import           System.IO.Error (isDoesNotExistError)

import           Control.Monad.Class.MonadAsync

import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.BackendKind (BackendKind (TraceForwarderBK))
import           Cardano.BM.Data.Tracer (ToLogObject (..))
import           Cardano.BM.Trace (appendName)
import           Cardano.Shell.Features.Logging (LoggingLayer (..))

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.Chain (genesisPoint)
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.NodeToClient as NodeToClient
import           Ouroboros.Network.NodeToNode as NodeToNode
import           Ouroboros.Network.Socket
import           Ouroboros.Network.Subscription.Common

import           Ouroboros.Network.Protocol.BlockFetch.Codec
import           Ouroboros.Network.Protocol.ChainSync.Codec
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.Protocol.LocalTxSubmission.Codec

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.ChainSyncClient (ClockSkew (..))
import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Demo.Run
import           Ouroboros.Consensus.Node
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.NodeNetwork
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.STM
import           Ouroboros.Consensus.Util.ThreadRegistry

import           Ouroboros.Storage.ChainDB (ChainDB)
import qualified Ouroboros.Storage.ChainDB as ChainDB hiding (openDB)
import qualified Ouroboros.Storage.ChainDB.Mock as ChainDB

import           Cardano.Node.CLI
import           CLI
import           LiveView
import           Topology
import           TraceAcceptor
import           TxSubmission


-- | Peer identifier used in consensus application
--
data Peer = Peer { localAddr  :: SockAddr
                 , remoteAddr :: SockAddr }
  deriving (Eq, Ord, Show)

instance Condense Peer where
    condense (Peer localAddr remoteAddr) = (show localAddr) ++ (show remoteAddr)


runNode :: NodeCLIArguments -> LoggingLayer -> IO ()
runNode nodeCli@NodeCLIArguments{..} loggingLayer = do
    let !tr = (llAppendName loggingLayer) "node" (llBasicTrace loggingLayer)
    -- If the user asked to submit a transaction, we don't have to spin up a
    -- full node, we simply transmit it and exit.
    case command of

      TxSubmitter topology tx protocol -> do
        let trace'      = appendName (pack (show (node topology))) tr
        let tracer      = contramap pack $ toLogObject trace'
        SomeProtocol p  <- fromProtocol protocol
        handleTxSubmission p topology tx tracer

      TraceAcceptor -> do
        let trace'      = appendName "acceptor" tr
        let tracer      = contramap pack $ toLogObject trace'
        handleTraceAcceptor tracer

      SimpleNode topology myNodeAddress protocol viewMode -> do
        let trace'      = appendName (pack $ show $ node topology) tr
        let tracer      = contramap pack $ toLogObject trace'
        SomeProtocol p  <- fromProtocol protocol
        case viewMode of
          SimpleView -> handleSimpleNode p nodeCli myNodeAddress topology tracer
          LiveView   -> do
#ifdef UNIX
            let c = llConfiguration loggingLayer
            -- We run 'handleSimpleNode' as usual and run TUI thread as well.
            -- turn off logging to the console, only forward it through a pipe to a central logging process
            CM.setDefaultBackends c [TraceForwarderBK, UserDefinedBK "LiveViewBackend"]
            -- User will see a terminal graphics and will be able to interact with it.
            nodeThread <- Async.async $ handleSimpleNode p nodeCli myNodeAddress topology tracer

            be :: LiveViewBackend Text <- realize c
            let lvbe = MkBackend { bEffectuate = effectuate be, bUnrealize = unrealize be }
            llAddBackend loggingLayer lvbe "LiveViewBackend"
            setTopology be topology
            captureCounters be tr

            _ <- Async.waitAny [nodeThread]
#endif
            return ()

-- | Sets up a simple node, which will run the chain sync protocol and block
-- fetch protocol, and, if core, will also look at the mempool when trying to
-- create a new block.
handleSimpleNode :: forall blk. RunDemo blk
                 => DemoProtocol blk
                 -> NodeCLIArguments
                 -> NodeAddress
                 -> TopologyInfo
                 -> Tracer IO String
                 -> IO ()
handleSimpleNode p NodeCLIArguments{..} myNodeAddress (TopologyInfo myNodeId topologyFile) tracer = do
    traceWith tracer $ "System started at " <> show systemStart
    NetworkTopology nodeSetups <-
      either error id <$> readTopologyFile topologyFile

    let producers' = case List.lookup myNodeAddress $ map (\ns -> (nodeAddress ns, producers ns)) nodeSetups of
          Just ps -> ps
          Nothing -> error "handleSimpleNode: own address not found in topology"

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
                demoForgeBlock pInfoConfig
                               slot
                               curNo
                               prevHash
                               txs
                               proof
          }

      chainDB :: ChainDB IO blk <-
        ChainDB.openDB
          pInfoConfig
          pInfoInitLedger

      btime  <- realBlockchainTime registry slotDuration systemStart
      let nodeParams :: NodeParams IO Peer blk
          nodeParams = NodeParams
            { tracer             = tracer
            , mempoolTracer      = contramap show tracer
            , decisionTracer     = nullTracer
            , fetchClientTracer  = nullTracer
            , threadRegistry     = registry
            , maxClockSkew       = ClockSkew 1
            , cfg                = pInfoConfig
            , initState          = pInfoInitState
            , btime
            , chainDB
            , callbacks
            , blockFetchSize     = demoBlockFetchSize
            , blockMatchesHeader = demoBlockMatchesHeader
            }

      kernel <- nodeKernel nodeParams

      let networkApps :: NetworkApplication
                           IO Peer
                           ByteString ByteString
                           ByteString ByteString ()
          networkApps =
            consensusNetworkApps
              nullTracer
              nullTracer
              kernel
              ProtocolCodecs
                { pcChainSyncCodec =
                    codecChainSync
                      (demoEncodeHeader pInfoConfig)
                      (demoDecodeHeader pInfoConfig)
                       encodePoint'
                       decodePoint'

                , pcBlockFetchCodec =
                    codecBlockFetch
                      (demoEncodeBlock pInfoConfig)
                      demoEncodeHeaderHash
                      (demoDecodeBlock pInfoConfig)
                      demoDecodeHeaderHash

                , pcLocalChainSyncCodec =
                    codecChainSync
                      (demoEncodeBlock pInfoConfig)
                      (demoDecodeBlock pInfoConfig)
                       encodePoint'
                       decodePoint'

                , pcLocalTxSubmissionCodec =
                    codecLocalTxSubmission
                      demoEncodeGenTx
                      demoDecodeGenTx
                      Serialise.encode
                      Serialise.decode

                }
              (protocolHandlers nodeParams kernel)

      let networkAppNodeToNode :: Versions
                                    NodeToNodeVersion
                                    DictVersion
                                    (NetworkApplication
                                       IO Peer
                                       ByteString ByteString
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
                                         ByteString ByteString
                                         ByteString ByteString ())
          networkAppNodeToClient =
            simpleSingletonVersions
              NodeToClientV_1
              (NodeToClientVersionData { networkMagic = 0 })
              (DictVersion nodeToClientCodecCBORTerm)
              networkApps

      watchChain registry tracer chainDB

      myAddr:_ <- case myNodeAddress of
        NodeAddress host port -> getAddrInfo Nothing (Just host) (Just port)

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
            (\(DictVersion _) -> acceptEq)
            (muxLocalResponderNetworkApplication <$> networkAppNodeToClient)
            wait

      -- serve downstream nodes
      connTable <- newConnectionTable
      peerServer <-
        forkLinked registry $ do
          NodeToNode.withServer
            connTable
            myAddr (\(DictVersion _) -> acceptEq)
            (muxResponderNetworkApplication <$> networkAppNodeToNode)
            wait

      -- ip subscription manager
      subManager <- forkLinked registry $
        ipSubscriptionWorker
          connTable
          (contramap show tracer)
          -- IPv4 address
          --
          -- We can't share portnumber with our server since we run separate
          -- 'MuxInitiatorApplication' and 'MuxResponderApplication'
          -- applications instead of a 'MuxInitiatorAndResponderApplication'.
          -- This means we don't utilise full duplex connection.
          (Just $ Socket.SockAddrInet 0 0)
          -- no IPv6 address
          Nothing
          (const Nothing)
          (IPSubscriptionTarget {
              ispIps     = map nodeAddressToSockAddr producers',
              ispValency = length producers'
            })
          (\sock -> do
              remoteAddr <- getPeerName sock
              localAddr  <- getSocketName sock
              connectToNode'
                      (\(DictVersion codec) -> encodeTerm codec)
                      (\(DictVersion codec) -> decodeTerm codec)
                      (muxInitiatorNetworkApplication
                          (Peer localAddr remoteAddr) <$> networkAppNodeToNode) sock)
          wait

      void $ Async.waitAny [localServer, peerServer, subManager]

  where

      nid :: Int
      nid = case myNodeId of
              CoreId  n -> n
              RelayId _ -> error "Non-core nodes currently not supported"

      watchChain :: ThreadRegistry IO
                 -> Tracer IO String
                 -> ChainDB IO blk
                 -> IO ()
      watchChain registry tracer' chainDB = onEachChange
          registry fingerprint initFingerprint
          (ChainDB.getCurrentChain chainDB) (const logFullChain)
        where
          initFingerprint  = (genesisPoint, genesisPoint)
          fingerprint frag = (AF.headPoint frag, AF.anchorPoint frag)
          logFullChain = do
            chain <- ChainDB.toChain chainDB
            traceWith tracer' $
              "Updated chain: " <> condense (Chain.toOldestFirst chain)

      encodePoint' ::  Point blk -> Encoding
      encodePoint' =
          Block.encodePoint demoEncodeHeaderHash

      decodePoint' :: forall s. Decoder s (Point blk)
      decodePoint' =
          Block.decodePoint demoDecodeHeaderHash


removeStaleLocalSocket :: FilePath -> IO ()
removeStaleLocalSocket socketPath =
    removeFile socketPath
      `catch` \e ->
        if isDoesNotExistError e
          then return ()
          else throwIO e
