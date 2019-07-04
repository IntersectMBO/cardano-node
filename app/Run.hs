{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Run (
      runNode
    ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.Serialise as Serialise (decode, encode)
import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import           Control.Exception
import           Control.Monad
import           Control.Tracer
import           Crypto.Random
import           Data.ByteString.Lazy (ByteString)
import           Data.Functor.Contravariant (contramap)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Semigroup ((<>))
import           Data.Text (Text, pack)
import           Network.Socket as Socket
import           System.Directory (removeFile)
import           System.IO.Error (isDoesNotExistError)

import           Control.Monad.Class.MonadAsync

import           Cardano.BM.Data.Tracer (ToLogObject (..))
import           Cardano.BM.Trace (Trace, appendName)

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.Chain (genesisPoint, pointHash)
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.NodeToClient as NodeToClient
import           Ouroboros.Network.NodeToNode as NodeToNode

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
import qualified Ouroboros.Storage.ChainDB as ChainDB
import qualified Ouroboros.Storage.ChainDB.Mock as ChainDB

import           Cardano.Node.CLI
import           CLI
import           LiveView
import           Topology
import           TraceAcceptor
import           TxSubmission

runNode :: NodeCLIArguments -> Trace IO Text -> IO ()
runNode nodeCli@NodeCLIArguments{..} trace = do
    -- If the user asked to submit a transaction, we don't have to spin up a
    -- full node, we simply transmit it and exit.
    case command of

      TxSubmitter topology tx protocol -> do
        trace'          <- appendName (pack (show (node topology))) trace
        let tracer      = contramap pack $ toLogObject trace'
        SomeProtocol p  <- fromProtocol protocol
        handleTxSubmission p topology tx tracer

      TraceAcceptor -> do
        trace'          <- appendName "acceptor" trace
        let tracer      = contramap pack $ toLogObject trace'
        handleTraceAcceptor tracer

      SimpleNode topology myNodeAddress protocol viewMode -> do
        trace'          <- appendName (pack $ show $ node topology) trace
        let tracer      = contramap pack $ toLogObject trace'
        SomeProtocol p  <- fromProtocol protocol
        case viewMode of
          SimpleView -> handleSimpleNode p nodeCli myNodeAddress topology tracer
          LiveView   -> do
            -- We run 'handleSimpleNode' as usual and run TUI thread as well.
            -- User will see a terminal graphics and will be able to interact with it.
            nodeThread <- Async.async $ handleSimpleNode p nodeCli myNodeAddress topology tracer
            tuiThread  <- Async.async $ runNodeLiveView topology
            _ <- Async.waitAny [nodeThread, tuiThread]
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
    t@(NetworkTopology nodeSetups) <-
      either error id <$> readTopologyFile topologyFile
    let topology  = toNetworkMap t
        nodeSetup = fromMaybe (error "node not found.") $
                          M.lookup myNodeId topology

    traceWith tracer $ "**************************************"
    traceWith tracer $ "I am Node = " <> show myNodeId
    traceWith tracer $ "My producers are " <> show (producers nodeSetup)
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
                    prevHash = castHash (pointHash prevPoint)

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
      let nodeParams :: NodeParams IO NodeAddress blk
          nodeParams = NodeParams
            { tracer             = tracer
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
                           IO NodeAddress
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
                                       IO NodeAddress
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
                                         IO NodeAddress
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

      -- TODO: cheap subscription managment, a proper one is on the way.  The
      -- point is that it only requires 'NetworkApplications' which is a thin
      -- layer around 'MuxApplication'.

      -- serve local clients (including tx submission)
      localServer <-
        forkLinked registry $
          NodeToClient.withServer
            myLocalAddr
            (\(DictVersion _) -> acceptEq)
            (muxLocalResponderNetworkApplication <$> networkAppNodeToClient)
            wait

      -- serve downstream nodes
      peerServer <-
        forkLinked registry $
          NodeToNode.withServer
            myAddr (\(DictVersion _) -> acceptEq)
            (muxResponderNetworkApplication <$> networkAppNodeToNode)
            wait

      -- connect to upstream nodes
      forM_ (producers nodeSetup) $ \na@(NodeAddress host port) ->
        forkLinked registry $ do

          let io = do
                addr:_ <- getAddrInfo Nothing (Just host) (Just port)
                NodeToNode.connectTo
                      (muxInitiatorNetworkApplication na <$> networkAppNodeToNode)
                      -- Do not bind to a local port, use ephemeral
                      -- one.  We cannot bind to port on which the server is
                      -- already accepting connections.
                      Nothing
                      addr
                  -- TODO: this is purposefully simple, a proper DNS management
                  -- is on the way in PR #607
                  `catch` \(_ :: IOException) -> threadDelay 250_000 >> io

          io

      _ <- Async.waitAny [localServer, peerServer]
      return ()
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
          Block.encodePoint $ Block.encodeChainHash demoEncodeHeaderHash

      decodePoint' :: forall s. Decoder s (Point blk)
      decodePoint' =
          Block.decodePoint $ Block.decodeChainHash demoDecodeHeaderHash


removeStaleLocalSocket :: FilePath -> IO ()
removeStaleLocalSocket socketPath =
    removeFile socketPath
      `catch` \e ->
        if isDoesNotExistError e
          then return ()
          else throwIO e
