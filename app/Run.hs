{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Run (
      runNode
    ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import qualified Control.Concurrent.Async as Async
import           Control.Monad
import           Control.Tracer
import           Crypto.Random
import           Data.Functor.Contravariant (contramap)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Semigroup ((<>))

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.Chain (genesisPoint, pointHash)
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.Protocol.BlockFetch.Codec
import           Ouroboros.Network.Protocol.ChainSync.Codec

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.ChainSyncClient (ClockSkew (..))
import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Demo.Run
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Node
import           Ouroboros.Consensus.NodeNetwork
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.STM
import           Ouroboros.Consensus.Util.ThreadRegistry

import           Ouroboros.Storage.ChainDB (ChainDB)
import qualified Ouroboros.Storage.ChainDB as ChainDB
import qualified Ouroboros.Storage.ChainDB.Mock as ChainDB

import           CLI
import           Mock.TxSubmission
import           NamedPipe (DataFlow (..), NodeMapping ((:==>:)))
import qualified NamedPipe
import           Topology

runNode :: CLI -> IO ()
runNode cli@CLI{..} = do
    -- If the user asked to submit a transaction, we don't have to spin up a
    -- full node, we simply transmit it and exit.
    case command of
      TxSubmitter topology tx ->
        handleTxSubmission topology tx
      SimpleNode topology protocol -> do
        SomeProtocol p <- fromProtocol protocol
        handleSimpleNode p cli topology

-- | Sets up a simple node, which will run the chain sync protocol and block
-- fetch protocol, and, if core, will also look at the mempool when trying to
-- create a new block.
handleSimpleNode :: forall blk. RunDemo blk
                 => DemoProtocol blk -> CLI -> TopologyInfo -> IO ()
handleSimpleNode p CLI{..} (TopologyInfo myNodeId topologyFile) = do
    putStrLn $ "System started at " <> show systemStart
    t@(NetworkTopology nodeSetups) <-
      either error id <$> readTopologyFile topologyFile
    let topology  = toNetworkMap t
        nodeSetup = fromMaybe (error "node not found.") $
                          M.lookup myNodeId topology

    putStrLn $ "**************************************"
    putStrLn $ "I am Node = " <> show myNodeId
    putStrLn $ "My consumers are " <> show (consumers nodeSetup)
    putStrLn $ "My producers are " <> show (producers nodeSetup)
    putStrLn $ "**************************************"

    let pInfo@ProtocolInfo{..} =
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

      chainDB :: ChainDB IO blk (Header blk) <-
        ChainDB.openDB
          pInfoConfig
          pInfoInitLedger
          getHeader

      btime  <- realBlockchainTime registry slotDuration systemStart
      let tracer = contramap ((show myNodeId <> " | ") <>) stdoutTracer
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
      let network = initNetworkLayer nodeParams kernel

      watchChain registry tracer chainDB

      -- Spawn the thread which listens to the mempool.
      mempoolThread <- spawnMempoolListener tracer myNodeId kernel

      forM_ (producers nodeSetup) (addUpstream'   pInfo network)
      forM_ (consumers nodeSetup) (addDownstream' pInfo network)

      Async.wait mempoolThread
  where
      nid :: Int
      nid = case myNodeId of
              CoreId  n -> n
              RelayId _ -> error "Non-core nodes currently not supported"

      watchChain :: ThreadRegistry IO
                 -> Tracer IO String
                 -> ChainDB IO blk (Header blk)
                 -> IO ()
      watchChain registry tracer chainDB = onEachChange
          registry fingerprint initFingerprint
          (ChainDB.getCurrentChain chainDB) (const logFullChain)
        where
          initFingerprint  = (genesisPoint, genesisPoint)
          fingerprint frag = (AF.headPoint frag, AF.anchorPoint frag)
          logFullChain = do
            chain <- ChainDB.toChain chainDB
            traceWith tracer $
              "Updated chain: " <> condense (Chain.toOldestFirst chain)

      -- We need to make sure that both nodes read from the same file
      -- We therefore use the convention to distinguish between
      -- upstream and downstream from the perspective of the "lower numbered" node
      addUpstream' :: ProtocolInfo blk
                   -> NetworkProvides IO NodeId blk
                   -> NodeId
                   -> IO ()
      addUpstream' pInfo@ProtocolInfo{..} NetworkProvides{addUpstream}
                   producerNodeId =
          addUpstream producerNodeId nodeCommsCS nodeCommsBF
        where
          direction = Upstream (producerNodeId :==>: myNodeId)
          nodeCommsCS = NodeComms {
              ncCodec    = codecChainSync
                             (demoEncodeHeader     pInfoConfig)
                             (demoDecodeHeader     pInfoConfig)
                             (encodePoint'         pInfo)
                             (decodePoint'         pInfo)
            , ncWithChan = NamedPipe.withPipeChannel "chain-sync" direction
            }
          nodeCommsBF = NodeComms {
              ncCodec    = codecBlockFetch
                             (demoEncodeBlock pInfoConfig)
                             demoEncodeHeaderHash
                             (demoDecodeBlock pInfoConfig)
                             demoDecodeHeaderHash
            , ncWithChan = NamedPipe.withPipeChannel "block-fetch" direction
            }

      addDownstream' :: ProtocolInfo blk
                     -> NetworkProvides IO NodeId blk
                     -> NodeId
                     -> IO ()
      addDownstream' pInfo@ProtocolInfo{..} NetworkProvides{addDownstream}
                     consumerNodeId =
          addDownstream nodeCommsCS nodeCommsBF
        where
          direction = Downstream (myNodeId :==>: consumerNodeId)
          nodeCommsCS = NodeComms {
              ncCodec    = codecChainSync
                             (demoEncodeHeader     pInfoConfig)
                             (demoDecodeHeader     pInfoConfig)
                             (encodePoint'         pInfo)
                             (decodePoint'         pInfo)
            , ncWithChan = NamedPipe.withPipeChannel "chain-sync" direction
            }
          nodeCommsBF = NodeComms {
              ncCodec    = codecBlockFetch
                             (demoEncodeBlock pInfoConfig)
                             demoEncodeHeaderHash
                             (demoDecodeBlock pInfoConfig)
                             demoDecodeHeaderHash
            , ncWithChan = NamedPipe.withPipeChannel "block-fetch" direction
            }

      encodePoint' :: ProtocolInfo blk -> Point blk -> Encoding
      encodePoint' ProtocolInfo{..} =
          Block.encodePoint $ Block.encodeChainHash demoEncodeHeaderHash

      decodePoint' :: forall s. ProtocolInfo blk -> Decoder s (Point blk)
      decodePoint' ProtocolInfo{..} =
          Block.decodePoint $ Block.decodeChainHash demoDecodeHeaderHash
