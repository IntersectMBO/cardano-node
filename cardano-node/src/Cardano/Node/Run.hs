{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

module Cardano.Node.Run
  ( runNode
  , ViewMode(..)
  )
where

import           Cardano.Prelude hiding (ByteString, atomically, trace)
import           Prelude (error, id, unlines)

import qualified Control.Concurrent.Async as Async
import           Control.Tracer
import qualified Data.ByteString.Char8 as BSC
import           Data.Either (partitionEithers)
import           Data.Functor.Contravariant (contramap)
import qualified Data.List as List
import           Data.Proxy (Proxy (..))
import           Data.Semigroup ((<>))
import           Data.Text (Text, pack)
import           Network.Socket as Socket
import           System.Directory (canonicalizePath, makeAbsolute)

import           Control.Monad.Class.MonadSTM

import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.BackendKind (BackendKind (TraceForwarderBK))
import           Cardano.BM.Data.LogItem (LogObject (..))
import           Cardano.BM.Data.Tracer (ToLogObject (..),
                     TracingVerbosity (..), setHostname)
import           Cardano.Config.Logging (LoggingLayer (..))
import           Cardano.Config.Types (CardanoConfiguration (..), ViewMode (..))

import           Ouroboros.Network.Block
import           Ouroboros.Network.Subscription.Dns

import           Ouroboros.Consensus.Node (NodeKernel (getChainDB),
                     RunNetworkArgs (..),
                     RunNode (nodeNetworkMagic, nodeStartTime))
import qualified Ouroboros.Consensus.Node as Node (run)
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import qualified Ouroboros.Consensus.Protocol as Consensus
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.STM (onEachChange)

import qualified Ouroboros.Storage.ChainDB as ChainDB

import           Cardano.Common.LocalSocket
import           Cardano.Config.Protocol (SomeProtocol(..), fromProtocol)
import           Cardano.Config.Topology
import           Cardano.Tracing.Tracers
#ifdef UNIX
import           Cardano.Node.TUI.LiveView
#endif


-- | Peer identifier used in consensus application
--
data Peer = Peer { localAddr  :: SockAddr
                 , remoteAddr :: SockAddr }
  deriving (Eq, Ord, Show)

instance Condense Peer where
    condense (Peer localA remoteA) = (show localA) ++ (show remoteA)

instance NoUnexpectedThunks Peer where
    showTypeOf _ = "Peer"
    whnfNoUnexpectedThunks _ctxt _act = return NoUnexpectedThunks


runNode
  :: LoggingLayer
  -> CardanoConfiguration
  -> IO ()
runNode loggingLayer cc = do
    let !trace = setHostname (pack $ show $ node $ ccTopologyInfo cc) $
                 llAppendName loggingLayer "node" (llBasicTrace loggingLayer)
    let tracer = contramap pack $ toLogObject trace

    traceWith tracer $ "tracing verbosity = " ++
                         case traceVerbosity $ ccTraceOptions cc of
                             NormalVerbosity -> "normal"
                             MinimalVerbosity -> "minimal"
                             MaximalVerbosity -> "maximal"
    SomeProtocol p  <- fromProtocol cc $ ccProtocol cc

    let tracers     = mkTracers (ccTraceOptions cc) trace

    case ccViewMode cc of
      SimpleView -> handleSimpleNode p trace tracers cc
      LiveView   -> do
#ifdef UNIX
        let c = llConfiguration loggingLayer
        -- We run 'handleSimpleNode' as usual and run TUI thread as well.
        -- turn off logging to the console, only forward it through a pipe to a central logging process
        CM.setDefaultBackends c [TraceForwarderBK, UserDefinedBK "LiveViewBackend"]
        -- User will see a terminal graphics and will be able to interact with it.
        nodeThread <- Async.async $ handleSimpleNode p trace tracers cc

        be :: LiveViewBackend Text <- realize c
        let lvbe = MkBackend { bEffectuate = effectuate be, bUnrealize = unrealize be }
        llAddBackend loggingLayer lvbe (UserDefinedBK "LiveViewBackend")
        setTopology be $ ccTopologyInfo cc
        setNodeThread be nodeThread
        captureCounters be trace

        void $ Async.waitAny [nodeThread]
#else
        handleSimpleNode p trace tracers cc
#endif

-- | Sets up a simple node, which will run the chain sync protocol and block
-- fetch protocol, and, if core, will also look at the mempool when trying to
-- create a new block.
handleSimpleNode :: forall blk. RunNode blk
                 => Consensus.Protocol blk
                 -> Tracer IO (LogObject Text)
                 -> Tracers Peer blk
                 -> CardanoConfiguration
                 -> IO ()
handleSimpleNode p trace nodeTracers cc = do
    NetworkTopology nodeSetups <-
      either error id <$> readTopologyFile (topologyFile $ ccTopologyInfo cc)

    let pInfo@ProtocolInfo{ pInfoConfig = cfg } =
          protocolInfo (NumCoreNodes (length nodeSetups)) (CoreNodeId nid) p

    let tracer = contramap pack $ toLogObject trace
    traceWith tracer $
      "System started at " <> show (nodeStartTime (Proxy @blk) cfg)

    let producers' = case List.lookup nid $
                          map (\ns -> (nodeId ns, producers ns)) nodeSetups of
          Just ps -> ps
          Nothing -> error $ "handleSimpleNode: own address "
                          <> show (ccNodeAddress cc)
                          <> ", Node Id "
                          <> show nid
                          <> " not found in topology"

    traceWith tracer $ unlines
      [ "**************************************"
      , "I am Node "        <> show (ccNodeAddress cc) <> " Id: " <> show nid
      , "My producers are " <> show producers'
      , "**************************************"
      ]

    -- Socket directory
    myLocalAddr <- localSocketAddrInfo (node $ ccTopologyInfo cc) (ccSocketDir cc) MkdirIfMissing

    addrs <- nodeAddressInfo $ ccNodeAddress cc
    let ipProducerAddrs  :: [NodeAddress]
        dnsProducerAddrs :: [RemoteAddress]
        (ipProducerAddrs, dnsProducerAddrs) = partitionEithers
          [ maybe (Right ra) Left $ remoteAddressToNodeAddress ra
          | ra <- producers' ]

        ipProducers :: [SockAddr]
        ipProducers = nodeAddressToSockAddr <$> ipProducerAddrs

        dnsProducers :: [DnsSubscriptionTarget]
        dnsProducers = producerSubscription <$> dnsProducerAddrs

        runNetworkArgs :: RunNetworkArgs Peer blk
        runNetworkArgs = RunNetworkArgs
          { rnaIpSubscriptionTracer  = ipSubscriptionTracer  nodeTracers
          , rnaDnsSubscriptionTracer = dnsSubscriptionTracer nodeTracers
          , rnaDnsResolverTracer     = dnsResolverTracer     nodeTracers
          , rnaMuxTracer             = muxTracer             nodeTracers
          , rnaMuxLocalTracer        = nullTracer
          , rnaMkPeer                = Peer
          , rnaMyAddrs               = addrs
          , rnaMyLocalAddr           = myLocalAddr
          , rnaIpProducers           = ipProducers
          , rnaDnsProducers          = dnsProducers
          , rnaHandshakeTracer       = nullTracer
          , rnaHandshakeLocalTracer  = nullTracer
          , rnaNetworkMagic          = nodeNetworkMagic (Proxy @blk) cfg
          }

        producerSubscription :: RemoteAddress -> DnsSubscriptionTarget
        producerSubscription ra =
          DnsSubscriptionTarget
          { dstDomain  = BSC.pack (raAddress ra)
          , dstPort    = raPort ra
          , dstValency = raValency ra
          }

    removeStaleLocalSocket (node $ ccTopologyInfo cc) (ccSocketDir cc)

    dbPath <- canonicalizePath =<< makeAbsolute (ccDBPath cc)

    varTip <- atomically $ newTVar GenesisPoint

    Node.run
      (consensusTracers nodeTracers)
      (withTip varTip $ chainDBTracer nodeTracers)
      runNetworkArgs
      (dbPath <> "-" <> show nid)
      pInfo
      id -- No ChainDbArgs customisation
      id -- No NodeParams customisation
      $ \registry nodeKernel -> do
        -- Watch the tip of the chain and store it in @varTip@ so we can include
        -- it in trace messages.
        let chainDB = getChainDB nodeKernel
        onEachChange registry id Nothing (ChainDB.getTipPoint chainDB) $ \tip ->
          atomically $ writeTVar varTip tip
  where
      nid :: Int
      nid = case node $ ccTopologyInfo cc of
              CoreId  n -> n
              RelayId _ -> error "Non-core nodes currently not supported"
