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

import           Cardano.Prelude hiding (ByteString, atomically, take, trace)
import           Prelude (error, id, unlines)

#ifdef UNIX
import qualified Control.Concurrent.Async as Async
#endif
import           Control.Tracer
import qualified Data.ByteString.Char8 as BSC
import           Data.Either (partitionEithers)
import           Data.Functor.Contravariant (contramap)
import qualified Data.List as List
import           Data.Proxy (Proxy (..))
import           Data.Semigroup ((<>))
import           Data.Text (Text, breakOn, pack, take)
import           Network.HostName (getHostName)
import           System.Directory (canonicalizePath, makeAbsolute)

import           Control.Monad.Class.MonadSTM

#ifdef UNIX
import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.BackendKind (BackendKind (..))
#endif
import           Cardano.BM.Data.LogItem (LogObject (..))
import           Cardano.BM.Data.Tracer (ToLogObject (..),
                     TracingVerbosity (..), setHostname)
import           Cardano.Config.Logging (LoggingLayer (..))
import           Cardano.Config.Types (MiscellaneousFilepaths(..),
                                       NodeConfiguration (..), ViewMode (..))

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Node (NodeKernel (getChainDB),
                     ConnectionId (..), DiffusionTracers (..), DiffusionArguments (..),
                     DnsSubscriptionTarget (..), IPSubscriptionTarget (..),
                     RunNode (nodeNetworkMagic, nodeStartTime))
import qualified Ouroboros.Consensus.Node as Node (run)
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import qualified Ouroboros.Consensus.Protocol as Consensus
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.STM (onEachChange)

import qualified Ouroboros.Storage.ChainDB as ChainDB

import           Cardano.Common.LocalSocket
import           Cardano.Config.Protocol (SomeProtocol(..), fromProtocol)
import           Cardano.Config.Topology
import           Cardano.Config.Types (DbFile(..), NodeCLI(..),
                                       SocketFile(..), TopologyFile(..))
import           Cardano.Tracing.Tracers
#ifdef UNIX
import           Cardano.Node.TUI.LiveView
#endif


runNode
  :: LoggingLayer
  -> NodeConfiguration
  -> NodeCLI
  -> IO ()
runNode loggingLayer nc nCli = do
    hn <- hostname
    let !trace = setHostname hn $
                 llAppendName loggingLayer "node" (llBasicTrace loggingLayer)
    let tracer = contramap pack $ toLogObject trace

    traceWith tracer $ "tracing verbosity = " ++
                         case traceVerbosity $ traceOpts nCli of
                             NormalVerbosity -> "normal"
                             MinimalVerbosity -> "minimal"
                             MaximalVerbosity -> "maximal"
    SomeProtocol p  <- fromProtocol
                         (ncGenesisHash nc)
                         (ncNodeId nc)
                         (ncNumCoreNodes nc)
                         (genesisFile $ mscFp nCli)
                         (ncReqNetworkMagic nc)
                         (ncPbftSignatureThresh nc)
                         (delegCertFile $ mscFp nCli)
                         (signKeyFile $ mscFp nCli)
                         (ncUpdate nc)
                         (ncProtocol nc)

    let tracers     = mkTracers (traceOpts nCli) trace

    case ncViewMode nc of
      SimpleView -> handleSimpleNode p trace tracers nCli nc
      LiveView   -> do
#ifdef UNIX
        let c = llConfiguration loggingLayer
        -- We run 'handleSimpleNode' as usual and run TUI thread as well.
        -- turn off logging to the console, only forward it through a pipe to a central logging process
        CM.setDefaultBackends c [KatipBK, TraceForwarderBK, UserDefinedBK "LiveViewBackend"]
        -- User will see a terminal graphics and will be able to interact with it.
        nodeThread <- Async.async $ handleSimpleNode p trace tracers nCli nc

        be :: LiveViewBackend Text <- realize c
        let lvbe = MkBackend { bEffectuate = effectuate be, bUnrealize = unrealize be }
        llAddBackend loggingLayer lvbe (UserDefinedBK "LiveViewBackend")
        let nId = fromMaybe (panic "LiveView not possible for real protocols as yet") (ncNodeId nc)
        setTopology be nId
        setNodeThread be nodeThread
        captureCounters be trace

        void $ Async.waitAny [nodeThread]
#else
        handleSimpleNode p trace tracers nCli nc
#endif
  where
    hostname = do
      hn0 <- pack <$> getHostName
      return $ take 8 $ fst $ breakOn "." hn0

-- | Sets up a simple node, which will run the chain sync protocol and block
-- fetch protocol, and, if core, will also look at the mempool when trying to
-- create a new block.
handleSimpleNode :: forall blk. RunNode blk
                 => Consensus.Protocol blk
                 -> Tracer IO (LogObject Text)
                 -> Tracers ConnectionId blk
                 -> NodeCLI
                 -> NodeConfiguration
                 -> IO ()
handleSimpleNode p trace nodeTracers nCli nc = do
    NetworkTopology nodeSetups <-
      either error id <$> readTopologyFile (unTopology . topFile $ mscFp nCli)

    let pInfo@ProtocolInfo{ pInfoConfig = cfg } = protocolInfo p

    let tracer = contramap pack $ toLogObject trace
    traceWith tracer $
      "System started at " <> show (nodeStartTime (Proxy @blk) cfg)

    let producers' = case List.lookup nid $
                          map (\ns -> (nodeId ns, producers ns)) nodeSetups of
          Just ps -> ps
          Nothing -> error $ "handleSimpleNode: own address "
                          <> show (nodeAddr nCli)
                          <> ", Node Id "
                          <> show nid
                          <> " not found in topology"

    traceWith tracer $ unlines
      [ "**************************************"
      , "I am Node "        <> show (nodeAddr nCli) <> " Id: " <> show nid
      , "My producers are " <> show producers'
      , "**************************************"
      ]

    -- Socket directory
    myLocalAddr <- localSocketAddrInfo
                     (ncNodeId nc)
                     (unSocket . socketFile $ mscFp nCli)
                     MkdirIfMissing

    addrs <- nodeAddressInfo $ nodeAddr nCli
    let ipProducerAddrs  :: [NodeAddress]
        dnsProducerAddrs :: [RemoteAddress]
        (ipProducerAddrs, dnsProducerAddrs) = partitionEithers
          [ maybe (Right ra) Left $ remoteAddressToNodeAddress ra
          | ra <- producers' ]

        ipProducers :: IPSubscriptionTarget
        ipProducers =
          let ips = nodeAddressToSockAddr <$> ipProducerAddrs
          in IPSubscriptionTarget {
                ispIps     = ips,
                ispValency = length ips
              }
              
        dnsProducers :: [DnsSubscriptionTarget]
        dnsProducers = producerSubscription <$> dnsProducerAddrs
        
        producerSubscription :: RemoteAddress -> DnsSubscriptionTarget
        producerSubscription ra =
          DnsSubscriptionTarget
          { dstDomain  = BSC.pack (raAddress ra)
          , dstPort    = raPort ra
          , dstValency = raValency ra
          }

        diffusionTracers :: DiffusionTracers
        diffusionTracers = DiffusionTracers
          { dtIpSubscriptionTracer  = ipSubscriptionTracer  nodeTracers
          , dtDnsSubscriptionTracer = dnsSubscriptionTracer nodeTracers
          , dtDnsResolverTracer     = dnsResolverTracer     nodeTracers
          , dtErrorPolicyTracer     = errorPolicyTracer     nodeTracers
          , dtMuxTracer             = muxTracer             nodeTracers
          , dtMuxLocalTracer        = nullTracer
          , dtHandshakeTracer       = nullTracer
          , dtHandshakeLocalTracer  = nullTracer
          }

        diffusionArguments :: DiffusionArguments
        diffusionArguments = DiffusionArguments
          { daAddresses             = addrs
          , daLocalAddress          = myLocalAddr
          , daIpProducers           = ipProducers
          , daDnsProducers          = dnsProducers
          }

    removeStaleLocalSocket (ncNodeId nc) (unSocket . socketFile $ mscFp nCli)

    dbPath <- canonicalizePath =<< makeAbsolute (unDB . dBFile $ mscFp nCli)

    varTip <- atomically $ newTVar GenesisPoint

    Node.run
      (consensusTracers nodeTracers)
      (withTip varTip $ chainDBTracer nodeTracers)
      diffusionTracers
      diffusionArguments
      (nodeNetworkMagic (Proxy @blk) cfg)
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
      nid = case ncNodeId nc of
              Just (CoreId  n) -> n
              Just (RelayId _) -> error "Non-core nodes currently not supported"
              Nothing -> 999
