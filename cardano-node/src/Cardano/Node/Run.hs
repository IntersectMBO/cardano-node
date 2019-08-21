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

module Cardano.Node.Run
  ( runNode
  , NodeCLIArguments(..)
  , NodeCommand(..)
  , ViewMode(..)
  )
where

import           Cardano.Prelude hiding (ByteString, atomically, throwIO, trace,
                                  wait)
import           Prelude (error, id, unlines)

import qualified Control.Concurrent.Async as Async
import           Control.Exception
import           Control.Tracer
import qualified Data.ByteString.Char8 as BSC
import           Data.Either (partitionEithers)
import           Data.Functor.Contravariant (contramap)
import qualified Data.List as List
import           Data.Proxy (Proxy (..))
import           Data.Semigroup ((<>))
import           Data.Text (Text, pack)
import           Network.Socket as Socket
import           System.Directory (canonicalizePath, createDirectoryIfMissing, makeAbsolute, removeFile)
import           System.FilePath ((</>))
import           System.IO.Error (isDoesNotExistError)

import           Control.Monad.Class.MonadSTM

import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.BackendKind (BackendKind (TraceForwarderBK))
import           Cardano.BM.Data.LogItem (LogObject (..))
import           Cardano.BM.Data.Tracer (ToLogObject (..),
                                         TracingVerbosity (..))
import           Cardano.BM.Trace (appendName)
import           Cardano.Node.Configuration.Types (CardanoConfiguration (..))
import           Cardano.Node.Features.Logging (LoggingLayer (..))

import           Ouroboros.Network.Block
import           Ouroboros.Network.Subscription.Dns

import           Ouroboros.Consensus.BlockchainTime (SlotLength(..))
import qualified Ouroboros.Consensus.Ledger.Mock as Mock
import           Ouroboros.Consensus.Node (NodeKernel (getChainDB),
                                           RunNetworkArgs (..),
                                           RunNode (nodeStartTime))
import qualified Ouroboros.Consensus.Node as Node (run)
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import qualified Ouroboros.Consensus.Protocol as Consensus
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.STM (onEachChange)

import qualified Ouroboros.Storage.ChainDB as ChainDB

import           Cardano.Common.CommonCLI (CommonCLI)
import           Cardano.Common.Protocol (Protocol(..), SomeProtocol(..), fromProtocol)
import           Cardano.Node.Topology
import           Cardano.Node.TraceAcceptor
import           Cardano.Node.Tracers
import           Cardano.Node.TxSubmission
#ifdef UNIX
import           Cardano.Node.LiveView
#endif


-- | Peer identifier used in consensus application
--
data Peer = Peer { localAddr  :: SockAddr
                 , remoteAddr :: SockAddr }
  deriving (Eq, Ord, Show)

instance Condense Peer where
    condense (Peer localAddr remoteAddr) = (show localAddr) ++ (show remoteAddr)

data NodeCLIArguments = NodeCLIArguments {
    slotDuration :: !SlotLength
  , commonCLI    :: !CommonCLI
  , command      :: !NodeCommand
  }

data NodeCommand =
    SimpleNode  TopologyInfo NodeAddress Protocol ViewMode TraceOptions
  | TxSubmitter TopologyInfo Mock.Tx     Protocol
  | TraceAcceptor

-- Node can be run in two modes.
data ViewMode =
    LiveView    -- Live mode with TUI
  | SimpleView  -- Simple mode, just output text.

runNode :: NodeCLIArguments -> LoggingLayer -> CardanoConfiguration -> IO ()
runNode nodeCli@NodeCLIArguments{..} loggingLayer cc = do
    let !tr = llAppendName loggingLayer "node" (llBasicTrace loggingLayer)
    -- If the user asked to submit a transaction, we don't have to spin up a
    -- full node, we simply transmit it and exit.
    case command of

      TraceAcceptor -> do
        let trace'      = appendName "acceptor" tr
        let tracer      = contramap pack $ toLogObject trace'
        handleTraceAcceptor tracer

      SimpleNode topology myNodeAddress protocol viewMode traceOptions -> do
        let trace'      = appendName (pack $ show $ node topology) tr
        let tracer      = contramap pack $ toLogObject trace'

        traceWith tracer $ "tracing verbosity = " ++
                             case traceVerbosity traceOptions of
                                 NormalVerbosity  -> "normal"
                                 MinimalVerbosity -> "minimal"
                                 MaximalVerbosity -> "maximal"
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
handleSimpleNode :: forall blk. RunNode blk
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
                 nodeTracers
                 CardanoConfiguration{..} = do
    NetworkTopology nodeSetups <-
      either error id <$> readTopologyFile topologyFile

    let pInfo@ProtocolInfo{ pInfoConfig = cfg } =
          protocolInfo (NumCoreNodes (length nodeSetups)) (CoreNodeId nid) p

    let tracer = contramap pack $ toLogObject trace
    traceWith tracer $
      "System started at " <> show (nodeStartTime (Proxy @blk) cfg)

    let producers' = case List.lookup myNodeAddress $
                          map (\ns -> (nodeAddress ns, producers ns)) nodeSetups of
          Just ps -> ps
          Nothing -> error $ "handleSimpleNode: own address "
                          <> show myNodeAddress
                          <> " not found in topology"

    traceWith tracer $ unlines
      [ "**************************************"
      , "I am Node "        <> show myNodeAddress
      , "My producers are " <> show producers'
      , "**************************************"
      ]

    -- Socket directory argument
    socketDir <- canonicalizePath =<< makeAbsolute ccSocketPath
    createDirectoryIfMissing True socketDir

    let ipProducerAddrs  :: [NodeAddress]
        dnsProducerAddrs :: [RemoteAddress]
        (ipProducerAddrs, dnsProducerAddrs) = partitionEithers
          [ maybe (Right ra) Left $ remoteAddressToNodeAddress ra
          | ra <- producers' ]

        ipProducers :: [SockAddr]
        ipProducers = nodeAddressToSockAddr <$> ipProducerAddrs

        dnsProducers :: [DnsSubscriptionTarget]
        dnsProducers =
          [ DnsSubscriptionTarget
              { dstDomain  = BSC.pack raAddress
              , dstPort    = raPort
              , dstValency = raValency
              }
          | RemoteAddress {raAddress, raPort, raValency} <- dnsProducerAddrs ]

        myLocalSockPath :: FilePath
        myLocalSockPath = socketDir </> localSocketFilePath myNodeId

        myLocalAddr :: AddrInfo
        myLocalAddr = localSocketAddrInfo myLocalSockPath

        runNetworkArgs :: RunNetworkArgs Peer blk
        runNetworkArgs = RunNetworkArgs
          { rnaIpSubscriptionTracer  = showTracing $ ipSubscriptionTracer  nodeTracers
          , rnaDnsSubscriptionTracer = showTracing $ dnsSubscriptionTracer nodeTracers
          , rnaDnsResolverTracer     = showTracing $ dnsResolverTracer     nodeTracers
          , rnaMkPeer                = Peer
          , rnaMyAddr                = nodeAddressInfo myNodeAddress
          , rnaMyLocalAddr           = myLocalAddr
          , rnaIpProducers           = ipProducers
          , rnaDnsProducers          = dnsProducers
          }

    removeStaleLocalSocket myLocalSockPath

    dbPath <- canonicalizePath =<< makeAbsolute ccDBPath

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
        onEachChange registry id Nothing (ChainDB.getTipPoint chainDB) $ \_ tip ->
          atomically $ writeTVar varTip tip
  where
      nid :: Int
      nid = case myNodeId of
              CoreId  n -> n
              RelayId _ -> error "Non-core nodes currently not supported"

removeStaleLocalSocket :: FilePath -> IO ()
removeStaleLocalSocket socketPath =
    removeFile socketPath
      `catch` \e ->
        if isDoesNotExistError e
          then return ()
          else throwIO e
