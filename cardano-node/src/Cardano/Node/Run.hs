{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

module Cardano.Node.Run
  ( runNode
  ) where

import           Cardano.Prelude hiding (ByteString, atomically, take, trace)
import           Prelude (String)

import qualified Control.Concurrent.Async as Async
import           Control.Tracer
import qualified Data.ByteString.Char8 as BSC
import           Data.Text (breakOn, pack, take)
import qualified Data.Text as Text
import           Data.Version (showVersion)
import           GHC.Clock (getMonotonicTimeNSec)
import           Network.HostName (getHostName)
import           Network.Socket (AddrInfo, Socket)
import           System.Directory (canonicalizePath, createDirectoryIfMissing, makeAbsolute)
import           System.Environment (lookupEnv)

import           Cardano.BM.Data.Aggregated (Measurable (..))
import           Paths_cardano_node (version)
#ifdef UNIX
import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Data.Backend
#endif
import           Cardano.BM.Data.LogItem (LOContent (..), PrivacyAnnotation (..), mkLOMeta)
import           Cardano.BM.Data.Tracer (ToLogObject (..), TracingVerbosity (..))
import           Cardano.BM.Data.Transformers (setHostname)
import           Cardano.BM.Trace

import           Cardano.Config.Git.Rev (gitRev)
import           Cardano.Node.Configuration.Logging (LoggingLayer (..), Severity (..),
                     createLoggingLayer, shutdownLoggingLayer)
import           Cardano.Node.Configuration.POM (NodeConfiguration (..),
                     PartialNodeConfiguration (..), defaultPartialNodeConfiguration,
                     makeNodeConfiguration, ncProtocol, parseNodeConfigurationFP)
import           Cardano.Node.Types
import           Cardano.Tracing.Config (TraceOptions (..), TraceSelection (..))

import           Ouroboros.Consensus.Block (BlockProtocol)
import qualified Ouroboros.Consensus.Cardano as Consensus
import qualified Ouroboros.Consensus.Config as Consensus
import           Ouroboros.Consensus.Config.SupportsNode (ConfigSupportsNode (..))
import           Ouroboros.Consensus.Fragment.InFuture (defaultClockSkew)
import           Ouroboros.Consensus.Node (DiffusionArguments (..), DiffusionTracers (..),
                     DnsSubscriptionTarget (..), IPSubscriptionTarget (..), NodeArgs (..),
                     RunNode (..), RunNodeArgs (..))
import qualified Ouroboros.Consensus.Node as Node (getChainDB, run)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Network.BlockFetch (BlockFetchConfiguration (..))
import           Ouroboros.Network.Magic (NetworkMagic (..))
import           Ouroboros.Network.NodeToNode (AcceptedConnectionsLimit (..))

import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Storage.ImmutableDB (ValidationPolicy (..))
import           Ouroboros.Consensus.Storage.VolatileDB (BlockValidationPolicy (..))

import           Cardano.Node.Configuration.Socket (SocketOrSocketInfo (..),
                     gatherConfiguredSockets)
import           Cardano.Node.Configuration.Topology
import           Cardano.Node.Handlers.Shutdown
import           Cardano.Node.Protocol (SomeConsensusProtocol (..), mkConsensusProtocol,
                     renderProtocolInstantiationError)
import           Cardano.Tracing.Kernel
import           Cardano.Tracing.Peer
import           Cardano.Tracing.Tracers
#ifdef UNIX
import           Cardano.Node.Run.Trace (checkLiveViewRequiredTracers)
import           Cardano.Node.TUI.Run
#endif

{- HLINT ignore "Use fewer imports" -}

runNode
  :: PartialNodeConfiguration
  -> IO ()
runNode cmdPc = do

    configYamlPc <- parseNodeConfigurationFP . getLast $ pncConfigFile cmdPc

    nc <- case makeNodeConfiguration $ defaultPartialNodeConfiguration <> configYamlPc <> cmdPc of
            Left err -> panic $ "Error in creating the NodeConfiguration: " <> Text.pack err
            Right nc' -> return nc'

    eLoggingLayer <- runExceptT $ createLoggingLayer
                     (Text.pack (showVersion version))
                     nc

    loggingLayer <- case eLoggingLayer of
                      Left err -> putTextLn (show err) >> exitFailure
                      Right res -> return res

    !trace <- setupTrace loggingLayer
    let tracer = contramap pack $ toLogObject trace


    logTracingVerbosity nc tracer

    eitherSomeProtocol <- runExceptT $ mkConsensusProtocol nc

    SomeConsensusProtocol (p :: Consensus.Protocol IO blk (BlockProtocol blk)) <-
      case eitherSomeProtocol of
        Left err -> putTextLn (renderProtocolInstantiationError err) >> exitFailure
        Right (SomeConsensusProtocol p) -> pure $ SomeConsensusProtocol p

#ifdef UNIX
    let viewmode = ncViewMode nc
#else
    let viewmode = SimpleView
#endif

    upTimeThread <- Async.async $ traceNodeUpTime (appendName "metrics" trace) =<< getMonotonicTimeNSec

    -- This IORef contains node kernel structure which holds node kernel.
    -- Used for ledger queries and peer connection status.
    nodeKernelData :: NodeKernelData blk <- mkNodeKernelData

    tracers <- mkTracers (ncTraceConfig nc) trace nodeKernelData

    case viewmode of
      SimpleView -> do
        peersThread <- Async.async $ handlePeersListSimple trace nodeKernelData
        handleSimpleNode p trace tracers nc (setNodeKernel nodeKernelData)
        Async.uninterruptibleCancel upTimeThread
        Async.uninterruptibleCancel peersThread

      LiveView -> do
#ifdef UNIX
        let c = llConfiguration loggingLayer

        -- check required tracers are turned on
        checkLiveViewRequiredTracers (ncTraceConfig nc)

        -- We run 'handleSimpleNode' as usual and run TUI thread as well.
        -- turn off logging to the console, only forward it through a pipe to a central logging process
        CM.setDefaultBackends c [KatipBK, TraceForwarderBK, UserDefinedBK "LiveViewBackend"]

        be :: LiveViewBackend blk Text <- realize c
        let lvbe = MkBackend { bEffectuate = effectuate be, bUnrealize = unrealize be }
        llAddBackend loggingLayer lvbe (UserDefinedBK "LiveViewBackend")
        liveViewPostSetup be nc
        captureCounters be trace

        -- User will see a terminal graphics and will be able to interact with it.
        nodeThread <- Async.async $ handleSimpleNode p trace tracers nc
                       (setNodeKernel nodeKernelData)
        setNodeThread be nodeThread

        peersThread <- Async.async $ handlePeersList trace nodeKernelData be

        void $ Async.waitAny [nodeThread, upTimeThread, peersThread]
#else
        peersThread <- Async.async $ handlePeersListSimple trace nodeKernelData
        handleSimpleNode p trace tracers nc (const $ pure ())
        Async.uninterruptibleCancel upTimeThread
        Async.uninterruptibleCancel peersThread
#endif
    shutdownLoggingLayer loggingLayer

logTracingVerbosity :: NodeConfiguration -> Tracer IO String -> IO ()
logTracingVerbosity nc tracer =
  case ncTraceConfig nc of
    TracingOff -> return ()
    TracingOn traceConf ->
      case traceVerbosity traceConf of
        NormalVerbosity -> traceWith tracer "tracing verbosity = normal verbosity "
        MinimalVerbosity -> traceWith tracer "tracing verbosity = minimal verbosity "
        MaximalVerbosity -> traceWith tracer "tracing verbosity = maximal verbosity "

-- | Add the application name and unqualified hostname to the logging
-- layer basic trace.
--
-- If the @CARDANO_NODE_LOGGING_HOSTNAME@ environment variable is set,
-- it overrides the system hostname. This is useful when running a
-- local test cluster with all nodes on the same host.
setupTrace
  :: LoggingLayer
  -> IO (Trace IO Text)
setupTrace loggingLayer = do
    hn <- maybe hostname (pure . pack) =<< lookupEnv "CARDANO_NODE_LOGGING_HOSTNAME"
    return $
        setHostname hn $
        llAppendName loggingLayer "node" (llBasicTrace loggingLayer)
  where
    hostname = do
      hn0 <- pack <$> getHostName
      return $ take 8 $ fst $ breakOn "." hn0

-- | The node sends its real up time, every second.
traceNodeUpTime
  :: Trace IO Text
  -> Word64
  -> IO ()
traceNodeUpTime tr nodeLaunchTime = do
  now <- getMonotonicTimeNSec
  let upTimeInNs = now - nodeLaunchTime
  meta <- mkLOMeta Notice Public
  traceNamedObject tr (meta, LogValue "upTime" (Nanoseconds upTimeInNs))
  threadDelay 1000000
  traceNodeUpTime tr nodeLaunchTime

#ifdef UNIX
-- | Every 2 seconds get the current peers list and store it to LiveViewBackend
--   (if it's activated) and trace it (for example, for forwarding to an external process).
handlePeersList
  :: NFData a
  => Trace IO Text
  -> NodeKernelData blk
  -> LiveViewBackend blk a
  -> IO ()
handlePeersList tr nodeKern lvbe = forever $ do
  peers <- getCurrentPeers nodeKern
  storePeersInLiveView peers lvbe
  tracePeers tr peers
  threadDelay 2000000 -- 2 seconds.
#endif

handlePeersListSimple
  :: Trace IO Text
  -> NodeKernelData blk
  -> IO ()
handlePeersListSimple tr nodeKern = forever $ do
  getCurrentPeers nodeKern >>= tracePeers tr
  threadDelay 2000000 -- 2 seconds.

-- | Sets up a simple node, which will run the chain sync protocol and block
-- fetch protocol, and, if core, will also look at the mempool when trying to
-- create a new block.

handleSimpleNode
  :: forall blk. RunNode blk
  => Consensus.Protocol IO blk (BlockProtocol blk)
  -> Trace IO Text
  -> Tracers RemoteConnectionId LocalConnectionId blk
  -> NodeConfiguration
  -> (NodeKernel IO RemoteConnectionId LocalConnectionId blk -> IO ())
  -- ^ Called on the 'NodeKernel' after creating it, but before the network
  -- layer is initialised.  This implies this function must not block,
  -- otherwise the node won't actually start.
  -> IO ()
handleSimpleNode p trace nodeTracers nc onKernel = do

  let pInfo@ProtocolInfo{ pInfoConfig = cfg } = Consensus.protocolInfo p
      tracer = toLogObject trace

  createTracers nc trace tracer cfg

  (publicSocketsOrAddrs,
   localSocketOrPath) <- either throwIO return =<<
                           runExceptT (gatherConfiguredSockets nc)

  dbPath <- canonDbPath nc

  eitherTopology <- readTopologyFile nc

  nt <- either (\err -> panic $ "Cardano.Node.Run.handleSimpleNode.readTopologyFile: " <> err) pure eitherTopology

  let diffusionArguments :: DiffusionArguments
      diffusionArguments = createDiffusionArguments publicSocketsOrAddrs
                                                    localSocketOrPath
                                                    ipProducers dnsProducers
      diffusionTracers :: DiffusionTracers
      diffusionTracers = createDiffusionTracers nodeTracers
      dnsProducers :: [DnsSubscriptionTarget]
      dnsProducers = dnsSubscriptionTarget <$> dnsProducerAddrs
      ipProducers :: IPSubscriptionTarget
      ipProducers = ipSubscriptionTargets ipProducerAddrs
      (dnsProducerAddrs, ipProducerAddrs) = producerAddresses nt

  withShutdownHandling nc trace $ \sfds ->
   Node.run
     RunNodeArgs {
       rnTraceConsensus       = consensusTracers nodeTracers,
       rnTraceNTN             = nodeToNodeTracers nodeTracers,
       rnTraceNTC             = nodeToClientTracers nodeTracers,
       rnTraceDB              = chainDBTracer nodeTracers,
       rnTraceDiffusion       = diffusionTracers,
       rnDiffusionArguments   = diffusionArguments,
       rnNetworkMagic         = getNetworkMagic (Consensus.configBlock cfg),
       rnDatabasePath         = dbPath,
       rnProtocolInfo         = pInfo,
       rnCustomiseChainDbArgs = customiseChainDbArgs $ ncValidateDB nc,
       rnCustomiseNodeArgs    = customiseNodeArgs (ncMaxConcurrencyBulkSync nc)
                                  (ncMaxConcurrencyDeadline nc),
       rnNodeToNodeVersions   = supportedNodeToNodeVersions (Proxy @blk),
       rnNodeToClientVersions = supportedNodeToClientVersions (Proxy @blk),
       rnNodeKernelHook       = \registry nodeKernel -> do
         maybeSpawnOnSlotSyncedShutdownHandler nc sfds trace registry
           (Node.getChainDB nodeKernel)
         onKernel nodeKernel,
       rnMaxClockSkew         = defaultClockSkew
    }
 where
  customiseNodeArgs :: Maybe MaxConcurrencyBulkSync
                    -> Maybe MaxConcurrencyDeadline
                    -> NodeArgs IO RemoteConnectionId LocalConnectionId blk
                    -> NodeArgs IO RemoteConnectionId LocalConnectionId blk
  customiseNodeArgs bulk_m deadline_m args@NodeArgs{ blockFetchConfiguration } = args {
      blockFetchConfiguration = blockFetchConfiguration {
          bfcMaxConcurrencyBulkSync = maybe (bfcMaxConcurrencyBulkSync blockFetchConfiguration)
            unMaxConcurrencyBulkSync bulk_m
        , bfcMaxConcurrencyDeadline = maybe (bfcMaxConcurrencyDeadline blockFetchConfiguration)
            unMaxConcurrencyDeadline deadline_m
        }
      }

  customiseChainDbArgs :: Bool
                       -> ChainDB.ChainDbArgs Identity IO blk
                       -> ChainDB.ChainDbArgs Identity IO blk
  customiseChainDbArgs runValid args
    | runValid
    = args
      { ChainDB.cdbImmutableDbValidation = ValidateAllChunks
      , ChainDB.cdbVolatileDbValidation = ValidateAll
      }
    | otherwise
    = args

  createDiffusionTracers :: Tracers RemoteConnectionId LocalConnectionId blk
                         -> DiffusionTracers
  createDiffusionTracers nodeTracers' = DiffusionTracers
    { dtIpSubscriptionTracer = ipSubscriptionTracer nodeTracers'
    , dtDnsSubscriptionTracer = dnsSubscriptionTracer nodeTracers'
    , dtDnsResolverTracer = dnsResolverTracer nodeTracers'
    , dtErrorPolicyTracer = errorPolicyTracer nodeTracers'
    , dtLocalErrorPolicyTracer = localErrorPolicyTracer nodeTracers'
    , dtAcceptPolicyTracer = acceptPolicyTracer nodeTracers'
    , dtMuxTracer = muxTracer nodeTracers'
    , dtMuxLocalTracer = nullTracer
    , dtHandshakeTracer = handshakeTracer nodeTracers'
    , dtHandshakeLocalTracer = localHandshakeTracer nodeTracers'
    }

  createTracers
    :: NodeConfiguration
    -> Trace IO Text
    -> Tracer IO Text
    -> Consensus.TopLevelConfig blk
    -> IO ()
  createTracers ncf@NodeConfiguration{ncNodeAddr, ncValidateDB}
                tr tracer cfg = do
         eitherTopology <- readTopologyFile ncf
         nt <- either
                 (\err -> panic $ "Cardano.Node.Run.createTracers.readTopologyFile: " <> err)
                 pure
                 eitherTopology

         let (dnsProducerAddrs, ipProducerAddrs) = producerAddresses nt

         traceWith tracer $
           "System started at " <> show (getSystemStart $ Consensus.configBlock cfg)

         traceWith tracer $ unlines
           [ ""
           , "**************************************"
           , "Host node address: " <> show ncNodeAddr
           , "My DNS producers are " <> show dnsProducerAddrs
           , "My IP producers are " <> show ipProducerAddrs
           , "**************************************"
           ]

         meta <- mkLOMeta Notice Public
         let rTr = appendName "release" tr
             nTr = appendName "networkMagic" tr
             vTr = appendName "version" tr
             cTr = appendName "commit"  tr
         traceNamedObject rTr (meta, LogMessage . Text.pack . protocolName $ ncProtocol nc)
         traceNamedObject nTr (meta, LogMessage ("NetworkMagic " <> show (unNetworkMagic . getNetworkMagic $ Consensus.configBlock cfg)))
         traceNamedObject vTr (meta, LogMessage . pack . showVersion $ version)
         traceNamedObject cTr (meta, LogMessage gitRev)

         when ncValidateDB $ traceWith tracer "Performing DB validation"

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

canonDbPath :: NodeConfiguration -> IO FilePath
canonDbPath NodeConfiguration{ncDatabaseFile = DbFile dbFp} = do
  fp <- canonicalizePath =<< makeAbsolute dbFp
  createDirectoryIfMissing True fp
  return fp

createDiffusionArguments
  :: SocketOrSocketInfo [Socket] [AddrInfo]
   -- ^ Either a list of sockets provided by systemd or addresses to bind to
   -- for NodeToNode communication.
  -> SocketOrSocketInfo Socket SocketPath
  -- ^ Either a SOCKET_UNIX socket provided by systemd or a path for
  -- NodeToClient communication.
  -> IPSubscriptionTarget
  -> [DnsSubscriptionTarget]
  -> DiffusionArguments
createDiffusionArguments publicSocketsOrAddrs localSocketOrPath
                         ipProducers dnsProducers =
  DiffusionArguments
    { daAddresses    = case publicSocketsOrAddrs of
                         ActualSocket sockets -> Left sockets
                         SocketInfo addrs     -> Right addrs
    , daLocalAddress = case localSocketOrPath of
                        ActualSocket socket          -> Left socket
                        SocketInfo (SocketPath path) -> Right path
    , daIpProducers  = ipProducers
    , daDnsProducers = dnsProducers
    -- TODO: these limits are arbitrary at the moment;
    -- issue: https://github.com/input-output-hk/ouroboros-network/issues/1836
    , daAcceptedConnectionsLimit = AcceptedConnectionsLimit {
        acceptedConnectionsHardLimit = 512
      , acceptedConnectionsSoftLimit = 384
      , acceptedConnectionsDelay     = 5
      }
    }

dnsSubscriptionTarget :: RemoteAddress -> DnsSubscriptionTarget
dnsSubscriptionTarget ra =
  DnsSubscriptionTarget { dstDomain  = BSC.pack (raAddress ra)
                        , dstPort    = raPort ra
                        , dstValency = raValency ra
                        }

ipSubscriptionTargets :: [NodeAddress] -> IPSubscriptionTarget
ipSubscriptionTargets ipProdAddrs =
  let ips = nodeAddressToSockAddr <$> ipProdAddrs
  in IPSubscriptionTarget { ispIps = ips
                          , ispValency = length ips
                          }

producerAddresses :: NetworkTopology -> ([RemoteAddress], [NodeAddress])
producerAddresses nt =
  case nt of
    RealNodeTopology producers' -> partitionEithers $ map remoteOrNode producers'
    MockNodeTopology nodeSetup ->
      partitionEithers . map remoteOrNode $ concatMap producers nodeSetup
 where
   remoteOrNode :: RemoteAddress -> Either RemoteAddress NodeAddress
   remoteOrNode ra = case remoteAddressToNodeAddress ra of
                       Just na -> Right na
                       Nothing -> Left ra
