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
#ifdef UNIX
  , checkVRFFilePermissions
#endif
  ) where

import           Cardano.Prelude hiding (ByteString, atomically, take, trace)
import           Prelude (String)

import qualified Control.Concurrent.Async as Async
#ifdef UNIX
import           Control.Monad.Trans.Except.Extra (left)
#endif
import           Control.Tracer
import           Data.Either (partitionEithers)
import           Data.Functor.Contravariant (contramap)
import           Data.Maybe (catMaybes)
import           Data.Proxy (Proxy (..))
import           Data.Semigroup ((<>))
import           Data.Text (Text, breakOn, pack, take, unlines)
import qualified Data.Text as Text
import           Data.Version (showVersion)
import           GHC.Clock (getMonotonicTimeNSec)
import           Network.HostName (getHostName)
import           Network.Socket (AddrInfo, Socket)
import           System.Directory (canonicalizePath, createDirectoryIfMissing, makeAbsolute)
import           System.Environment (lookupEnv)
#ifdef UNIX
import           System.Posix.Files
import           System.Posix.Types (FileMode)
#endif

import           Cardano.BM.Data.Aggregated (Measurable (..))
import           Paths_cardano_node (version)
import           Cardano.BM.Data.LogItem (LOContent (..), PrivacyAnnotation (..), mkLOMeta)
import           Cardano.BM.Data.Tracer (ToLogObject (..), TracingVerbosity (..))
import           Cardano.BM.Data.Transformers (setHostname)
import           Cardano.BM.Trace

import qualified Cardano.Crypto.Libsodium as Crypto

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
                     NodeKernel, RunNode (..), RunNodeArgs (..))
import qualified Ouroboros.Consensus.Node as Node (getChainDB, run)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Network.BlockFetch (BlockFetchConfiguration (..))
import           Ouroboros.Network.Magic (NetworkMagic (..))
import           Ouroboros.Network.NodeToClient (LocalConnectionId)
import           Ouroboros.Network.NodeToNode (AcceptedConnectionsLimit (..), DiffusionMode,
                     RemoteConnectionId)

import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Storage.ImmutableDB (ValidationPolicy (..))
import           Ouroboros.Consensus.Storage.VolatileDB (BlockValidationPolicy (..))

import           Cardano.Node.Configuration.Socket (SocketOrSocketInfo (..),
                     gatherConfiguredSockets, getSocketOrSocketInfoAddr)
import           Cardano.Node.Configuration.Topology
import           Cardano.Node.Handlers.Shutdown
import           Cardano.Node.Protocol (SomeConsensusProtocol (..), mkConsensusProtocol,
                     renderProtocolInstantiationError)
import           Cardano.Tracing.Kernel
import           Cardano.Tracing.Peer
import           Cardano.Tracing.Tracers

{- HLINT ignore "Use fewer imports" -}

runNode
  :: PartialNodeConfiguration
  -> IO ()
runNode cmdPc = do
    -- TODO: Remove sodiumInit: https://github.com/input-output-hk/cardano-base/issues/175
    Crypto.sodiumInit

    configYamlPc <- parseNodeConfigurationFP . getLast $ pncConfigFile cmdPc

    nc <- case makeNodeConfiguration $ defaultPartialNodeConfiguration <> configYamlPc <> cmdPc of
            Left err -> panic $ "Error in creating the NodeConfiguration: " <> Text.pack err
            Right nc' -> return nc'

#ifdef UNIX
    case shelleyVRFFile $ ncProtocolFiles nc of
      Just vrfFp -> do vrf <- runExceptT $ checkVRFFilePermissions vrfFp
                       case vrf of
                         Left err ->
                           putTextLn (renderVRFPrivateKeyFilePermissionError err) >> exitFailure
                         Right () ->
                           pure ()
      Nothing -> pure ()
#endif

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

    upTimeThread <- Async.async $ traceNodeUpTime (appendName "metrics" trace) =<< getMonotonicTimeNSec

    -- This IORef contains node kernel structure which holds node kernel.
    -- Used for ledger queries and peer connection status.
    nodeKernelData :: NodeKernelData blk <- mkNodeKernelData

    tracers <- mkTracers (ncTraceConfig nc) trace nodeKernelData

    peersThread <- Async.async $ handlePeersListSimple trace nodeKernelData
    handleSimpleNode p trace tracers nc (setNodeKernel nodeKernelData)
    Async.uninterruptibleCancel upTimeThread
    Async.uninterruptibleCancel peersThread

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

  (publicIPv4SocketOrAddr
    , publicIPv6SocketOrAddr
    , localSocketOrPath) <- either throwIO return =<<
                           runExceptT (gatherConfiguredSockets nc)

  dbPath <- canonDbPath nc

  eitherTopology <- readTopologyFile nc
  nt <- either (\err -> panic $ "Cardano.Node.Run.handleSimpleNode.readTopologyFile: " <> err) pure eitherTopology

  let diffusionTracers :: DiffusionTracers
      diffusionTracers = createDiffusionTracers nodeTracers

      (ipProducerAddrs, dnsProducerAddrs) = producerAddresses nt

      dnsProducers :: [DnsSubscriptionTarget]
      dnsProducers = uncurry dnsSubscriptionTarget `map` dnsProducerAddrs

      ipProducers :: IPSubscriptionTarget
      ipProducers = ipSubscriptionTargets ipProducerAddrs

      diffusionArguments :: DiffusionArguments
      diffusionArguments =
        createDiffusionArguments
          publicIPv4SocketOrAddr
          publicIPv6SocketOrAddr
          localSocketOrPath
          (ncDiffusionMode nc)
          ipProducers
          dnsProducers

  ipv4 <- traverse getSocketOrSocketInfoAddr publicIPv4SocketOrAddr
  ipv6 <- traverse getSocketOrSocketInfoAddr publicIPv6SocketOrAddr
  traceWith tracer $ unlines
    [ ""
    , "**************************************"
    , "Addresses: "     <> show (catMaybes [ ipv4, ipv6 ])
    , "DiffusionMode: " <> show (ncDiffusionMode nc)
    , "DNS producers: " <> show dnsProducers
    , "IP producers: "  <> show ipProducers
    , "**************************************"
    ]

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
  createTracers NodeConfiguration { ncValidateDB }
                tr tracer cfg = do

         traceWith tracer $
           "System started at " <> show (getSystemStart $ Consensus.configBlock cfg)

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

#ifdef UNIX
-- | Make sure the VRF private key file is readable only
-- by the current process owner the node is running under.
checkVRFFilePermissions ::FilePath -> ExceptT VRFPrivateKeyFilePermissionError IO ()
checkVRFFilePermissions vrfPrivKey = do
  fs <- liftIO $ getFileStatus vrfPrivKey
  let fm = fileMode fs
  -- Check the the VRF private key file does not give read/write/exec permissions to others.
  when (hasOtherPermissions fm)
       (left $ OtherPermissionsExist vrfPrivKey)
  -- Check the the VRF private key file does not give read/write/exec permissions to any group.
  when (hasGroupPermissions fm)
       (left $ GroupPermissionsExist vrfPrivKey)
 where
  hasPermission :: FileMode -> FileMode -> Bool
  hasPermission fModeA fModeB = fModeA `intersectFileModes` fModeB /= nullFileMode

  hasOtherPermissions :: FileMode -> Bool
  hasOtherPermissions fm' = fm' `hasPermission` otherModes

  hasGroupPermissions :: FileMode -> Bool
  hasGroupPermissions fm' = fm' `hasPermission` groupModes
#endif


createDiffusionArguments
  :: Maybe (SocketOrSocketInfo Socket AddrInfo)
   -- ^ Either a socket bound to IPv4 address provided by systemd or IPv4
   -- address to bind to for NodeToNode communication.
  -> Maybe (SocketOrSocketInfo Socket AddrInfo)
   -- ^ Either a socket bound to IPv6 address provided by systemd or IPv6
   -- address to bind to for NodeToNode communication.
  -> SocketOrSocketInfo Socket SocketPath
  -- ^ Either a SOCKET_UNIX socket provided by systemd or a path for
  -- NodeToClient communication.
  -> DiffusionMode
  -> IPSubscriptionTarget
  -> [DnsSubscriptionTarget]
  -> DiffusionArguments
createDiffusionArguments publicIPv4SocketsOrAddrs
                         publicIPv6SocketsOrAddrs
                         localSocketOrPath
                         diffusionMode
                         ipProducers dnsProducers
                         =
  DiffusionArguments
    -- This is not elegant, but it will change once `coot/connection-manager` is
    -- merged into `ouroboros-networ`.
    { daIPv4Address = eitherSocketOrSocketInfo <$> publicIPv4SocketsOrAddrs
    , daIPv6Address = eitherSocketOrSocketInfo <$> publicIPv6SocketsOrAddrs
    , daLocalAddress = fmap unSocketPath
                     . eitherSocketOrSocketInfo
                     $ localSocketOrPath
    , daIpProducers  = ipProducers
    , daDnsProducers = dnsProducers
    -- TODO: these limits are arbitrary at the moment;
    -- issue: https://github.com/input-output-hk/ouroboros-network/issues/1836
    , daAcceptedConnectionsLimit = AcceptedConnectionsLimit {
        acceptedConnectionsHardLimit = 512
      , acceptedConnectionsSoftLimit = 384
      , acceptedConnectionsDelay     = 5
      }
    , daDiffusionMode = diffusionMode
    }
  where
    eitherSocketOrSocketInfo :: SocketOrSocketInfo a b -> Either a b
    eitherSocketOrSocketInfo (ActualSocket a) = Left a
    eitherSocketOrSocketInfo (SocketInfo b)   = Right b

dnsSubscriptionTarget :: NodeDnsAddress -> Int -> DnsSubscriptionTarget
dnsSubscriptionTarget na valency =
  DnsSubscriptionTarget { dstDomain  = nodeHostDnsAddressToDomain (naHostAddress na)
                        , dstPort    = naPort na
                        , dstValency = valency
                        }

ipSubscriptionTargets :: [NodeIPAddress] -> IPSubscriptionTarget
ipSubscriptionTargets ipProdAddrs =
  let ips = nodeAddressToSockAddr <$> ipProdAddrs
  in IPSubscriptionTarget { ispIps = ips
                          , ispValency = length ips
                          }


producerAddresses
  :: NetworkTopology
  -> ( [NodeIPAddress]
     , [(NodeDnsAddress, Int)])
producerAddresses nt =
  case nt of
    RealNodeTopology producers' ->
        partitionEithers
      . mapMaybe remoteAddressToNodeAddress
      $ producers'
    MockNodeTopology nodeSetup ->
        partitionEithers
      . mapMaybe remoteAddressToNodeAddress
      . concatMap producers
      $ nodeSetup
