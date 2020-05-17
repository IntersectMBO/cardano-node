{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

module Cardano.Node.Run
  ( runNode
  )
where

import           Cardano.Prelude hiding (ByteString, atomically, take, trace)
import           Prelude (error)

import qualified Control.Concurrent.Async as Async
import           Control.Tracer
import qualified Data.ByteString.Char8 as BSC
import           Data.Either (partitionEithers)
import           Data.Functor.Contravariant (contramap)
import           Data.IORef (IORef, newIORef)
import qualified Data.List as List (lookup, null)
import           Data.Proxy (Proxy (..))
import           Data.Semigroup ((<>))
import           Data.Text (Text, breakOn, pack, take, unlines)
import           GHC.Clock (getMonotonicTimeNSec)
import           Data.Version (showVersion)
import           Network.HostName (getHostName)
import           Network.Socket (AddrInfo, Socket)
import           System.Directory (canonicalizePath, makeAbsolute)
#ifdef SYSTEMD
import           System.Systemd.Daemon (getActivatedSockets)
#endif

import           Paths_cardano_node (version)
import           Cardano.BM.Data.Aggregated (Measurable (..))
#ifdef UNIX
import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.BackendKind (BackendKind (..))
#endif
import           Cardano.BM.Data.LogItem (LOContent (..),
                     PrivacyAnnotation (..), mkLOMeta)
import           Cardano.BM.Data.Tracer (ToLogObject (..),
                     TracingVerbosity (..))
import           Cardano.BM.Data.Transformers (setHostname)
import           Cardano.BM.Trace

import           Cardano.Config.GitRev (gitRev)
import           Cardano.Config.Logging (LoggingLayer (..), Severity (..))
import           Cardano.Config.TraceConfig (traceBlockFetchDecisions,
                     traceChainDB, traceConfigVerbosity, traceForge,
                     traceMempool, traceEnabled)
import           Cardano.Config.Types (NodeConfiguration (..), ViewMode (..))

import           Ouroboros.Network.Magic (NetworkMagic (..))
import           Ouroboros.Network.NodeToClient (LocalConnectionId)
import           Ouroboros.Network.NodeToNode (RemoteConnectionId, AcceptedConnectionsLimit (..))
import           Ouroboros.Consensus.Block (BlockProtocol)
import           Ouroboros.Consensus.Node (NodeKernel,
                     DiffusionTracers (..), DiffusionArguments (..),
                     DnsSubscriptionTarget (..), IPSubscriptionTarget (..),
                     RunNode (nodeNetworkMagic, nodeStartTime),
                     RunNodeArgs (..))
import qualified Ouroboros.Consensus.Node as Node (getChainDB, run)
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Fragment.InFuture (defaultClockSkew)
import qualified Ouroboros.Consensus.Config as Consensus
import qualified Ouroboros.Consensus.Cardano as Consensus
import           Ouroboros.Consensus.Util.Orphans ()


import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Storage.ImmutableDB (ValidationPolicy (..))
import           Ouroboros.Consensus.Storage.VolatileDB (BlockValidationPolicy (..))

import           Cardano.Config.Protocol
                   (SomeConsensusProtocol(..), mkConsensusProtocol,
                    renderProtocolInstantiationError)
import           Cardano.Config.Topology
import           Cardano.Config.Types
import           Cardano.Node.LocalSocket
                   (nodeLocalSocketAddrInfo, removeStaleLocalSocket)
import           Cardano.Node.Shutdown
import           Cardano.Tracing.Tracers
#ifdef UNIX
import           Cardano.Node.TUI.LiveView
#endif


runNode
  :: LoggingLayer
  -> NodeCLI
  -> IO ()
runNode loggingLayer npm@NodeCLI{protocolFiles} = do
    hn <- hostname
    let !trace = setHostname hn $
                 llAppendName loggingLayer "node" (llBasicTrace loggingLayer)
    let tracer = contramap pack $ toLogObject trace

    nc <- parseNodeConfiguration npm

    traceWith tracer $ "tracing verbosity = " ++
                         case traceConfigVerbosity $ ncTraceConfig nc of
                           NormalVerbosity -> "normal"
                           MinimalVerbosity -> "minimal"
                           MaximalVerbosity -> "maximal"
    eitherSomeProtocol <- runExceptT $ mkConsensusProtocol nc (Just protocolFiles)

    SomeConsensusProtocol (p :: Consensus.Protocol blk (BlockProtocol blk)) <-
      case eitherSomeProtocol of
        Left err -> putTextLn (renderProtocolInstantiationError err) >> exitFailure
        Right (SomeConsensusProtocol p) -> pure $ SomeConsensusProtocol p

    bcCounters :: IORef BlockchainCounters <- newIORef initialBlockchainCounters

    tracers <- mkTracers (ncTraceConfig nc) trace bcCounters

#ifdef UNIX
    let viewmode = ncViewMode nc
#else
    let viewmode = SimpleView
#endif

    upTimeThread <- Async.async $ traceNodeUpTime (appendName "metrics" trace) =<< getMonotonicTimeNSec

    case viewmode of
      SimpleView -> do

        handleSimpleNode p trace tracers npm (const $ pure ())
        Async.uninterruptibleCancel upTimeThread

      LiveView   -> do
#ifdef UNIX
        let c = llConfiguration loggingLayer
        -- check required tracers are turned on
        let reqtrs = [("TraceBlockFetchDecisions",traceBlockFetchDecisions), ("TraceChainDb",traceChainDB), ("TraceForge",traceForge), ("TraceMempool",traceMempool)]
            trsinactive = filter (\(_,f) -> not $ traceEnabled (ncTraceConfig nc) f) reqtrs
        unless (List.null trsinactive) $ do
            putTextLn "for full functional 'LiveView', please turn on the following tracers in the configuration file:"
            forM_ trsinactive $ \(m, _) ->
                putTextLn $ m <> " : True"
            putTextLn "     (press enter to continue)"
            _ <- getLine
            pure ()

        -- We run 'handleSimpleNode' as usual and run TUI thread as well.
        -- turn off logging to the console, only forward it through a pipe to a central logging process
        CM.setDefaultBackends c [KatipBK, TraceForwarderBK, UserDefinedBK "LiveViewBackend"]

        be :: LiveViewBackend blk Text <- realize c
        let lvbe = MkBackend { bEffectuate = effectuate be, bUnrealize = unrealize be }
        llAddBackend loggingLayer lvbe (UserDefinedBK "LiveViewBackend")
        liveViewPostSetup be npm nc
        captureCounters be trace

        -- User will see a terminal graphics and will be able to interact with it.
        nodeThread <- Async.async $ handleSimpleNode p trace tracers npm
                       (setNodeKernel be)
        setNodeThread be nodeThread

        void $ Async.waitAny [nodeThread, upTimeThread]
#else
        handleSimpleNode p trace tracers npm (const $ pure ())
        Async.uninterruptibleCancel upTimeThread
#endif
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

-- | Sets up a simple node, which will run the chain sync protocol and block
-- fetch protocol, and, if core, will also look at the mempool when trying to
-- create a new block.

handleSimpleNode
  :: forall blk. RunNode blk
  => Consensus.Protocol blk (BlockProtocol blk)
  -> Trace IO Text
  -> Tracers RemoteConnectionId LocalConnectionId blk
  -> NodeCLI
  -> (NodeKernel IO RemoteConnectionId LocalConnectionId blk -> IO ())
  -- ^ Called on the 'NodeKernel' after creating it, but before the network
  -- layer is initialised.  This implies this function must not block,
  -- otherwise the node won't actually start.
  -> IO ()
handleSimpleNode p trace nodeTracers npm onKernel = do

  let pInfo@ProtocolInfo{ pInfoConfig = cfg } = Consensus.protocolInfo p
      tracer = toLogObject trace

  -- Node configuration
  nc <- parseNodeConfiguration npm

  createTracers npm nc trace tracer cfg

  sds_m <- systemdSockets

  addrs <- case sds_m of
                Nothing      -> Right <$> nodeAddressInfo npm
                Just (_,[])  -> Right <$> nodeAddressInfo npm
                Just (_,sds) -> return $ Left sds

  dbPath <- canonDbPath npm

  eitherTopology <- readTopologyFile npm

  nt <- either (\err -> panic $ "Cardano.Node.Run.readTopologyFile: " <> err) pure eitherTopology

  let myLocalAddr = case sds_m of
                      Nothing     -> Right $ nodeLocalSocketAddrInfo nc npm
                      Just (sd,_) -> Left sd

  let diffusionArguments :: DiffusionArguments
      diffusionArguments = createDiffusionArguments addrs myLocalAddr ipProducers dnsProducers
      diffusionTracers :: DiffusionTracers
      diffusionTracers = createDiffusionTracers nodeTracers
      dnsProducers :: [DnsSubscriptionTarget]
      dnsProducers = dnsSubscriptionTarget <$> dnsProducerAddrs
      ipProducers :: IPSubscriptionTarget
      ipProducers = ipSubscriptionTargets ipProducerAddrs
      (dnsProducerAddrs, ipProducerAddrs) = producerAddresses nt

  removedStaleSocket <- runExceptT $ removeStaleLocalSocket nc npm
  case removedStaleSocket of
    Left err   -> putTextLn (show err) >> exitFailure
    Right addr -> return addr

  withShutdownHandling npm trace $ \sfds ->
   Node.run
     RunNodeArgs {
       rnTraceConsensus       = consensusTracers nodeTracers,
       rnTraceNTN             = nodeToNodeTracers nodeTracers,
       rnTraceNTC             = nodeToClientTracers nodeTracers,
       rnTraceDB              = chainDBTracer nodeTracers,
       rnTraceDiffusion       = diffusionTracers,
       rnDiffusionArguments   = diffusionArguments,
       rnNetworkMagic         = nodeNetworkMagic cfg,
       rnDatabasePath         = dbPath,
       rnProtocolInfo         = pInfo,
       rnCustomiseChainDbArgs = customiseChainDbArgs $ validateDB npm,
       rnCustomiseNodeArgs    = identity,
       rnNodeToNodeVersions   = supportedNodeToNodeVersions (Proxy @blk),
       rnNodeToClientVersions = supportedNodeToClientVersions (Proxy @blk),
       rnNodeKernelHook       = \registry nodeKernel -> do
         maybeSpawnOnSlotSyncedShutdownHandler npm sfds trace registry
           (Node.getChainDB nodeKernel)
         onKernel nodeKernel,
       rnMaxClockSkew         = defaultClockSkew
    }
 where
  customiseChainDbArgs :: Bool
                       -> ChainDB.ChainDbArgs IO blk
                       -> ChainDB.ChainDbArgs IO blk
  customiseChainDbArgs runValid args
    | runValid
    = args
      { ChainDB.cdbImmValidation = ValidateAllChunks
      , ChainDB.cdbVolValidation = ValidateAll
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
    :: NodeCLI
    -> NodeConfiguration
    -> Trace IO Text
    -> Tracer IO Text
    -> Consensus.TopLevelConfig blk
    -> IO ()
  createTracers npm'@NodeCLI{nodeMode = RealProtocolMode, nodeAddr, validateDB}
                nc tr tracer cfg = do
         eitherTopology <- readTopologyFile npm'
         nt <- either
                 (\err -> panic $ "Cardano.Node.Run.readTopologyFile: " <> err)
                 pure
                 eitherTopology

         let (dnsProducerAddrs, ipProducerAddrs) = producerAddresses nt

         traceWith tracer $
           "System started at " <> show (nodeStartTime cfg)

         traceWith tracer $ unlines
           [ ""
           , "**************************************"
           , "Host node address: " <> show nodeAddr
           , "My DNS producers are " <> show dnsProducerAddrs
           , "My IP producers are " <> show ipProducerAddrs
           , "**************************************"
           ]

         meta <- mkLOMeta Notice Public
         let rTr = appendName "release" tr
             vTr = appendName "version" tr
             cTr = appendName "commit"  tr
         traceNamedObject rTr (meta, LogMessage (show (ncProtocol nc)))
         traceNamedObject rTr (meta, LogMessage ("NetworkMagic " <> show (unNetworkMagic $ nodeNetworkMagic cfg)))
         traceNamedObject vTr (meta, LogMessage . pack . showVersion $ version)
         traceNamedObject cTr (meta, LogMessage gitRev)

         when validateDB $ traceWith tracer "Performing DB validation"

  --TODO: there's still lots of duplication here, when only minor things are
  -- different
  createTracers npm'@NodeCLI{nodeMode = MockProtocolMode, nodeAddr, validateDB}
                _nc _tr tracer cfg = do
         eitherTopology <- readTopologyFile npm'
         nodeid <- nid npm'
         (MockNodeTopology nodeSetups) <- either
                                            (\err -> panic $ "Cardano.Node.Run.readTopologyFile: " <> err)
                                            pure
                                            eitherTopology

         traceWith tracer $ "System started at " <> show (nodeStartTime cfg)
         let producersList = map (\ns -> (nodeId ns, producers ns)) nodeSetups
             producers' = case (List.lookup nodeid producersList) of
                            Just ps ->  ps
                            Nothing -> error $ "handleSimpleNode: own address "
                                         <> show nodeAddr
                                         <> ", Node Id "
                                         <> show nodeid
                                         <> " not found in topology"

         traceWith tracer $ unlines
                               [ ""
                               , "**************************************"
                               , "I am Node "        <> show nodeAddr
                                          <> " Id: " <> show nodeid
                               , "My producers are " <> show producers'
                               , "**************************************"
                               ]

         when validateDB $ traceWith tracer "Performing DB validation"

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

canonDbPath :: NodeCLI -> IO FilePath
canonDbPath npm@NodeCLI{databaseFile, nodeMode} = do
  dbFp <- case nodeMode of
            MockProtocolMode -> do
              --TODO: we should eliminate auto-naming here too
              nodeid <- nid npm
              pure $ unDB databaseFile <> "-" <> show nodeid

            RealProtocolMode -> pure (unDB databaseFile)

  canonicalizePath =<< makeAbsolute dbFp

createDiffusionArguments
  :: Either [Socket] [AddrInfo]
   -- ^ Either a list of sockets provided by systemd or addresses to bind to
   -- for NodeToNode communication.
  -> Either Socket FilePath
  -- ^ Either a SOCKET_UNIX socket provided by systemd or a path for
  -- NodeToClient communication.
  -> IPSubscriptionTarget
  -> [DnsSubscriptionTarget]
  -> DiffusionArguments
createDiffusionArguments addrs myLocalAddr ipProducers dnsProducers =
  DiffusionArguments
    { daAddresses    = addrs
    , daLocalAddress = myLocalAddr
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

-- | NodeIds are only required for mock protocols
nid :: NodeCLI -> IO Word64
nid NodeCLI{nodeMode = RealProtocolMode} =
    panic $ "Cardano.Node.Run.nid: Real protocols do not require node ids"
nid npm@NodeCLI{nodeMode = MockProtocolMode} = do
   nc <- parseNodeConfiguration npm
   case ncNodeId nc of
        Just (CoreId (CoreNodeId n)) -> pure n
        Just (RelayId _) -> panic $ "Cardano.Node.Run.nid: "
                                 <> "Non-core nodes currently not supported"
        Nothing -> panic $ "Cardano.Node.Run.nid: "
                         <> "Please specify a NodeId in your configuration .yaml file"

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

-- |
-- Possibly return a SOCKET_UNIX socket for NodeToClient communication
-- and a list of SOCKET_STREAM sockets for NodeToNode communication.
-- The SOCKEt_UNIX socket should be defined first in the `.socket` systemd file.
systemdSockets :: IO (Maybe (Socket, [Socket]))
#ifdef SYSTEMD
systemdSockets = do
  sds_m <- getActivatedSockets
  case sds_m of
       Nothing       -> return Nothing
       Just []       -> return Nothing
       Just (sd:sds) -> return $ Just (sd, sds)
#else
systemdSockets = return Nothing
#endif
