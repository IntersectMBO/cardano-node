{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}


{-# OPTIONS_GHC -Wno-unused-imports #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

module Cardano.Node.Run
  ( runNode
  , checkVRFFilePermissions
  ) where

import           Cardano.Api (File (..), FileDirection (..))
import           Cardano.Api.Error (displayError)
import qualified Cardano.Api as Api
import           System.Random (randomIO)

import           Cardano.BM.Data.LogItem (LogObject (..))
import           Cardano.BM.Data.Tracer (ToLogObject (..), TracingVerbosity (..))
import           Cardano.BM.Data.Transformers (setHostname)
import           Cardano.BM.Trace
import qualified Cardano.Crypto.Init as Crypto
import           Cardano.Node.Configuration.LedgerDB
import           Cardano.Node.Configuration.Logging (LoggingLayer (..), createLoggingLayer,
                   nodeBasicInfo, shutdownLoggingLayer)
import           Cardano.Node.Configuration.NodeAddress
import           Cardano.Node.Configuration.POM (NodeConfiguration (..),
                   PartialNodeConfiguration (..), TimeoutOverride (..),
                   defaultPartialNodeConfiguration, makeNodeConfiguration,
                   parseNodeConfigurationFP, getForkPolicy)
import           Cardano.Node.Configuration.Socket (LocalSocketOrSocketInfo,
                   SocketOrSocketInfo, SocketOrSocketInfo' (..),
                   gatherConfiguredSockets, getSocketOrSocketInfoAddr)
import           Cardano.Node.Configuration.TopologyP2P
import qualified Cardano.Node.Configuration.TopologyP2P as TopologyP2P
import           Cardano.Node.Handlers.Shutdown
import           Cardano.Node.Protocol (ProtocolInstantiationError (..), mkConsensusProtocol)
import           Cardano.Node.Protocol.Byron (ByronProtocolInstantiationError (CredentialsError))
import           Cardano.Node.Protocol.Cardano (CardanoProtocolInstantiationError (..))
import           Cardano.Node.Protocol.Shelley (PraosLeaderCredentialsError (..),
                   ShelleyProtocolInstantiationError (PraosLeaderCredentialsError))
import           Cardano.Node.Protocol.Types
import           Cardano.Node.Queries
import           Cardano.Node.Startup
import           Cardano.Node.TraceConstraints (TraceConstraints)
import           Cardano.Node.Tracing.API
import           Cardano.Node.Tracing.StateRep (NodeState (NodeKernelOnline))
import           Cardano.Node.Tracing.Tracers.NodeVersion (getNodeVersion)
import           Cardano.Node.Tracing.Tracers.Startup (getStartupInfo)
import           Cardano.Node.Types
import           Cardano.Prelude (ExitCode (..), FatalError (..), bool, (:~:) (..))
import           Cardano.Slotting.Slot (WithOrigin (..))
import           Cardano.Tracing.Config (TraceOptions (..), TraceSelection (..))
import           Cardano.Tracing.Tracers

import qualified Ouroboros.Consensus.Config as Consensus
import           Ouroboros.Consensus.Config.SupportsNode (ConfigSupportsNode (..))
import           Ouroboros.Consensus.Node (SnapshotPolicyArgs (..),
                   NodeDatabasePaths (..), nonImmutableDbPath, RunNodeArgs (..), StdRunNodeArgs (..))
import           Ouroboros.Consensus.Protocol.Praos.AgentClient (KESAgentClientTrace)
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTxId)
import           Ouroboros.Consensus.Node (RunNodeArgs (..),
                   SnapshotPolicyArgs (..), StdRunNodeArgs (..))
import qualified Ouroboros.Consensus.Node as Node (NodeDatabasePaths (..), getChainDB, run)
import           Ouroboros.Consensus.Node.Genesis
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.ProtocolInfo
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import qualified Ouroboros.Consensus.Storage.LedgerDB.Args as LDBArgs
import           Ouroboros.Consensus.Util.Args
import           Ouroboros.Consensus.Util.Orphans ()

import           Cardano.Network.ConsensusMode
import qualified Cardano.Network.Diffusion as Cardano.Diffusion
import qualified Cardano.Network.Diffusion.Configuration as Configuration
import           Cardano.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..))
import           Cardano.Network.PeerSelection.PeerTrustable (PeerTrustable)
import qualified Cardano.Network.PeerSelection.PeerSelectionActions as Cardano
import           Cardano.Network.PeerSelection.Churn (ChurnMode (..), peerChurnGovernor)
import qualified Cardano.Network.PeerSelection.Governor.PeerSelectionActions as Cardano.PeerSelection
import qualified Cardano.Network.PeerSelection.Governor.PeerSelectionState as Cardano.PeerSelection
import qualified Cardano.Network.PeerSelection.Governor.PeerSelectionState as CPST
import qualified Cardano.Network.PeerSelection.Governor.Types as Cardano
import qualified Cardano.Network.PeerSelection.Governor.Types as CPSV
import qualified Cardano.Network.PeerSelection.PublicRootPeers as Cardano.PublicRoots
import qualified Cardano.Network.PeerSelection.Governor.PeerSelectionActions as Cardano.PeerSelection
import qualified Cardano.Network.LedgerPeerConsensusInterface as Cardano
import qualified Cardano.Network.PeerSelection.PeerSelectionActions as Cardano
import qualified Cardano.Network.PeerSelection.Churn as Cardano.Churn
import           Cardano.Network.Types (NumberOfBigLedgerPeers (..))

import           Ouroboros.Network.BlockFetch (FetchMode)
import qualified Ouroboros.Network.Diffusion as Diffusion
import qualified Ouroboros.Network.Diffusion.Types as Diffusion
import qualified Ouroboros.Network.Diffusion.Configuration as Configuration
import           Ouroboros.Network.Mux (noBindForkPolicy, responderForkPolicy, ForkPolicy)
import           Cardano.Network.NodeToClient (LocalAddress (..), LocalSocket (..))
import           Cardano.Network.NodeToNode (AcceptedConnectionsLimit (..), ConnectionId,
                   PeerSelectionTargets (..), RemoteAddress)
import           Ouroboros.Network.PeerSelection.Governor.Types (PeerSelectionState,
                   PublicPeerSelectionState, makePublicPeerSelectionStateVar, BootstrapPeersCriticalTimeoutError)
import           Ouroboros.Network.PeerSelection.LedgerPeers.Type (LedgerPeerSnapshot (..),
                   UseLedgerPeers (..), AfterSlot (..))
import           Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import           Ouroboros.Network.PeerSelection.RelayAccessPoint (RelayAccessPoint (..))
import           Ouroboros.Network.PeerSelection.RootPeersDNS.PublicRootPeers (TracePublicRootPeers)
import           Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency, LocalRootConfig (..), WarmValency)
import           Ouroboros.Network.Protocol.ChainSync.Codec

import           Control.Applicative (empty)
import           Control.Concurrent (killThread, mkWeakThreadId, myThreadId, getNumCapabilities)
import           Control.Concurrent.Class.MonadSTM.Strict
import           Control.Exception (try, Exception, IOException)
import qualified Control.Exception as Exception
import           Control.Monad (forM, forM_, unless, void, when, join)
import           Control.Monad.Class.MonadThrow (MonadThrow (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Control.Monad.Trans.Except.Extra (left, hushM)
import           Control.Monad.Trans.Maybe (MaybeT(runMaybeT, MaybeT), hoistMaybe)
import           "contra-tracer" Control.Tracer
import           Data.Bits
import           Data.Either (partitionEithers)
import           Data.IP (toSockAddr)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import           Data.Monoid (Last (..))
import           Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import           Data.SOP.Dict
import           Data.Text (Text, breakOn, pack)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import           Data.Time.Clock (getCurrentTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Data.Version (showVersion)
import           Network.DNS (Resolver)
import           Network.HostName (getHostName)
import           Network.Socket (Socket)
import           System.Directory (canonicalizePath, createDirectoryIfMissing, makeAbsolute)
import           System.Environment (lookupEnv)
import           System.IO (hPutStrLn)
#ifdef UNIX
import           GHC.Weak (deRefWeak)
import           System.Posix.Files
import qualified System.Posix.Signals as Signals
import           System.Posix.Types (FileMode)
#else
import           System.Win32.File
#endif
import           Paths_cardano_node (version)

import           Paths_cardano_node (version)
import Ouroboros.Consensus.Mempool (MempoolTimeoutConfig(..))

{- HLINT ignore "Fuse concatMap/map" -}
{- HLINT ignore "Redundant <$>" -}
{- HLINT ignore "Use fewer imports" -}

runNode
  :: PartialNodeConfiguration
  -> IO ()
runNode cmdPc = do
    installSigTermHandler

    Crypto.cryptoInit

    configYamlPc <- parseNodeConfigurationFP . getLast $ pncConfigFile cmdPc

    nc <- case makeNodeConfiguration $ defaultPartialNodeConfiguration <> configYamlPc <> cmdPc of
            Left err -> error $ "Error in creating the NodeConfiguration: " <> err
            Right nc' -> return nc'

    putStrLn $ "Node configuration: " <> show nc

    case ncProtocolFiles nc of
      ProtocolFilepaths{shelleyVRFFile=Just vrfFp} ->
        runThrowExceptT $
          checkVRFFilePermissions stdoutTracer (File vrfFp)
      _ -> pure ()

    consensusProtocol <-
      runThrowExceptT $
        mkConsensusProtocol
         (ncProtocolConfig nc)
         -- TODO: Convert ncProtocolFiles to Maybe as relay nodes
         -- don't need these.
         (Just $ ncProtocolFiles nc)

    handleNodeWithTracers cmdPc nc consensusProtocol

runThrowExceptT :: Exception e => ExceptT e IO a -> IO a
runThrowExceptT act = runExceptT act >>= either Exception.throwIO pure

-- | Workaround to ensure that the main thread throws an async exception on
-- receiving a SIGTERM signal.
installSigTermHandler :: IO ()
installSigTermHandler = do
#ifdef UNIX
  -- Similar implementation to the RTS's handling of SIGINT (see GHC's
  -- https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/GHC/TopHandler.hs).
  runThreadIdWk <- mkWeakThreadId =<< myThreadId
  _ <- Signals.installHandler
    Signals.sigTERM
    (Signals.CatchOnce $ do
      runThreadIdMay <- deRefWeak runThreadIdWk
      forM_ runThreadIdMay $ \runThreadId -> Exception.throwTo runThreadId ExitSuccess
    )
    Nothing
#endif
  return ()

handleNodeWithTracers
  :: PartialNodeConfiguration
  -> NodeConfiguration
  -> SomeConsensusProtocol
  -> IO ()
handleNodeWithTracers cmdPc nc p@(SomeConsensusProtocol blockType runP) = do
  let ProtocolInfo{pInfoConfig} = fst $ Api.protocolInfo @IO runP
      networkMagic :: Api.NetworkMagic = getNetworkMagic $ Consensus.configBlock pInfoConfig
  -- This IORef contains node kernel structure which holds node kernel.
  -- Used for ledger queries and peer connection status.
  nodeKernelData <- mkNodeKernelData
  let ProtocolInfo { pInfoConfig = cfg } = fst $ Api.protocolInfo @IO runP
  let fp = maybe  "No file path found!"
                  unConfigPath
                  (getLast (pncConfigFile cmdPc))
  case ncTraceConfig nc of
    TraceDispatcher{} -> do
      blockForging <- snd (Api.protocolInfo runP) nullTracer
      tracers <-
        initTraceDispatcher
          nc
          p
          networkMagic
          nodeKernelData
          (null blockForging)

      startupInfo <- getStartupInfo nc p fp
      mapM_ (traceWith $ startupTracer tracers) startupInfo
      traceNodeStartupInfo (nodeStartupInfoTracer tracers) startupInfo
      -- sends initial BlockForgingUpdate
      let isNonProducing = ncStartAsNonProducingNode nc
      traceWith (startupTracer tracers)
                (BlockForgingUpdate (if isNonProducing || null blockForging
                                      then DisabledBlockForging
                                      else EnabledBlockForging))

      handleSimpleNode blockType runP tracers nc
        (\nk -> do
            setNodeKernel nodeKernelData nk
            traceWith (nodeStateTracer tracers) NodeKernelOnline)

    _ -> do
      eLoggingLayer <- runExceptT $ createLoggingLayer
        (Text.pack (showVersion version))
        nc
        p

      loggingLayer <- case eLoggingLayer of
        Left err  -> Exception.throwIO err
        Right res -> return res
      !trace <- setupTrace loggingLayer
      let tracer = contramap pack $ toLogObject trace
      logTracingVerbosity nc tracer

      -- Legacy logging infrastructure must trace 'nodeStartTime' and 'nodeBasicInfo'.
      startTime <- getCurrentTime
      traceCounter "nodeStartTime" trace (ceiling $ utcTimeToPOSIXSeconds startTime)
      nbi <- nodeBasicInfo nc p startTime
      forM_ nbi $ \(LogObject nm mt content) ->
        traceNamedObject (appendName nm trace) (mt, content)

      tracers <-
        mkTracers
          (Consensus.configBlock cfg)
          (ncTraceConfig nc)
          trace
          nodeKernelData
          (llEKGDirect loggingLayer)

      getStartupInfo nc p fp
        >>= mapM_ (traceWith $ startupTracer tracers)

      traceWith (nodeVersionTracer tracers) getNodeVersion
      let isNonProducing = ncStartAsNonProducingNode nc
      blockForging <- snd (Api.protocolInfo runP) nullTracer
      traceWith (startupTracer tracers)
                (BlockForgingUpdate (if isNonProducing || null blockForging
                                      then DisabledBlockForging
                                      else EnabledBlockForging))

      -- We ignore peer logging thread if it dies, but it will be killed
      -- when 'handleSimpleNode' terminates.
      handleSimpleNode blockType runP tracers nc
        (\nk -> do
            setNodeKernel nodeKernelData nk
            traceWith (nodeStateTracer tracers) NodeKernelOnline)
        `finally` do
          forM_ eLoggingLayer
            shutdownLoggingLayer

-- | Currently, we trace only 'ShelleyBased'-info which will be asked
--   by 'cardano-tracer' service as a datapoint. It can be extended in the future.
traceNodeStartupInfo
  :: Tracer IO NodeStartupInfo
  -> [StartupTrace blk]
  -> IO ()
traceNodeStartupInfo t startupTrace =
  forM_ startupTrace $ \case
    BIShelley (BasicInfoShelleyBased era _ sl el spkp) ->
      traceWith t $ NodeStartupInfo era sl el spkp
    _ -> return ()

logTracingVerbosity :: NodeConfiguration -> Tracer IO String -> IO ()
logTracingVerbosity nc tracer =
  case ncTraceConfig nc of
    TracingOff -> return ()
    TracingOnLegacy traceConf ->
      case traceVerbosity traceConf of
        NormalVerbosity -> traceWith tracer "tracing verbosity = normal verbosity "
        MinimalVerbosity -> traceWith tracer "tracing verbosity = minimal verbosity "
        MaximalVerbosity -> traceWith tracer "tracing verbosity = maximal verbosity "
    TraceDispatcher _traceConf ->
      pure ()
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
      return $ Text.take 8 $ fst $ breakOn "." hn0

{-
-- TODO: needs to be finished (issue #4362)
handlePeersListSimple
  :: Trace IO Text
  -> NodeKernelData blk
  -> IO ()
handlePeersListSimple tr nodeKern = forever $ do
  getCurrentPeers nodeKern >>= tracePeers tr
  threadDelay 2000000 -- 2 seconds.
-}

-- | Sets up a simple node, which will run the chain sync protocol and block
-- fetch protocol, and, if core, will also look at the mempool when trying to
-- create a new block.

handleSimpleNode
  :: forall blk .
    ( Api.Protocol IO blk
    )
  => Api.BlockType blk
  -> Api.ProtocolInfoArgs blk
  -> Tracers RemoteAddress LocalAddress blk IO
  -> NodeConfiguration
  -> (NodeKernel IO RemoteAddress LocalConnectionId blk -> IO ())
  -- ^ Called on the 'NodeKernel' after creating it, but before the network
  -- layer is initialised.  This implies this function must not block,
  -- otherwise the node won't actually start.
  -> IO ()
handleSimpleNode blockType runP tracers nc onKernel = do
  logStartupWarnings

  logDeprecatedLedgerDBOptions

  traceWith (startupTracer tracers)
    =<< StartupTime <$> getCurrentTime

  when (ncValidateDB nc) $
    traceWith (startupTracer tracers)
      StartupDBValidation

  let pInfo = fst $ Api.protocolInfo @IO runP

  (publicIPv4SocketOrAddr, publicIPv6SocketOrAddr, localSocketOrPath) <- do
    result <- runExceptT (gatherConfiguredSockets $ ncSocketConfig nc)
    case result of
      Right triplet -> return triplet
      Left err -> do
        traceWith (startupTracer tracers)
                $ StartupSocketConfigError err
        Exception.throwIO err

  dbPath <- canonDbPath nc

  (publicPeerSelectionVar :: StrictTVar IO (PublicPeerSelectionState RemoteAddress))
    <- makePublicPeerSelectionStateVar

  ipv4 <- traverse getSocketOrSocketInfoAddr publicIPv4SocketOrAddr
  ipv6 <- traverse getSocketOrSocketInfoAddr publicIPv6SocketOrAddr

  traceWith (startupTracer tracers)
            (StartupInfo (catMaybes [ipv4, ipv6])
                         localSocketOrPath
                         ( limitToLatestReleasedVersion fst
                         . supportedNodeToNodeVersions
                         $ Proxy @blk
                         )
                         ( limitToLatestReleasedVersion snd
                         . supportedNodeToClientVersions
                         $ Proxy @blk
                         ))

  withShutdownHandling (ncShutdownConfig nc) (shutdownTracer tracers) $ do
    traceWith (startupTracer tracers)
              (StartupP2PInfo (ncDiffusionMode nc))
    nt@TopologyP2P.RealNodeTopology
      { ntUseLedgerPeers
      , ntUseBootstrapPeers
      , ntPeerSnapshotPath
      } <- TopologyP2P.readTopologyFileOrError nc (startupTracer tracers)
    let (localRoots, publicRoots) = producerAddresses nt
    traceWith (startupTracer tracers)
            $ NetworkConfig localRoots
                            publicRoots
                            ntUseLedgerPeers
                            ntPeerSnapshotPath
    case ncPeerSharing nc of
      PeerSharingEnabled
        | hasProtocolFile (ncProtocolFiles nc) ->
            traceWith (startupTracer tracers) . NetworkConfigUpdateWarning . Text.pack $
                 "Mainnet block producers may not meet the Praos performance guarantees "
              <> "and host IP address will be leaked since peer sharing is enabled."
      _otherwise -> pure ()
    localRootsVar   <- newTVarIO localRoots
    publicRootsVar  <- newTVarIO publicRoots
    useLedgerVar    <- newTVarIO ntUseLedgerPeers
    useBootstrapVar <- newTVarIO ntUseBootstrapPeers
    ledgerPeerSnapshotPathVar <- newTVarIO ntPeerSnapshotPath
    ledgerPeerSnapshotVar <- newTVarIO =<< updateLedgerPeerSnapshot
                                            (startupTracer tracers)
                                            nc
                                            (readTVar ledgerPeerSnapshotPathVar)
                                            (readTVar useLedgerVar)
                                            (const . pure $ ())

    let nodeArgs = RunNodeArgs
          { rnGenesisConfig  = ncGenesisConfig nc
          , rnTraceConsensus = consensusTracers tracers
          , rnTraceNTN       = nodeToNodeTracers tracers
          , rnTraceNTC       = nodeToClientTracers tracers
          , rnProtocolInfo   = pInfo
          , rnMempoolTimeoutConfig = Just $ MempoolTimeoutConfig
              { mempoolTimeoutSoft     = ncMempoolTimeoutSoft nc
              , mempoolTimeoutHard     = ncMempoolTimeoutHard nc
              , mempoolTimeoutCapacity = ncMempoolTimeoutCapacity nc
              }
          , rnNodeKernelHook = \registry nodeKernel -> do
              -- set the initial block forging
              blockForging <- snd (Api.protocolInfo runP) (Consensus.kesAgentTracer $ consensusTracers tracers)

              unless (ncStartAsNonProducingNode nc) $
                setBlockForging nodeKernel blockForging

              maybeSpawnOnSlotSyncedShutdownHandler
                (ncShutdownConfig nc)
                (shutdownTracer tracers)
                registry
                (Node.getChainDB nodeKernel)
              onKernel nodeKernel
          , rnPeerSharing    = ncPeerSharing nc
          , rnGetUseBootstrapPeers = readTVar useBootstrapVar
          , rnFeatureFlags = mempty
          }
#ifdef UNIX
    -- initial `SIGHUP` handler, which only rereads the topology file but
    -- doesn't update block forging.  The latter is only possible once
    -- consensus initialised (e.g. reapplied all blocks).
    _ <- Signals.installHandler
          Signals.sigHUP
          (Signals.Catch $ do
            updateTopologyConfiguration
              (startupTracer tracers) nc
              localRootsVar publicRootsVar useLedgerVar useBootstrapVar
              ledgerPeerSnapshotPathVar
            void $ updateLedgerPeerSnapshot
              (startupTracer tracers)
              nc
              (readTVar ledgerPeerSnapshotPathVar)
              (readTVar useLedgerVar)
              (writeTVar ledgerPeerSnapshotVar)
            traceWith (startupTracer tracers) (BlockForgingUpdate NotEffective)
          )
          Nothing
#endif
    nForkPolicy <- getForkPolicy $ ncResponderCoreAffinityPolicy nc
    cForkPolicy <- getForkPolicy $ ncResponderCoreAffinityPolicy nc
    void $
      let diffusionNodeArguments :: Cardano.Diffusion.CardanoNodeArguments IO
          diffusionNodeArguments = Cardano.Diffusion.CardanoNodeArguments {
              Cardano.Diffusion.consensusMode      = ncConsensusMode nc,
              Cardano.Diffusion.genesisPeerTargets =
                PeerSelectionTargets {
                  targetNumberOfRootPeers                 = ncSyncTargetOfRootPeers nc,
                  targetNumberOfKnownPeers                = ncSyncTargetOfKnownPeers nc,
                  targetNumberOfEstablishedPeers          = ncSyncTargetOfEstablishedPeers nc,
                  targetNumberOfActivePeers               = ncSyncTargetOfActivePeers nc,
                  targetNumberOfKnownBigLedgerPeers       = ncSyncTargetOfKnownBigLedgerPeers nc,
                  targetNumberOfEstablishedBigLedgerPeers = ncSyncTargetOfEstablishedBigLedgerPeers nc,
                  targetNumberOfActiveBigLedgerPeers      = ncSyncTargetOfActiveBigLedgerPeers nc
                },
              Cardano.Diffusion.minNumOfBigLedgerPeers  = ncMinBigLedgerPeersForTrustedState nc,
              Cardano.Diffusion.tracerChurnMode         = churnModeTracer tracers
            }

          diffusionConfiguration :: Cardano.Diffusion.CardanoConfiguration IO
          diffusionConfiguration =
            mkDiffusionConfiguration
              publicIPv4SocketOrAddr
              publicIPv6SocketOrAddr
              localSocketOrPath
              publicPeerSelectionVar
              nForkPolicy cForkPolicy
              (readTVar localRootsVar)
              (readTVar publicRootsVar)
              (readTVar useLedgerVar)
              (readTVar ledgerPeerSnapshotVar)
              nc
      in
      Node.run
        nodeArgs {
            rnNodeKernelHook = \registry nodeKernel -> do
              -- reinstall `SIGHUP` handler
              installSigHUPHandler (startupTracer tracers) (Consensus.kesAgentTracer $ consensusTracers tracers) blockType nc nodeKernel
                                   localRootsVar publicRootsVar useLedgerVar useBootstrapVar
                                   ledgerPeerSnapshotPathVar ledgerPeerSnapshotVar
              rnNodeKernelHook nodeArgs registry nodeKernel
        }
        StdRunNodeArgs
          { srnBfcMaxConcurrencyBulkSync    = unMaxConcurrencyBulkSync <$> ncMaxConcurrencyBulkSync nc
          , srnBfcMaxConcurrencyDeadline    = unMaxConcurrencyDeadline <$> ncMaxConcurrencyDeadline nc
          , srnChainDbValidateOverride      = ncValidateDB nc
          , srnDatabasePath                 = dbPath
          , srnDiffusionConfiguration       = diffusionConfiguration
          , srnDiffusionArguments           = diffusionNodeArguments
          , srnDiffusionTracers             = diffusionTracers tracers
          , srnEnableInDevelopmentVersions  = ncExperimentalProtocolsEnabled nc
          , srnTraceChainDB                 = chainDBTracer tracers
          , srnMaybeMempoolCapacityOverride = ncMaybeMempoolCapacityOverride nc
          , srnChainSyncIdleTimeout         = customizeChainSyncTimeout
          , srnSnapshotPolicyArgs           = snapshotPolicyArgs
          , srnQueryBatchSize               = queryBatchSize
          , srnLedgerDbBackendArgs          = selectorToArgs ldbBackend (nonImmutableDbPath dbPath)
          }
 where
  customizeChainSyncTimeout :: ChainSyncIdleTimeout
  customizeChainSyncTimeout = case ncChainSyncIdleTimeout nc of
    NoTimeoutOverride -> Configuration.defaultChainSyncIdleTimeout
    TimeoutOverride t | t == 0    -> ChainSyncNoIdleTimeout
                      | otherwise -> ChainSyncIdleTimeout t

  logStartupWarnings :: IO ()
  logStartupWarnings = do
    let developmentNtnVersions =
          case latestReleasedNodeVersion (Proxy @blk) of
            (Just ntnVersion, _) -> filter (> ntnVersion)
                                  . Map.keys
                                  $ supportedNodeToNodeVersions (Proxy @blk)
            (Nothing, _)         -> Map.keys
                                  $ supportedNodeToNodeVersions (Proxy @blk)
        developmentNtcVersions =
          case latestReleasedNodeVersion (Proxy @blk) of
            (_, Just ntcVersion) -> filter (> ntcVersion)
                                  . Map.keys
                                  $ supportedNodeToClientVersions (Proxy @blk)
            (_, Nothing)         -> Map.keys
                                  $ supportedNodeToClientVersions (Proxy @blk)
    when (  ncExperimentalProtocolsEnabled nc
         && not (null developmentNtnVersions))
       $ traceWith (startupTracer tracers)
                   (WarningDevelopmentNodeToNodeVersions
                     developmentNtnVersions)

    when (  ncExperimentalProtocolsEnabled nc
         && not (null developmentNtcVersions))
       $ traceWith (startupTracer tracers)
                   (WarningDevelopmentNodeToClientVersions
                     developmentNtcVersions)


  logDeprecatedLedgerDBOptions :: IO ()
  logDeprecatedLedgerDBOptions =
    case deprecatedOpts of
      DeprecatedOptions [] -> pure ()
      DeprecatedOptions opts ->
        mapM_ (traceWith (startupTracer tracers) . MovedTopLevelOption) opts

  limitToLatestReleasedVersion :: forall k v.
       Ord k
    => ((Maybe NodeToNodeVersion, Maybe NodeToClientVersion) -> Maybe k)
    -> Map k v
    -> Map k v
  limitToLatestReleasedVersion prj =
      if ncExperimentalProtocolsEnabled nc then id
      else
      case prj $ latestReleasedNodeVersion (Proxy @blk) of
        Nothing       -> id
        Just version_ -> Map.takeWhileAntitone (<= version_)

  LedgerDbConfiguration
    snapInterval
    numSnaps
    queryBatchSize
    ldbBackend
    deprecatedOpts = ncLedgerDbConfig nc

  snapshotPolicyArgs :: SnapshotPolicyArgs
  snapshotPolicyArgs = SnapshotPolicyArgs numSnaps snapInterval

--------------------------------------------------------------------------------
-- SIGHUP Handlers
--------------------------------------------------------------------------------

-- | The P2P SIGHUP handler can update block forging & reconfigure network topology.
--
installSigHUPHandler :: Tracer IO (StartupTrace blk)
                     -> Tracer IO KESAgentClientTrace
                     -> Api.BlockType blk
                     -> NodeConfiguration
                     -> NodeKernel IO RemoteAddress (ConnectionId LocalAddress) blk
                     -> StrictTVar IO [(HotValency, WarmValency, Map RelayAccessPoint (LocalRootConfig PeerTrustable))]
                     -> StrictTVar IO (Map RelayAccessPoint PeerAdvertise)
                     -> StrictTVar IO UseLedgerPeers
                     -> StrictTVar IO UseBootstrapPeers
                     -> StrictTVar IO (Maybe PeerSnapshotFile)
                     -> StrictTVar IO (Maybe LedgerPeerSnapshot)
                     -> IO ()
#ifndef UNIX
installSigHUPHandler _ _ _ _ _ _ _ _ _ _ _ = return ()
#else
installSigHUPHandler startupTracer kesAgentTracer blockType nc nodeKernel localRootsVar publicRootsVar useLedgerVar
                        useBootstrapPeersVar ledgerPeerSnapshotPathVar ledgerPeerSnapshotVar =
  void $ Signals.installHandler
    Signals.sigHUP
    (Signals.Catch $ do
      updateBlockForging startupTracer kesAgentTracer blockType nodeKernel nc
      updateTopologyConfiguration startupTracer nc localRootsVar publicRootsVar
                                  useLedgerVar useBootstrapPeersVar ledgerPeerSnapshotPathVar
      void $ updateLedgerPeerSnapshot
               startupTracer
               nc
               (readTVar ledgerPeerSnapshotPathVar)
               (readTVar useLedgerVar)
               (writeTVar ledgerPeerSnapshotVar)
    )
    Nothing
#endif


#ifdef UNIX
updateBlockForging :: Tracer IO (StartupTrace blk)
                   -> Tracer IO KESAgentClientTrace
                   -> Api.BlockType blk
                   -> NodeKernel IO RemoteAddress (ConnectionId LocalAddress) blk
                   -> NodeConfiguration
                   -> IO ()
updateBlockForging startupTracer kesAgentTracer blockType nodeKernel nc = do
  eitherSomeProtocol <- runExceptT $ mkConsensusProtocol
                                       (ncProtocolConfig nc)
                                       (Just (ncProtocolFiles nc))
  case eitherSomeProtocol of
    Left err ->
      case wasFileRemovedFromScope err of
        Just (Api.FileDoesNotExistError _) -> do
          traceWith startupTracer (BlockForgingUpdate DisabledBlockForging)
          setBlockForging nodeKernel []
        _NothingOrOtherFileError ->
          traceWith startupTracer (BlockForgingUpdateError err)
    Right (SomeConsensusProtocol blockType' runP') ->
      case Api.reflBlockType blockType blockType' of
        Just Refl -> do
          -- TODO: check if runP' has changed
          blockForging <- snd (Api.protocolInfo runP') kesAgentTracer
          traceWith startupTracer
                    (BlockForgingUpdate (if null blockForging
                                          then DisabledBlockForging
                                          else EnabledBlockForging))
          setBlockForging nodeKernel blockForging
        Nothing ->
          traceWith startupTracer
            $ BlockForgingBlockTypeMismatch
                (Api.SomeBlockType blockType)
                (Api.SomeBlockType blockType')
  return ()
  where
    wasFileRemovedFromScope :: ProtocolInstantiationError
                            -> Maybe (Api.FileError Api.TextEnvelopeError)
    wasFileRemovedFromScope (ShelleyProtocolInstantiationError
                              (PraosLeaderCredentialsError
                                (FileError fe))) = Just fe
    wasFileRemovedFromScope (CardanoProtocolInstantiationError
                              (CardanoProtocolInstantiationPraosLeaderCredentialsError
                                (FileError fe))) = Just fe
    wasFileRemovedFromScope (CardanoProtocolInstantiationError
                              (CardanoProtocolInstantiationPraosLeaderCredentialsError
                                (CredentialsReadError fp _))) =
                                  Just (Api.FileDoesNotExistError fp)
    wasFileRemovedFromScope (ByronProtocolInstantiationError _)   = Nothing
    wasFileRemovedFromScope (ShelleyProtocolInstantiationError _) = Nothing
    wasFileRemovedFromScope (CardanoProtocolInstantiationError _) = Nothing


updateTopologyConfiguration :: Tracer IO (StartupTrace blk)
                            -> NodeConfiguration
                            -> StrictTVar IO [(HotValency, WarmValency, Map RelayAccessPoint (LocalRootConfig PeerTrustable))]
                            -> StrictTVar IO (Map RelayAccessPoint PeerAdvertise)
                            -> StrictTVar IO UseLedgerPeers
                            -> StrictTVar IO UseBootstrapPeers
                            -> StrictTVar IO (Maybe PeerSnapshotFile)
                            -> IO ()
updateTopologyConfiguration startupTracer nc localRootsVar publicRootsVar useLedgerVar
                            useBootsrapPeersVar ledgerPeerSnapshotPathVar = do
    traceWith startupTracer NetworkConfigUpdate
    result <- try $ TopologyP2P.readTopologyFileOrError nc startupTracer
    case result of
      Left (FatalError err) ->
        traceWith startupTracer
                $ NetworkConfigUpdateError
                $ pack "Error reading topology configuration file:" <> err
      Right nt@RealNodeTopology { ntUseLedgerPeers
                                , ntUseBootstrapPeers
                                , ntPeerSnapshotPath
                                } -> do
        let (localRoots, publicRoots) = producerAddresses nt
        traceWith startupTracer
                $ NetworkConfig localRoots publicRoots ntUseLedgerPeers ntPeerSnapshotPath
        atomically $ do
          writeTVar localRootsVar localRoots
          writeTVar publicRootsVar publicRoots
          writeTVar useLedgerVar ntUseLedgerPeers
          writeTVar useBootsrapPeersVar ntUseBootstrapPeers
          writeTVar ledgerPeerSnapshotPathVar ntPeerSnapshotPath
#endif

updateLedgerPeerSnapshot :: Tracer IO (StartupTrace blk)
                         -> NodeConfiguration
                         -> STM IO (Maybe PeerSnapshotFile)
                         -> STM IO UseLedgerPeers
                         -> (Maybe LedgerPeerSnapshot -> STM IO ())
                         -> IO (Maybe LedgerPeerSnapshot)
updateLedgerPeerSnapshot startupTracer (NodeConfiguration {ncConsensusMode}) readLedgerPeerPath readUseLedgerVar writeVar = do
  (mPeerSnapshotFile, useLedgerPeers)
    <- atomically $ (,) <$> readLedgerPeerPath <*> readUseLedgerVar

  let trace    = traceWith startupTracer
      traceL   = liftIO . trace

  mLedgerPeerSnapshot <- runMaybeT $ do
    case useLedgerPeers of
      DontUseLedgerPeers       -> empty
      UseLedgerPeers afterSlot -> do
        snapshotFile <- hoistMaybe mPeerSnapshotFile
        eSnapshot
          <- liftIO $ readPeerSnapshotFile snapshotFile
        lps@(LedgerPeerSnapshot (wOrigin, _)) <-
          case ncConsensusMode of
            GenesisMode ->
              MaybeT $ hushM eSnapshot (trace . NetworkConfigUpdateError)
            PraosMode  ->
              MaybeT $ hushM eSnapshot (trace . NetworkConfigUpdateWarning)
        case afterSlot of
          Always -> do
            traceL $ LedgerPeerSnapshotLoaded wOrigin
            return lps
          After ledgerSlotNo
            | fileSlot >= ledgerSlotNo -> do
                traceL $ LedgerPeerSnapshotLoaded wOrigin
                pure lps
            | otherwise -> do
                case ncConsensusMode of
                  GenesisMode -> do
                    traceL $ LedgerPeerSnapshotError ledgerSlotNo fileSlot snapshotFile
                    liftIO $ throwIO (LedgerPeerSnapshotTooOld ledgerSlotNo fileSlot snapshotFile)
                  PraosMode -> do
                    traceL $ LedgerPeerSnapshotIgnored ledgerSlotNo fileSlot snapshotFile
                    empty
            where
              fileSlot = case wOrigin of; Origin -> 0; At slot -> slot

  mLedgerPeerSnapshot <$ atomically (writeVar mLedgerPeerSnapshot)

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

canonDbPath :: NodeConfiguration -> IO Node.NodeDatabasePaths
canonDbPath NodeConfiguration{ncDatabaseFile = nodeDatabaseFps} =
  case nodeDatabaseFps of
    Node.OnePathForAllDbs dbFp -> do
      fp <- canonicalizePath =<< makeAbsolute dbFp
      createDirectoryIfMissing True fp
      return $ Node.OnePathForAllDbs fp

    Node.MultipleDbPaths immutable volatile -> do
      canonImmutable <- canonicalizePath =<< makeAbsolute immutable
      canonVolatile  <- canonicalizePath =<< makeAbsolute volatile
      createDirectoryIfMissing True canonImmutable
      createDirectoryIfMissing True canonVolatile
      return $ Node.MultipleDbPaths canonImmutable canonVolatile

-- | Make sure the VRF private key file is readable only
-- by the current process owner the node is running under.
checkVRFFilePermissions :: Tracer IO String -> File content direction -> ExceptT VRFPrivateKeyFilePermissionError IO ()
#ifdef UNIX
checkVRFFilePermissions tracer (File vrfPrivKey) = do
  fs <- liftIO $ getFileStatus vrfPrivKey
  let fm = fileMode fs
  -- Check the VRF private key file does not give read/write/exec permissions to others.
  when (hasOtherPermissions fm) $
     left $ OtherPermissionsExist vrfPrivKey
  -- Check the VRF private key file does not give read/write/exec permissions to any group.
  when (hasGroupPermissions fm) $
     liftIO $ traceWith tracer $ ("WARNING: " <>) .  displayError $ GroupPermissionsExist vrfPrivKey
 where
  hasPermission :: FileMode -> FileMode -> Bool
  hasPermission fModeA fModeB = fModeA `intersectFileModes` fModeB /= nullFileMode

  hasOtherPermissions :: FileMode -> Bool
  hasOtherPermissions fm' = fm' `hasPermission` otherModes

  hasGroupPermissions :: FileMode -> Bool
  hasGroupPermissions fm' = fm' `hasPermission` groupModes
#else
checkVRFFilePermissions _ (File vrfPrivKey) = do
  attribs <- liftIO $ getFileAttributes vrfPrivKey
  -- https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-createfilea
  -- https://docs.microsoft.com/en-us/windows/win32/fileio/file-access-rights-constants
  -- https://docs.microsoft.com/en-us/windows/win32/secauthz/standard-access-rights
  -- https://docs.microsoft.com/en-us/windows/win32/secauthz/generic-access-rights
  -- https://docs.microsoft.com/en-us/windows/win32/secauthz/access-mask
  when (attribs `hasPermission` genericPermissions)
       (left $ GenericPermissionsExist vrfPrivKey)
 where
  genericPermissions = gENERIC_ALL .|. gENERIC_READ .|. gENERIC_WRITE .|. gENERIC_EXECUTE
  hasPermission fModeA fModeB = fModeA .&. fModeB /= gENERIC_NONE
#endif


mkDiffusionConfiguration
  :: Maybe SocketOrSocketInfo -- ^ ipv4
  -> Maybe SocketOrSocketInfo -- ^ ipv6
  -> Maybe LocalSocketOrSocketInfo -- ^ unix socket or a named pipe (Windows)
  -> StrictTVar IO (PublicPeerSelectionState RemoteAddress)
  -> ForkPolicy RemoteAddress
  -> ForkPolicy LocalAddress
  -> STM IO [(HotValency, WarmValency, Map RelayAccessPoint (LocalRootConfig PeerTrustable))]
     -- ^ non-overlapping local root peers groups; the 'Int' denotes the
     -- valency of its group.
  -> STM IO (Map RelayAccessPoint PeerAdvertise)
  -> STM IO UseLedgerPeers
  -> STM IO (Maybe LedgerPeerSnapshot)
  -> NodeConfiguration
  -> Cardano.Diffusion.CardanoConfiguration IO
mkDiffusionConfiguration
  publicIPv4SocketOrAddr
  publicIPv6SocketOrAddr
  localSocketOrPath
  dcPublicPeerSelectionVar
  dcMuxForkPolicy dcLocalMuxForkPolicy
  dcReadLocalRootPeers
  dcReadPublicRootPeers
  dcReadUseLedgerPeers
  dcReadLedgerPeerSnapshot
  nc
  =
  Diffusion.Configuration
    { Diffusion.dcIPv4Address  =
        case publicIPv4SocketOrAddr of
          Just (ActualSocket socket) -> Just (Left socket)
          Just (SocketInfo addr)     -> Just (Right addr)
          Nothing                    -> Nothing
    , Diffusion.dcIPv6Address  =
        case publicIPv6SocketOrAddr of
          Just (ActualSocket socket) -> Just (Left socket)
          Just (SocketInfo addr)     -> Just (Right addr)
          Nothing                    -> Nothing
    , Diffusion.dcLocalAddress =
        case localSocketOrPath of
          Just (ActualSocket localSocket) -> Just (Left  localSocket)
          Just (SocketInfo localAddr)     -> Just (Right localAddr)
          Nothing                         -> Nothing
    , Diffusion.dcAcceptedConnectionsLimit = ncAcceptedConnectionsLimit nc
    , Diffusion.dcMode                     = ncDiffusionMode nc
    , Diffusion.dcPublicPeerSelectionVar
    , Diffusion.dcPeerSelectionTargets
    , Diffusion.dcReadLocalRootPeers
    , Diffusion.dcReadPublicRootPeers
    , Diffusion.dcReadLedgerPeerSnapshot
    , Diffusion.dcReadUseLedgerPeers
    , Diffusion.dcPeerSharing              = ncPeerSharing nc
    , Diffusion.dcProtocolIdleTimeout      = ncProtocolIdleTimeout nc
    , Diffusion.dcTimeWaitTimeout          = ncTimeWaitTimeout nc
    , Diffusion.dcDeadlineChurnInterval    = Configuration.defaultDeadlineChurnInterval
    , Diffusion.dcBulkChurnInterval        = Configuration.defaultBulkChurnInterval
    , Diffusion.dcMuxForkPolicy
    , Diffusion.dcLocalMuxForkPolicy
    , Diffusion.dcEgressPollInterval       = ncEgressPollInterval nc
    }
  where
    dcPeerSelectionTargets = PeerSelectionTargets {
      targetNumberOfRootPeers                 = ncDeadlineTargetOfRootPeers nc,
      targetNumberOfKnownPeers                = ncDeadlineTargetOfKnownPeers nc,
      targetNumberOfEstablishedPeers          = ncDeadlineTargetOfEstablishedPeers nc,
      targetNumberOfActivePeers               = ncDeadlineTargetOfActivePeers nc,
      targetNumberOfKnownBigLedgerPeers       = ncDeadlineTargetOfKnownBigLedgerPeers nc,
      targetNumberOfEstablishedBigLedgerPeers = ncDeadlineTargetOfEstablishedBigLedgerPeers nc,
      targetNumberOfActiveBigLedgerPeers      = ncDeadlineTargetOfActiveBigLedgerPeers nc
    }


producerAddresses
  :: NetworkTopology RelayAccessPoint
  -> ( [(HotValency, WarmValency, Map RelayAccessPoint (LocalRootConfig PeerTrustable))]
     , Map RelayAccessPoint PeerAdvertise
     )
  -- ^ local roots & public roots
producerAddresses RealNodeTopology { ntLocalRootPeersGroups
                                   , ntPublicRootPeers
                                   } =
  ( map (\lrp -> ( hotValency lrp
                 , warmValency lrp
                 , Map.fromList
                 . map (\(addr, peerAdvertise) ->
                         ( addr
                         , LocalRootConfig {
                             diffusionMode = rootDiffusionMode lrp,
                             peerAdvertise,
                             extraFlags = trustable lrp
                           }
                         )
                       )
                 . rootConfigToRelayAccessPoint
                 $ localRoots lrp
                 )
        )
        (groups ntLocalRootPeersGroups)
  , foldMap ( Map.fromList
            . rootConfigToRelayAccessPoint
            . publicRoots
            ) ntPublicRootPeers
  )
