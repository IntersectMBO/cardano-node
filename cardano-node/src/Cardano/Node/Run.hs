{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
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

import qualified Cardano.Api as Api

import           Cardano.Prelude (FatalError (..))

import           Data.Bits
import           Data.IP (toSockAddr)

import           Control.Concurrent (killThread, mkWeakThreadId, myThreadId)
import           Control.Concurrent.Class.MonadSTM.Strict
import           Control.Exception (try)
import           Control.Monad (forM_, unless, void, when)
import           Control.Monad.Class.MonadThrow (MonadThrow (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Control.Monad.Trans.Except.Extra (left)
import           "contra-tracer" Control.Tracer
import           Data.Either (partitionEithers)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, mapMaybe)
import           Data.Monoid (Last (..))
import           Data.Proxy (Proxy (..))
import           Data.Text (Text, breakOn, pack)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import           Data.Time.Clock (getCurrentTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Data.Version (showVersion)
import           Network.HostName (getHostName)
import           Network.Socket (Socket)
import           System.Directory (canonicalizePath, createDirectoryIfMissing, makeAbsolute)
import           System.Environment (lookupEnv)
import           System.Exit (exitFailure)
#ifdef UNIX
import           GHC.Weak (deRefWeak)
import           System.Posix.Files
import qualified System.Posix.Signals as Signals
import           System.Posix.Types (FileMode)
#else
import           System.Win32.File
#endif

import           Cardano.BM.Data.LogItem (LogObject (..))
import           Cardano.BM.Data.Tracer (ToLogObject (..), TracingVerbosity (..))
import           Cardano.BM.Data.Transformers (setHostname)
import           Cardano.BM.Trace
import           Paths_cardano_node (version)

import qualified Cardano.Crypto.Libsodium as Crypto

import           Cardano.Node.Configuration.Logging (LoggingLayer (..), createLoggingLayer,
                   nodeBasicInfo, shutdownLoggingLayer)
import           Cardano.Node.Configuration.NodeAddress
import           Cardano.Node.Configuration.POM (NodeConfiguration (..),
                   PartialNodeConfiguration (..), SomeNetworkP2PMode (..),
                   defaultPartialNodeConfiguration, makeNodeConfiguration, parseNodeConfigurationFP)
import           Cardano.Node.Startup
import           Cardano.Node.Tracing.API
import           Cardano.Node.Tracing.StateRep (NodeState (NodeKernelOnline))
import           Cardano.Node.Tracing.Tracers.Startup (getStartupInfo)
import           Cardano.Node.Types
import           Cardano.Tracing.Config (TraceOptions (..), TraceSelection (..))

import qualified Ouroboros.Consensus.Config as Consensus
import           Ouroboros.Consensus.Config.SupportsNode (ConfigSupportsNode (..))
import           Ouroboros.Consensus.Node (NetworkP2PMode (..), RunNodeArgs (..),
                   StdRunNodeArgs (..))
import qualified Ouroboros.Consensus.Node as Node (getChainDB, run)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Util.Orphans ()
import qualified Ouroboros.Network.Diffusion as Diffusion
import qualified Ouroboros.Network.Diffusion.NonP2P as NonP2P
import qualified Ouroboros.Network.Diffusion.P2P as P2P
import           Ouroboros.Network.NodeToClient (LocalAddress (..), LocalSocket (..))
import           Ouroboros.Network.NodeToNode (AcceptedConnectionsLimit (..),
                   PeerSelectionTargets (..), RemoteAddress)
import           Ouroboros.Network.PeerSelection.LedgerPeers (UseLedgerAfter (..))
import           Ouroboros.Network.PeerSelection.RelayAccessPoint (RelayAccessPoint (..))
import           Ouroboros.Network.Subscription (DnsSubscriptionTarget (..),
                   IPSubscriptionTarget (..))

import           Cardano.Node.Configuration.Socket (SocketOrSocketInfo (..),
                   gatherConfiguredSockets, getSocketOrSocketInfoAddr)
import qualified Cardano.Node.Configuration.Topology as TopologyNonP2P
import           Cardano.Node.Configuration.TopologyP2P
import qualified Cardano.Node.Configuration.TopologyP2P as TopologyP2P
import           Cardano.Node.Handlers.Shutdown
import           Cardano.Node.Protocol (mkConsensusProtocol)
import           Cardano.Node.Protocol.Types
import           Cardano.Node.Queries
import           Cardano.Node.TraceConstraints (TraceConstraints)
import           Cardano.Tracing.Tracers
import           Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))

{- HLINT ignore "Fuse concatMap/map" -}
{- HLINT ignore "Redundant <$>" -}
{- HLINT ignore "Use fewer imports" -}

runNode
  :: PartialNodeConfiguration
  -> IO ()
runNode cmdPc = do
    installSigTermHandler

    -- TODO: Remove sodiumInit: https://github.com/input-output-hk/cardano-base/issues/175
    Crypto.sodiumInit

    configYamlPc <- parseNodeConfigurationFP . getLast $ pncConfigFile cmdPc

    nc <- case makeNodeConfiguration $ defaultPartialNodeConfiguration <> configYamlPc <> cmdPc of
            Left err -> error $ "Error in creating the NodeConfiguration: " <> err
            Right nc' -> return nc'

    putStrLn $ "Node configuration: " <> show nc

    case shelleyVRFFile $ ncProtocolFiles nc of
      Just vrfFp -> do vrf <- runExceptT $ checkVRFFilePermissions vrfFp
                       case vrf of
                         Left err ->
                           Text.putStrLn (renderVRFPrivateKeyFilePermissionError err) >> exitFailure
                         Right () ->
                           pure ()
      Nothing -> pure ()

    eitherSomeProtocol <- runExceptT $ mkConsensusProtocol
                                         (ncProtocolConfig nc)
                                         -- TODO: Convert ncProtocolFiles to Maybe as relay nodes
                                         -- don't need these.
                                         (Just $ ncProtocolFiles nc)

    p :: SomeConsensusProtocol <-
      case eitherSomeProtocol of
        Left err -> putStrLn (Api.displayError err) >> exitFailure
        Right p  -> pure p

    let networkMagic :: Api.NetworkMagic =
          case p of
            SomeConsensusProtocol _ runP ->
              let ProtocolInfo { pInfoConfig } = Api.protocolInfo runP
              in getNetworkMagic $ Consensus.configBlock pInfoConfig

    case p of
      SomeConsensusProtocol _ runP ->
        handleNodeWithTracers cmdPc nc p networkMagic runP

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
      forM_ runThreadIdMay $ \runThreadId -> killThread runThreadId
    )
    Nothing
#endif
  return ()

handleNodeWithTracers
  :: ( TraceConstraints blk
     , Api.Protocol IO blk
     )
  => PartialNodeConfiguration
  -> NodeConfiguration
  -> SomeConsensusProtocol
  -> Api.NetworkMagic
  -> Api.ProtocolInfoArgs IO blk
  -> IO ()
handleNodeWithTracers cmdPc nc p networkMagic runP = do
  -- This IORef contains node kernel structure which holds node kernel.
  -- Used for ledger queries and peer connection status.
  nodeKernelData <- mkNodeKernelData
  let ProtocolInfo { pInfoConfig = cfg } = Api.protocolInfo runP
  case ncEnableP2P nc of
    SomeNetworkP2PMode p2pMode -> do
      let fp = maybe  "No file path found!"
                      unConfigPath
                      (getLast (pncConfigFile cmdPc))
      case ncTraceConfig nc of
        TraceDispatcher{} -> do
          tracers <-
            initTraceDispatcher
              nc
              p
              networkMagic
              nodeKernelData
              p2pMode

          startupInfo <- getStartupInfo nc p fp
          mapM_ (traceWith $ startupTracer tracers) startupInfo
          traceNodeStartupInfo (nodeStartupInfoTracer tracers) startupInfo

          handleSimpleNode runP p2pMode tracers nc
            (\nk -> do
                setNodeKernel nodeKernelData nk
                traceWith (nodeStateTracer tracers) NodeKernelOnline)

        _ -> do
          eLoggingLayer <- runExceptT $ createLoggingLayer
            (Text.pack (showVersion version))
            nc
            p

          loggingLayer <- case eLoggingLayer of
            Left err  -> Text.putStrLn (Text.pack $ show err) >> exitFailure
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
              p2pMode

          getStartupInfo nc p fp
            >>= mapM_ (traceWith $ startupTracer tracers)

          -- We ignore peer logging thread if it dies, but it will be killed
          -- when 'handleSimpleNode' terminates.
          handleSimpleNode runP p2pMode tracers nc
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
  :: forall blk p2p . Api.Protocol IO blk
  => Api.ProtocolInfoArgs IO blk
  -> NetworkP2PMode p2p
  -> Tracers RemoteConnectionId LocalConnectionId blk p2p
  -> NodeConfiguration
  -> (NodeKernel IO RemoteAddress LocalConnectionId blk -> IO ())
  -- ^ Called on the 'NodeKernel' after creating it, but before the network
  -- layer is initialised.  This implies this function must not block,
  -- otherwise the node won't actually start.
  -> IO ()
handleSimpleNode runP p2pMode tracers nc onKernel = do
  logStartupWarnings

  traceWith (startupTracer tracers)
    =<< StartupTime <$> getCurrentTime

  when (ncValidateDB nc) $
    traceWith (startupTracer tracers)
      StartupDBValidation

  let pInfo = Api.protocolInfo runP

  (publicIPv4SocketOrAddr, publicIPv6SocketOrAddr, localSocketOrPath) <- do
    result <- runExceptT (gatherConfiguredSockets $ ncSocketConfig nc)
    case result of
      Right triplet -> return triplet
      Left err -> do
        traceWith (startupTracer tracers)
                $ StartupSocketConfigError err
        throwIO err

  dbPath <- canonDbPath nc

  let diffusionArguments :: Diffusion.Arguments Socket      RemoteAddress
                                                LocalSocket LocalAddress
      diffusionArguments =
        Diffusion.Arguments {
            Diffusion.daIPv4Address  =
              case publicIPv4SocketOrAddr of
                Just (ActualSocket socket) -> Just (Left socket)
                Just (SocketInfo addr)     -> Just (Right addr)
                Nothing                    -> Nothing
          , Diffusion.daIPv6Address  =
              case publicIPv6SocketOrAddr of
                Just (ActualSocket socket) -> Just (Left socket)
                Just (SocketInfo addr)     -> Just (Right addr)
                Nothing                    -> Nothing
          , Diffusion.daLocalAddress =
              case localSocketOrPath of  -- TODO allow expressing the Nothing case in the config
                Just (ActualSocket localSocket) -> Just (Left  localSocket)
                Just (SocketInfo localAddr)     -> Just (Right localAddr)
                Nothing                         -> Nothing
          , Diffusion.daAcceptedConnectionsLimit = ncAcceptedConnectionsLimit nc
          , Diffusion.daMode = ncDiffusionMode nc
          }

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

  withShutdownHandling (ncShutdownConfig nc) (shutdownTracer tracers) $
    let nodeArgs = RunNodeArgs
          { rnTraceConsensus = consensusTracers tracers
          , rnTraceNTN       = nodeToNodeTracers tracers
          , rnTraceNTC       = nodeToClientTracers tracers
          , rnProtocolInfo   = pInfo
          , rnNodeKernelHook = \registry nodeKernel -> do
              maybeSpawnOnSlotSyncedShutdownHandler
                (ncShutdownConfig nc)
                (shutdownTracer tracers)
                registry
                (Node.getChainDB nodeKernel)
              onKernel nodeKernel
          , rnEnableP2P      = p2pMode
          , rnPeerSharing    = ncPeerSharing nc
          }
    in case p2pMode of
      EnabledP2PMode -> do
        traceWith (startupTracer tracers)
                  (StartupP2PInfo (ncDiffusionMode nc))
        nt <- TopologyP2P.readTopologyFileOrError (startupTracer tracers) nc
        let (localRoots, publicRoots) = producerAddresses nt
        traceWith (startupTracer tracers)
                $ NetworkConfig localRoots
                                publicRoots
                                (useLedgerAfterSlot nt)
        (localRootsVar :: StrictTVar IO [(Int, Map RelayAccessPoint PeerAdvertise)])  <- newTVarIO localRoots
        publicRootsVar <- newTVarIO publicRoots
        useLedgerVar   <- newTVarIO (useLedgerAfterSlot nt)
#ifdef UNIX
        _ <- Signals.installHandler
              Signals.sigHUP
              (updateTopologyConfiguration localRootsVar publicRootsVar useLedgerVar)
              Nothing
#endif
        void $
          let diffusionArgumentsExtra =
                mkP2PArguments nc
                  (readTVar localRootsVar)
                  (readTVar publicRootsVar)
                  (readTVar useLedgerVar)
          in
          Node.run
            nodeArgs
            StdRunNodeArgs
              { srnBfcMaxConcurrencyBulkSync    = unMaxConcurrencyBulkSync <$> ncMaxConcurrencyBulkSync nc
              , srnBfcMaxConcurrencyDeadline    = unMaxConcurrencyDeadline <$> ncMaxConcurrencyDeadline nc
              , srnChainDbValidateOverride      = ncValidateDB nc
              , srnSnapshotInterval             = ncSnapshotInterval nc
              , srnDatabasePath                 = dbPath
              , srnDiffusionArguments           = diffusionArguments
              , srnDiffusionArgumentsExtra      = diffusionArgumentsExtra
              , srnDiffusionTracers             = diffusionTracers tracers
              , srnDiffusionTracersExtra        = diffusionTracersExtra tracers
              , srnEnableInDevelopmentVersions  = ncExperimentalProtocolsEnabled nc
              , srnTraceChainDB                 = chainDBTracer tracers
              , srnMaybeMempoolCapacityOverride = ncMaybeMempoolCapacityOverride nc
              }
      DisabledP2PMode -> do
#ifdef UNIX
        _ <- Signals.installHandler
              Signals.sigHUP
              (Signals.Catch $ do
                traceWith (startupTracer tracers) NetworkConfigUpdateUnsupported
              )
              Nothing
#endif
        nt <- TopologyNonP2P.readTopologyFileOrError nc
        let (ipProducerAddrs, dnsProducerAddrs) = producerAddressesNonP2P nt

            dnsProducers :: [DnsSubscriptionTarget]
            dnsProducers = [ DnsSubscriptionTarget (Text.encodeUtf8 addr) port v
                           | (NodeAddress (NodeHostDnsAddress addr) port, v) <- dnsProducerAddrs
                           ]

            ipProducers :: IPSubscriptionTarget
            ipProducers = IPSubscriptionTarget
                           [ toSockAddr (addr, port)
                           | (NodeAddress (NodeHostIPAddress addr) port) <- ipProducerAddrs
                           ]
                           (length ipProducerAddrs)
        void $
          Node.run
            nodeArgs
            StdRunNodeArgs
              { srnBfcMaxConcurrencyBulkSync   = unMaxConcurrencyBulkSync <$> ncMaxConcurrencyBulkSync nc
              , srnBfcMaxConcurrencyDeadline   = unMaxConcurrencyDeadline <$> ncMaxConcurrencyDeadline nc
              , srnChainDbValidateOverride     = ncValidateDB nc
              , srnSnapshotInterval            = ncSnapshotInterval nc
              , srnDatabasePath                = dbPath
              , srnDiffusionArguments          = diffusionArguments
              , srnDiffusionArgumentsExtra     = mkNonP2PArguments ipProducers dnsProducers
              , srnDiffusionTracers            = diffusionTracers tracers
              , srnDiffusionTracersExtra       = diffusionTracersExtra tracers
              , srnEnableInDevelopmentVersions = ncExperimentalProtocolsEnabled nc
              , srnTraceChainDB                = chainDBTracer tracers
              , srnMaybeMempoolCapacityOverride = ncMaybeMempoolCapacityOverride nc
              }
 where
  logStartupWarnings :: IO ()
  logStartupWarnings = do
    (case p2pMode of
      DisabledP2PMode -> return ()
      EnabledP2PMode  -> traceWith (startupTracer tracers) P2PWarning
      ) :: IO () -- annoying, but unavoidable for GADT type inference

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

#ifdef UNIX
  -- only used when P2P is enabled
  updateTopologyConfiguration :: StrictTVar IO [(Int, Map RelayAccessPoint PeerAdvertise)]
                              -> StrictTVar IO (Map RelayAccessPoint PeerAdvertise)
                              -> StrictTVar IO UseLedgerAfter
                              -> Signals.Handler
  updateTopologyConfiguration localRootsVar publicRootsVar useLedgerVar =
    Signals.Catch $ do
      traceWith (startupTracer tracers) NetworkConfigUpdate
      result <- try $ TopologyP2P.readTopologyFileOrError (startupTracer tracers) nc
      case result of
        Left (FatalError err) ->
          traceWith (startupTracer tracers)
                  $ NetworkConfigUpdateError
                  $ pack "Error reading topology configuration file:" <> err
        Right nt -> do
          let (localRoots, publicRoots) = producerAddresses nt
          traceWith (startupTracer tracers)
                  $ NetworkConfig localRoots publicRoots (useLedgerAfterSlot nt)
          atomically $ do
            writeTVar localRootsVar localRoots
            writeTVar publicRootsVar publicRoots
            writeTVar useLedgerVar (useLedgerAfterSlot nt)
#endif

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

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

canonDbPath :: NodeConfiguration -> IO FilePath
canonDbPath NodeConfiguration{ncDatabaseFile = DbFile dbFp} = do
  fp <- canonicalizePath =<< makeAbsolute dbFp
  createDirectoryIfMissing True fp
  return fp


-- | Make sure the VRF private key file is readable only
-- by the current process owner the node is running under.
checkVRFFilePermissions :: FilePath -> ExceptT VRFPrivateKeyFilePermissionError IO ()
#ifdef UNIX
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
#else
checkVRFFilePermissions vrfPrivKey = do
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


mkP2PArguments
  :: NodeConfiguration
  -> STM IO [(Int, Map RelayAccessPoint PeerAdvertise)]
     -- ^ non-overlapping local root peers groups; the 'Int' denotes the
     -- valency of its group.
  -> STM IO (Map RelayAccessPoint PeerAdvertise)
  -> STM IO UseLedgerAfter
  -> Diffusion.ExtraArguments 'Diffusion.P2P IO
mkP2PArguments NodeConfiguration {
                 ncTargetNumberOfRootPeers,
                 ncTargetNumberOfKnownPeers,
                 ncTargetNumberOfEstablishedPeers,
                 ncTargetNumberOfActivePeers,
                 ncProtocolIdleTimeout,
                 ncTimeWaitTimeout,
                 ncPeerSharing
               }
               daReadLocalRootPeers
               daReadPublicRootPeers
               daReadUseLedgerAfter =
    Diffusion.P2PArguments P2P.ArgumentsExtra
      { P2P.daPeerSelectionTargets
      , P2P.daReadLocalRootPeers
      , P2P.daReadPublicRootPeers
      , P2P.daReadUseLedgerAfter
      , P2P.daProtocolIdleTimeout   = ncProtocolIdleTimeout
      , P2P.daTimeWaitTimeout       = ncTimeWaitTimeout
      , P2P.daDeadlineChurnInterval = 3300
      , P2P.daBulkChurnInterval     = 300
      , P2P.daOwnPeerSharing        = ncPeerSharing
      }
  where
    daPeerSelectionTargets = PeerSelectionTargets {
        targetNumberOfRootPeers        = ncTargetNumberOfRootPeers,
        targetNumberOfKnownPeers       = ncTargetNumberOfKnownPeers,
        targetNumberOfEstablishedPeers = ncTargetNumberOfEstablishedPeers,
        targetNumberOfActivePeers      = ncTargetNumberOfActivePeers
    }

mkNonP2PArguments
  :: IPSubscriptionTarget
  -> [DnsSubscriptionTarget]
  -> Diffusion.ExtraArguments 'Diffusion.NonP2P m
mkNonP2PArguments daIpProducers daDnsProducers =
    Diffusion.NonP2PArguments NonP2P.ArgumentsExtra
      { NonP2P.daIpProducers
      , NonP2P.daDnsProducers
      }

-- | TODO: Only needed for enabling P2P switch
--
producerAddressesNonP2P
  :: TopologyNonP2P.NetworkTopology
  -> ( [NodeIPAddress]
     , [(NodeDnsAddress, Int)])
producerAddressesNonP2P nt =
  case nt of
    TopologyNonP2P.RealNodeTopology producers' ->
        partitionEithers
      . mapMaybe TopologyNonP2P.remoteAddressToNodeAddress
      $ producers'
    TopologyNonP2P.MockNodeTopology nodeSetup ->
        partitionEithers
      . mapMaybe TopologyNonP2P.remoteAddressToNodeAddress
      . concatMap TopologyNonP2P.producers
      $ nodeSetup

producerAddresses
  :: NetworkTopology
  -> ([(Int, Map RelayAccessPoint PeerAdvertise)], Map RelayAccessPoint PeerAdvertise)
producerAddresses nt =
  case nt of
    RealNodeTopology lrpg prp _ ->
      ( map (\lrp -> ( valency lrp
                     , Map.fromList $ rootConfigToRelayAccessPoint
                                    $ localRoots lrp
                     )
            )
            (groups lrpg)
      , foldMap (Map.fromList . rootConfigToRelayAccessPoint)
                (map publicRoots prp)
      )

useLedgerAfterSlot
  :: NetworkTopology
  -> UseLedgerAfter
useLedgerAfterSlot (RealNodeTopology _ _ (UseLedger ul)) = ul
