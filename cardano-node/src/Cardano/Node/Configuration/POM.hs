{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-noncanonical-monoid-instances #-}

module Cardano.Node.Configuration.POM
  ( NodeConfiguration (..)
  , NetworkP2PMode (..)
  , SomeNetworkP2PMode (..)
  , PartialNodeConfiguration(..)
  , TimeoutOverride (..)
  , defaultPartialNodeConfiguration
  , lastOption
  , makeNodeConfiguration
  , parseNodeConfigurationFP
  , pncProtocol
  , ncProtocol
  )
where

import           Cardano.Crypto (RequiresNetworkMagic (..))
import           Cardano.Logging.Types
import           Cardano.Node.Configuration.NodeAddress (SocketPath)
import           Cardano.Node.Configuration.Socket (SocketConfig (..))
import           Cardano.Node.Handlers.Shutdown
import           Cardano.Node.Protocol.Types (Protocol (..))
import           Cardano.Node.Types
import           Cardano.Tracing.Config
import           Cardano.Tracing.OrphanInstances.Network ()
import           Ouroboros.Consensus.Mempool (MempoolCapacityBytes (..),
                   MempoolCapacityBytesOverride (..))
import qualified Ouroboros.Consensus.Node as Consensus (NetworkP2PMode (..))
import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy (NumOfDiskSnapshots (..),
                   SnapshotInterval (..))
import           Ouroboros.Network.NodeToNode (AcceptedConnectionsLimit (..), DiffusionMode (..))
import           Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))

import           Control.Monad (when)
import           Data.Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Bifunctor (Bifunctor (..))
import           Data.Maybe
import           Data.Monoid (Last (..))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time.Clock (DiffTime)
import           Data.Yaml (decodeFileThrow)
import           GHC.Generics (Generic)
import           Options.Applicative
import           System.FilePath (takeDirectory, (</>))

import           Generic.Data (gmappend)
import           Generic.Data.Orphans ()

data NetworkP2PMode = EnabledP2PMode | DisabledP2PMode
  deriving (Eq, Show, Generic)

data SomeNetworkP2PMode where
    SomeNetworkP2PMode :: forall p2p.
                          Consensus.NetworkP2PMode p2p
                       -> SomeNetworkP2PMode

instance Eq SomeNetworkP2PMode where
    (==) (SomeNetworkP2PMode Consensus.EnabledP2PMode)
         (SomeNetworkP2PMode Consensus.EnabledP2PMode)
       = True
    (==) (SomeNetworkP2PMode Consensus.DisabledP2PMode)
         (SomeNetworkP2PMode Consensus.DisabledP2PMode)
       = True
    (==) _ _
       = False

instance Show SomeNetworkP2PMode where
    show (SomeNetworkP2PMode mode@Consensus.EnabledP2PMode)  = show mode
    show (SomeNetworkP2PMode mode@Consensus.DisabledP2PMode) = show mode

-- | Isomorphic to a `Maybe DiffTime`, but expresses what `Nothing` means, in
-- this case that we want to /NOT/ override the default timeout.
data TimeoutOverride = NoTimeoutOverride | TimeoutOverride DiffTime
  deriving (Eq, Show)

data NodeConfiguration
  = NodeConfiguration
      {  ncSocketConfig    :: !SocketConfig
           -- | Filepath of the configuration yaml file. This file determines
          -- all the configuration settings required for the cardano node
          -- (logging, tracing, protocol, slot length etc)
       , ncConfigFile      :: !ConfigYamlFilePath
       , ncTopologyFile    :: !TopologyFile
       , ncDatabaseFile    :: !DbFile
       , ncProtocolFiles   :: !ProtocolFilepaths
       , ncValidateDB      :: !Bool
       , ncShutdownConfig  :: !ShutdownConfig

       , ncStartAsNonProducingNode :: !Bool

        -- Protocol-specific parameters:
       , ncProtocolConfig :: !NodeProtocolConfiguration

         -- Node parameters, not protocol-specific:
       , ncDiffusionMode      :: !DiffusionMode
       , ncNumOfDiskSnapshots :: !NumOfDiskSnapshots
       , ncSnapshotInterval   :: !SnapshotInterval

         -- | During the development and integration of new network protocols
         -- (node-to-node and node-to-client) we wish to be able to test them
         -- but not have everybody use them by default on the mainnet. Avoiding
         -- enabling them by default makes it practical to include such
         -- not-yet-ready protocol versions into released versions of the node
         -- without the danger that node operators on the mainnet will start
         -- using them prematurely, before the testing is complete.
         --
         -- The flag defaults to 'False'
         --
         -- This flag should be set to 'True' when testing the new protocol
         -- versions.
       , ncExperimentalProtocolsEnabled :: !Bool

         -- BlockFetch configuration
       , ncMaxConcurrencyBulkSync :: !(Maybe MaxConcurrencyBulkSync)
       , ncMaxConcurrencyDeadline :: !(Maybe MaxConcurrencyDeadline)

         -- Logging parameters:
       , ncLoggingSwitch  :: !Bool
       , ncLogMetrics     :: !Bool
       , ncTraceConfig    :: !TraceOptions
       , ncTraceForwardSocket :: !(Maybe (SocketPath, ForwarderMode))

       , ncMaybeMempoolCapacityOverride :: !(Maybe MempoolCapacityBytesOverride)

         -- | Protocol idleness timeout, see
         -- 'Ouroboros.Network.Diffusion.daProtocolIdleTimeout'.
         --
       , ncProtocolIdleTimeout   :: DiffTime
         -- | Wait time timeout, see
         -- 'Ouroboros.Network.Diffusion.daTimeWaitTimeout'.
         --
       , ncTimeWaitTimeout       :: DiffTime

         -- | Timeout override for ChainSync, see
         -- 'Ouroboros.Network.Protocol.ChainSync.Codec.ChainSyncTimeout'
       , ncChainSyncIdleTimeout :: TimeoutOverride

         -- | Node AcceptedConnectionsLimit
       , ncAcceptedConnectionsLimit :: !AcceptedConnectionsLimit

         -- P2P governor targets
       , ncTargetNumberOfRootPeers        :: Int
       , ncTargetNumberOfKnownPeers       :: Int
       , ncTargetNumberOfEstablishedPeers :: Int
       , ncTargetNumberOfActivePeers      :: Int
       , ncTargetNumberOfKnownBigLedgerPeers       :: Int
       , ncTargetNumberOfEstablishedBigLedgerPeers :: Int
       , ncTargetNumberOfActiveBigLedgerPeers      :: Int

         -- Enable experimental P2P mode
       , ncEnableP2P :: SomeNetworkP2PMode

         -- Enable Peer Sharing
       , ncPeerSharing :: PeerSharing
       } deriving (Eq, Show)


data PartialNodeConfiguration
  = PartialNodeConfiguration
      {  pncSocketConfig    :: !(Last SocketConfig)
         -- | Filepath of the configuration yaml file. This file determines
         -- all the configuration settings required for the cardano node
         -- (logging, tracing, protocol, slot length etc)
       , pncConfigFile      :: !(Last ConfigYamlFilePath)
       , pncTopologyFile    :: !(Last TopologyFile)
       , pncDatabaseFile    :: !(Last DbFile)
       , pncProtocolFiles   :: !(Last ProtocolFilepaths)
       , pncValidateDB      :: !(Last Bool)
       , pncShutdownConfig  :: !(Last ShutdownConfig)

       , pncStartAsNonProducingNode :: !(Last Bool)

         -- Protocol-specific parameters:
       , pncProtocolConfig :: !(Last NodeProtocolConfiguration)

         -- Node parameters, not protocol-specific:
       , pncDiffusionMode      :: !(Last DiffusionMode  )
       , pncNumOfDiskSnapshots :: !(Last NumOfDiskSnapshots)
       , pncSnapshotInterval   :: !(Last SnapshotInterval)
       , pncExperimentalProtocolsEnabled :: !(Last Bool)

         -- BlockFetch configuration
       , pncMaxConcurrencyBulkSync :: !(Last MaxConcurrencyBulkSync)
       , pncMaxConcurrencyDeadline :: !(Last MaxConcurrencyDeadline)

         -- Logging parameters:
       , pncLoggingSwitch  :: !(Last Bool)
       , pncLogMetrics     :: !(Last Bool)
       , pncTraceConfig    :: !(Last PartialTraceOptions)
       , pncTraceForwardSocket :: !(Last (SocketPath, ForwarderMode))

         -- Configuration for testing purposes
       , pncMaybeMempoolCapacityOverride :: !(Last MempoolCapacityBytesOverride)

         -- Network timeouts
       , pncProtocolIdleTimeout   :: !(Last DiffTime)
       , pncTimeWaitTimeout       :: !(Last DiffTime)

       , pncChainSyncIdleTimeout      :: !(Last DiffTime)

         -- AcceptedConnectionsLimit
       , pncAcceptedConnectionsLimit :: !(Last AcceptedConnectionsLimit)

         -- P2P governor targets
       , pncTargetNumberOfRootPeers        :: !(Last Int)
       , pncTargetNumberOfKnownPeers       :: !(Last Int)
       , pncTargetNumberOfEstablishedPeers :: !(Last Int)
       , pncTargetNumberOfActivePeers      :: !(Last Int)
       , pncTargetNumberOfKnownBigLedgerPeers       :: !(Last Int)
       , pncTargetNumberOfEstablishedBigLedgerPeers :: !(Last Int)
       , pncTargetNumberOfActiveBigLedgerPeers      :: !(Last Int)

         -- Enable experimental P2P mode
       , pncEnableP2P :: !(Last NetworkP2PMode)

         -- Peer Sharing
       , pncPeerSharing :: !(Last PeerSharing)
       } deriving (Eq, Generic, Show)

instance AdjustFilePaths PartialNodeConfiguration where
  adjustFilePaths f x =
    x { pncProtocolConfig = adjustFilePaths f (pncProtocolConfig x)
      , pncSocketConfig   = adjustFilePaths f (pncSocketConfig x)
      }

instance Semigroup PartialNodeConfiguration where
  (<>) = gmappend

instance FromJSON PartialNodeConfiguration where
  parseJSON =
    withObject "PartialNodeConfiguration" $ \v -> do

      -- Node parameters, not protocol-specific
      pncSocketPath <- Last <$> v .:? "SocketPath"
      pncDiffusionMode
        <- Last . fmap getDiffusionMode <$> v .:? "DiffusionMode"
      pncNumOfDiskSnapshots
        <- Last . fmap RequestedNumOfDiskSnapshots <$> v .:? "NumOfDiskSnapshots"
      pncSnapshotInterval
        <- Last . fmap RequestedSnapshotInterval <$> v .:? "SnapshotInterval"
      pncExperimentalProtocolsEnabled <- fmap Last $ do
        mValue <- v .:? "ExperimentalProtocolsEnabled"

        mOldValue <- v .:? "TestEnableDevelopmentNetworkProtocols"

        when (isJust mOldValue) $ do
          when (mOldValue /= mValue) $
            fail "TestEnableDevelopmentNetworkProtocols has been renamed to ExperimentalProtocolsEnabled in the configuration file"

        pure mValue

      -- Blockfetch parameters
      pncMaxConcurrencyBulkSync <- Last <$> v .:? "MaxConcurrencyBulkSync"
      pncMaxConcurrencyDeadline <- Last <$> v .:? "MaxConcurrencyDeadline"

      -- Logging parameters
      pncLoggingSwitch'  <-                 v .:? "TurnOnLogging" .!= True
      pncLogMetrics      <- Last        <$> v .:? "TurnOnLogMetrics"
      useTraceDispatcher <-                 v .:? "UseTraceDispatcher" .!= False
      pncTraceConfig     <-  if pncLoggingSwitch'
                             then do
                               partialTraceSelection <- parseJSON $ Object v
                               if useTraceDispatcher
                               then return $ Last $ Just $ PartialTraceDispatcher partialTraceSelection
                               else return $ Last $ Just $ PartialTracingOnLegacy partialTraceSelection
                             else return $ Last $ Just PartialTracingOff

      -- Protocol parameters
      protocol <-  v .:? "Protocol" .!= CardanoProtocol
      pncProtocolConfig <-
        case protocol of
          CardanoProtocol ->
            fmap (Last . Just) $
              NodeProtocolConfigurationCardano
                <$> parseByronProtocol v
                <*> parseShelleyProtocol v
                <*> parseAlonzoProtocol v
                <*> parseConwayProtocol v
                <*> parseHardForkProtocol v
      pncMaybeMempoolCapacityOverride <- Last <$> parseMempoolCapacityBytesOverride v

      -- Network timeouts
      pncProtocolIdleTimeout   <- Last <$> v .:? "ProtocolIdleTimeout"
      pncTimeWaitTimeout       <- Last <$> v .:? "TimeWaitTimeout"


      -- AcceptedConnectionsLimit
      pncAcceptedConnectionsLimit
        <- Last <$> v .:? "AcceptedConnectionsLimit"

      -- P2P Governor parameters, with conservative defaults.
      pncTargetNumberOfRootPeers        <- Last <$> v .:? "TargetNumberOfRootPeers"
      pncTargetNumberOfKnownPeers       <- Last <$> v .:? "TargetNumberOfKnownPeers"
      pncTargetNumberOfEstablishedPeers <- Last <$> v .:? "TargetNumberOfEstablishedPeers"
      pncTargetNumberOfActivePeers      <- Last <$> v .:? "TargetNumberOfActivePeers"
      pncTargetNumberOfKnownBigLedgerPeers       <- Last <$> v .:? "TargetNumberOfKnownBigLedgerPeers"
      pncTargetNumberOfEstablishedBigLedgerPeers <- Last <$> v .:? "TargetNumberOfEstablishedBigLedgerPeers"
      pncTargetNumberOfActiveBigLedgerPeers      <- Last <$> v .:? "TargetNumberOfActiveBigLedgerPeers"

      pncChainSyncIdleTimeout      <- Last <$> v .:? "ChainSyncIdleTimeout"

      -- Enable P2P switch
      p2pSwitch <- v .:? "EnableP2P" .!= Just False
      let pncEnableP2P =
            case p2pSwitch of
              Nothing    -> mempty
              Just False -> Last $ Just DisabledP2PMode
              Just True  -> Last $ Just EnabledP2PMode

      -- Peer Sharing
      -- DISABLED BY DEFAULT
      pncPeerSharing <- Last <$> v .:? "PeerSharing" .!= Just PeerSharingDisabled

      pure PartialNodeConfiguration {
             pncProtocolConfig
           , pncSocketConfig = Last . Just $ SocketConfig mempty mempty mempty pncSocketPath
           , pncDiffusionMode
           , pncNumOfDiskSnapshots
           , pncSnapshotInterval
           , pncExperimentalProtocolsEnabled
           , pncMaxConcurrencyBulkSync
           , pncMaxConcurrencyDeadline
           , pncLoggingSwitch = Last $ Just pncLoggingSwitch'
           , pncLogMetrics
           , pncTraceConfig
           , pncTraceForwardSocket = mempty
           , pncConfigFile = mempty
           , pncTopologyFile = mempty
           , pncDatabaseFile = mempty
           , pncProtocolFiles = mempty
           , pncValidateDB = mempty
           , pncShutdownConfig = mempty
           , pncStartAsNonProducingNode = Last $ Just False
           , pncMaybeMempoolCapacityOverride
           , pncProtocolIdleTimeout
           , pncTimeWaitTimeout
           , pncChainSyncIdleTimeout
           , pncAcceptedConnectionsLimit
           , pncTargetNumberOfRootPeers
           , pncTargetNumberOfKnownPeers
           , pncTargetNumberOfEstablishedPeers
           , pncTargetNumberOfActivePeers
           , pncTargetNumberOfKnownBigLedgerPeers
           , pncTargetNumberOfEstablishedBigLedgerPeers
           , pncTargetNumberOfActiveBigLedgerPeers
           , pncEnableP2P
           , pncPeerSharing
           }
    where
      parseMempoolCapacityBytesOverride v = parseNoOverride <|> parseOverride
        where
          parseNoOverride = fmap (MempoolCapacityBytesOverride . MempoolCapacityBytes) <$> v .:? "MempoolCapacityBytesOverride"
          parseOverride = do
            maybeString :: Maybe String <- v .:? "MempoolCapacityBytesOverride"
            case maybeString of
              Just "NoOverride" -> return (Just NoMempoolCapacityBytesOverride)
              Just invalid ->  fmap Just . Aeson.parseFail $ mconcat
                [ "Invalid value for 'MempoolCapacityBytesOverride'.  "
                , "Expecting byte count or NoOverride.  Value was: "
                , show invalid
                ]
              Nothing -> return Nothing
      parseByronProtocol v = do
        primary   <- v .:? "ByronGenesisFile"
        secondary <- v .:? "GenesisFile"
        npcByronGenesisFile <-
          case (primary, secondary) of
            (Just g, Nothing)  -> return g
            (Nothing, Just g)  -> return g
            (Nothing, Nothing) -> fail $ "Missing required field, either "
                                      ++ "ByronGenesisFile or GenesisFile"
            (Just _, Just _)   -> fail $ "Specify either ByronGenesisFile"
                                      ++ "or GenesisFile, but not both"
        npcByronGenesisFileHash <- v .:? "ByronGenesisHash"

        npcByronReqNetworkMagic     <- v .:? "RequiresNetworkMagic"
                                         .!= RequiresNoMagic
        npcByronPbftSignatureThresh <- v .:? "PBftSignatureThreshold"
        protVerMajor                <- v .: "LastKnownBlockVersion-Major"
        protVerMinor                <- v .: "LastKnownBlockVersion-Minor"
        protVerAlt                  <- v .: "LastKnownBlockVersion-Alt" .!= 0

        pure NodeByronProtocolConfiguration {
               npcByronGenesisFile
             , npcByronGenesisFileHash
             , npcByronReqNetworkMagic
             , npcByronPbftSignatureThresh
             , npcByronSupportedProtocolVersionMajor = protVerMajor
             , npcByronSupportedProtocolVersionMinor = protVerMinor
             , npcByronSupportedProtocolVersionAlt   = protVerAlt
             }

      parseShelleyProtocol v = do
        primary   <- v .:? "ShelleyGenesisFile"
        secondary <- v .:? "GenesisFile"
        npcShelleyGenesisFile <-
          case (primary, secondary) of
            (Just g, Nothing)  -> return g
            (Nothing, Just g)  -> return g
            (Nothing, Nothing) -> fail $ "Missing required field, either "
                                      ++ "ShelleyGenesisFile or GenesisFile"
            (Just _, Just _)   -> fail $ "Specify either ShelleyGenesisFile"
                                      ++ "or GenesisFile, but not both"
        npcShelleyGenesisFileHash <- v .:? "ShelleyGenesisHash"

        pure NodeShelleyProtocolConfiguration {
               npcShelleyGenesisFile
             , npcShelleyGenesisFileHash
             }

      parseAlonzoProtocol v = do
        npcAlonzoGenesisFile     <- v .:  "AlonzoGenesisFile"
        npcAlonzoGenesisFileHash <- v .:? "AlonzoGenesisHash"
        pure NodeAlonzoProtocolConfiguration {
               npcAlonzoGenesisFile
             , npcAlonzoGenesisFileHash
             }

      parseConwayProtocol v = do
        npcConwayGenesisFile     <- v .:  "ConwayGenesisFile"
        npcConwayGenesisFileHash <- v .:? "ConwayGenesisHash"
        pure NodeConwayProtocolConfiguration {
               npcConwayGenesisFile
             , npcConwayGenesisFileHash
             }

      parseHardForkProtocol v = do

        npcExperimentalHardForksEnabled <- do
          mValue <- v .:? "ExperimentalHardForksEnabled"

          mOldValue <- v .:? "TestEnableDevelopmentHardForkEras"

          when (isJust mOldValue) $ do
            when (mOldValue /= mValue) $
              fail "TestEnableDevelopmentHardForkEras has been renamed to ExperimentalHardForksEnabled in the configuration file"

          pure (fromMaybe False mValue)

        npcTestShelleyHardForkAtEpoch   <- v .:? "TestShelleyHardForkAtEpoch"
        npcTestShelleyHardForkAtVersion <- v .:? "TestShelleyHardForkAtVersion"

        npcTestAllegraHardForkAtEpoch   <- v .:? "TestAllegraHardForkAtEpoch"
        npcTestAllegraHardForkAtVersion <- v .:? "TestAllegraHardForkAtVersion"

        npcTestMaryHardForkAtEpoch   <- v .:? "TestMaryHardForkAtEpoch"
        npcTestMaryHardForkAtVersion <- v .:? "TestMaryHardForkAtVersion"

        npcTestAlonzoHardForkAtEpoch   <- v .:? "TestAlonzoHardForkAtEpoch"
        npcTestAlonzoHardForkAtVersion <- v .:? "TestAlonzoHardForkAtVersion"

        npcTestBabbageHardForkAtEpoch   <- v .:? "TestBabbageHardForkAtEpoch"
        npcTestBabbageHardForkAtVersion <- v .:? "TestBabbageHardForkAtVersion"

        npcTestConwayHardForkAtEpoch   <- v .:? "TestConwayHardForkAtEpoch"
        npcTestConwayHardForkAtVersion <- v .:? "TestConwayHardForkAtVersion"

        pure NodeHardForkProtocolConfiguration
          { npcExperimentalHardForksEnabled

          , npcTestShelleyHardForkAtEpoch
          , npcTestShelleyHardForkAtVersion

          , npcTestAllegraHardForkAtEpoch
          , npcTestAllegraHardForkAtVersion

          , npcTestMaryHardForkAtEpoch
          , npcTestMaryHardForkAtVersion

          , npcTestAlonzoHardForkAtEpoch
          , npcTestAlonzoHardForkAtVersion

          , npcTestBabbageHardForkAtEpoch
          , npcTestBabbageHardForkAtVersion

          , npcTestConwayHardForkAtEpoch
          , npcTestConwayHardForkAtVersion
          }

-- | Default configuration is mainnet
defaultPartialNodeConfiguration :: PartialNodeConfiguration
defaultPartialNodeConfiguration =
  PartialNodeConfiguration
    { pncConfigFile = Last . Just $ ConfigYamlFilePath "configuration/cardano/mainnet-config.json"
    , pncDatabaseFile = Last . Just $ DbFile "mainnet/db/"
    , pncLoggingSwitch = Last $ Just True
    , pncSocketConfig = Last . Just $ SocketConfig mempty mempty mempty mempty
    , pncDiffusionMode = Last $ Just InitiatorAndResponderDiffusionMode
    , pncNumOfDiskSnapshots = Last $ Just DefaultNumOfDiskSnapshots
    , pncSnapshotInterval = Last $ Just DefaultSnapshotInterval
    , pncExperimentalProtocolsEnabled = Last $ Just False
    , pncTopologyFile = Last . Just $ TopologyFile "configuration/cardano/mainnet-topology.json"
    , pncProtocolFiles = mempty
    , pncValidateDB = Last $ Just False
    , pncShutdownConfig = Last . Just $ ShutdownConfig Nothing Nothing
    , pncStartAsNonProducingNode = Last $ Just False
    , pncProtocolConfig = mempty
    , pncMaxConcurrencyBulkSync = mempty
    , pncMaxConcurrencyDeadline = mempty
    , pncLogMetrics = mempty
    , pncTraceConfig = mempty
    , pncTraceForwardSocket = mempty
    , pncMaybeMempoolCapacityOverride = mempty
    , pncProtocolIdleTimeout   = Last (Just 5)
    , pncTimeWaitTimeout       = Last (Just 60)
    , pncAcceptedConnectionsLimit =
        Last
      $ Just
      $ AcceptedConnectionsLimit
        { acceptedConnectionsHardLimit = 512
        , acceptedConnectionsSoftLimit = 384
        , acceptedConnectionsDelay     = 5
        }
    , pncTargetNumberOfRootPeers        = Last (Just 85)
    , pncTargetNumberOfKnownPeers       = Last (Just 85)
    , pncTargetNumberOfEstablishedPeers = Last (Just 40)
    , pncTargetNumberOfActivePeers      = Last (Just 15)
    , pncChainSyncIdleTimeout           = mempty
    , pncTargetNumberOfKnownBigLedgerPeers       = Last (Just 15)
    , pncTargetNumberOfEstablishedBigLedgerPeers = Last (Just 10)
    , pncTargetNumberOfActiveBigLedgerPeers      = Last (Just 5)
    , pncEnableP2P                      = Last (Just DisabledP2PMode)
    , pncPeerSharing                    = Last (Just PeerSharingDisabled)
    }

lastOption :: Parser a -> Parser (Last a)
lastOption = fmap Last . optional

makeNodeConfiguration :: PartialNodeConfiguration -> Either String NodeConfiguration
makeNodeConfiguration pnc = do
  configFile <- lastToEither "Missing YAML config file" $ pncConfigFile pnc
  topologyFile <- lastToEither "Missing TopologyFile" $ pncTopologyFile pnc
  databaseFile <- lastToEither "Missing DatabaseFile" $ pncDatabaseFile pnc
  validateDB <- lastToEither "Missing ValidateDB" $ pncValidateDB pnc
  startAsNonProducingNode <- lastToEither "Missing StartAsNonProducingNode" $ pncStartAsNonProducingNode pnc
  protocolConfig <- lastToEither "Missing ProtocolConfig" $ pncProtocolConfig pnc
  loggingSwitch <- lastToEither "Missing LoggingSwitch" $ pncLoggingSwitch pnc
  logMetrics <- lastToEither "Missing LogMetrics" $ pncLogMetrics pnc
  traceConfig <- first Text.unpack $ partialTraceSelectionToEither $ pncTraceConfig pnc
  diffusionMode <- lastToEither "Missing DiffusionMode" $ pncDiffusionMode pnc
  numOfDiskSnapshots <- lastToEither "Missing NumOfDiskSnapshots" $ pncNumOfDiskSnapshots pnc
  snapshotInterval <- lastToEither "Missing SnapshotInterval" $ pncSnapshotInterval pnc
  shutdownConfig <- lastToEither "Missing ShutdownConfig" $ pncShutdownConfig pnc
  socketConfig <- lastToEither "Missing SocketConfig" $ pncSocketConfig pnc

  ncTargetNumberOfRootPeers <-
    lastToEither "Missing TargetNumberOfRootPeers"
    $ pncTargetNumberOfRootPeers pnc
  ncTargetNumberOfKnownPeers <-
    lastToEither "Missing TargetNumberOfKnownPeers"
    $ pncTargetNumberOfKnownPeers pnc
  ncTargetNumberOfEstablishedPeers <-
    lastToEither "Missing TargetNumberOfEstablishedPeers"
    $ pncTargetNumberOfEstablishedPeers pnc
  ncTargetNumberOfActivePeers <-
    lastToEither "Missing TargetNumberOfActivePeers"
    $ pncTargetNumberOfActivePeers pnc
  ncTargetNumberOfKnownBigLedgerPeers <-
    lastToEither "Missing TargetNumberOfKnownBigLedgerPeers"
    $ pncTargetNumberOfKnownBigLedgerPeers pnc
  ncTargetNumberOfEstablishedBigLedgerPeers <-
    lastToEither "Missing TargetNumberOfEstablishedBigLedgerPeers"
    $ pncTargetNumberOfEstablishedBigLedgerPeers pnc
  ncTargetNumberOfActiveBigLedgerPeers <-
    lastToEither "Missing TargetNumberOfActiveBigLedgerPeers"
    $ pncTargetNumberOfActiveBigLedgerPeers pnc
  ncProtocolIdleTimeout <-
    lastToEither "Missing ProtocolIdleTimeout"
    $ pncProtocolIdleTimeout pnc
  ncTimeWaitTimeout <-
    lastToEither "Missing TimeWaitTimeout"
    $ pncTimeWaitTimeout pnc
  ncAcceptedConnectionsLimit <-
    lastToEither "Missing AcceptedConnectionsLimit" $
      pncAcceptedConnectionsLimit pnc
  enableP2P <-
    lastToEither "Missing EnableP2P"
    $ pncEnableP2P pnc
  ncChainSyncIdleTimeout <-
    Right
    $ maybe NoTimeoutOverride TimeoutOverride
    $ getLast
    $ pncChainSyncIdleTimeout pnc

  ncPeerSharing <-
    lastToEither "Missing PeerSharing"
    $ pncPeerSharing pnc

  -- TODO: This is not mandatory
  experimentalProtocols <-
    lastToEither "Missing ExperimentalProtocolsEnabled" $
      pncExperimentalProtocolsEnabled pnc
  return $ NodeConfiguration
             { ncConfigFile = configFile
             , ncTopologyFile = topologyFile
             , ncDatabaseFile = databaseFile
             , ncProtocolFiles =
                 -- TODO: ncProtocolFiles should be Maybe ProtocolFiles
                 -- as relay nodes don't need the protocol files because
                 -- they are not minting blocks.
                 case getLast $ pncProtocolFiles pnc of
                   Just pFiles -> pFiles
                   Nothing -> ProtocolFilepaths Nothing Nothing Nothing Nothing Nothing Nothing
             , ncValidateDB = validateDB
             , ncShutdownConfig = shutdownConfig
             , ncStartAsNonProducingNode = startAsNonProducingNode
             , ncProtocolConfig = protocolConfig
             , ncSocketConfig = socketConfig
             , ncDiffusionMode = diffusionMode
             , ncNumOfDiskSnapshots = numOfDiskSnapshots
             , ncSnapshotInterval = snapshotInterval
             , ncExperimentalProtocolsEnabled = experimentalProtocols
             , ncMaxConcurrencyBulkSync = getLast $ pncMaxConcurrencyBulkSync pnc
             , ncMaxConcurrencyDeadline = getLast $ pncMaxConcurrencyDeadline pnc
             , ncLoggingSwitch = loggingSwitch
             , ncLogMetrics = logMetrics
             , ncTraceConfig = if loggingSwitch then traceConfig
                                                else TracingOff
             , ncTraceForwardSocket = getLast $ pncTraceForwardSocket pnc
             , ncMaybeMempoolCapacityOverride = getLast $ pncMaybeMempoolCapacityOverride pnc
             , ncProtocolIdleTimeout
             , ncTimeWaitTimeout
             , ncChainSyncIdleTimeout
             , ncAcceptedConnectionsLimit
             , ncTargetNumberOfRootPeers
             , ncTargetNumberOfKnownPeers
             , ncTargetNumberOfEstablishedPeers
             , ncTargetNumberOfActivePeers
             , ncTargetNumberOfKnownBigLedgerPeers
             , ncTargetNumberOfEstablishedBigLedgerPeers
             , ncTargetNumberOfActiveBigLedgerPeers
             , ncEnableP2P = case enableP2P of
                 EnabledP2PMode  -> SomeNetworkP2PMode Consensus.EnabledP2PMode
                 DisabledP2PMode -> SomeNetworkP2PMode Consensus.DisabledP2PMode
             , ncPeerSharing
             }

ncProtocol :: NodeConfiguration -> Protocol
ncProtocol nc =
  case ncProtocolConfig nc of
    -- NodeProtocolConfigurationByron{}   -> ByronProtocol -- jky delete me
    -- NodeProtocolConfigurationShelley{} -> ShelleyProtocol -- jky delete me
    NodeProtocolConfigurationCardano{} -> CardanoProtocol

pncProtocol :: PartialNodeConfiguration -> Either Text Protocol
pncProtocol pnc =
  case pncProtocolConfig pnc of
    Last Nothing -> Left "Node protocol configuration not found"
    Last (Just NodeProtocolConfigurationCardano{}) -> Right CardanoProtocol

parseNodeConfigurationFP :: Maybe ConfigYamlFilePath -> IO PartialNodeConfiguration
parseNodeConfigurationFP Nothing = parseNodeConfigurationFP . getLast $ pncConfigFile defaultPartialNodeConfiguration
parseNodeConfigurationFP (Just (ConfigYamlFilePath fp)) = do
    nc <- decodeFileThrow fp
    -- Make all the files be relative to the location of the config file.
    pure $ adjustFilePaths (takeDirectory fp </>) nc
