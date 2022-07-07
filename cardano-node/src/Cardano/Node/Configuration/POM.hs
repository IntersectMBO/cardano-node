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
  , defaultPartialNodeConfiguration
  , lastOption
  , makeNodeConfiguration
  , parseNodeConfigurationFP
  , pncProtocol
  , ncProtocol
  )
where

import           Cardano.Prelude
import qualified GHC.Show as Show
import           Prelude (String)

import           Control.Monad (fail)
import           Data.Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as Text
import           Data.Time.Clock (DiffTime)
import           Data.Yaml (decodeFileThrow)
import           Generic.Data (gmappend)
import           Generic.Data.Orphans ()
import           Options.Applicative
import           System.FilePath (takeDirectory, (</>))

import qualified Cardano.Chain.Update as Byron
import           Cardano.Crypto (RequiresNetworkMagic (..))
import           Cardano.Logging.Types
import           Cardano.Node.Configuration.NodeAddress (SocketPath)
import           Cardano.Node.Configuration.Socket (SocketConfig (..))
import           Cardano.Node.Handlers.Shutdown
import           Cardano.Node.Protocol.Types (Protocol (..))
import           Cardano.Node.Types
import           Cardano.Tracing.Config
import           Ouroboros.Consensus.Mempool.API (MempoolCapacityBytes (..),
                   MempoolCapacityBytesOverride (..))
import qualified Ouroboros.Consensus.Node as Consensus (NetworkP2PMode (..))
import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy (SnapshotInterval (..))
import           Ouroboros.Network.NodeToNode (AcceptedConnectionsLimit (..), DiffusionMode (..))

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

        -- Protocol-specific parameters:
       , ncProtocolConfig :: !NodeProtocolConfiguration

         -- Node parameters, not protocol-specific:
       , ncDiffusionMode    :: !DiffusionMode
       , ncSnapshotInterval :: !SnapshotInterval

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
       , ncTestEnableDevelopmentNetworkProtocols :: !Bool

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

         -- | Node AcceptedConnectionsLimit
       , ncAcceptedConnectionsLimit :: !AcceptedConnectionsLimit

         -- P2P governor targets
       , ncTargetNumberOfRootPeers        :: Int
       , ncTargetNumberOfKnownPeers       :: Int
       , ncTargetNumberOfEstablishedPeers :: Int
       , ncTargetNumberOfActivePeers      :: Int

         -- Enable experimental P2P mode
       , ncEnableP2P :: SomeNetworkP2PMode
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

         -- Protocol-specific parameters:
       , pncProtocolConfig :: !(Last NodeProtocolConfiguration)

         -- Node parameters, not protocol-specific:
       , pncDiffusionMode    :: !(Last DiffusionMode)
       , pncSnapshotInterval :: !(Last SnapshotInterval)
       , pncTestEnableDevelopmentNetworkProtocols :: !(Last Bool)

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

         -- AcceptedConnectionsLimit
       , pncAcceptedConnectionsLimit :: !(Last AcceptedConnectionsLimit)

         -- P2P governor targets
       , pncTargetNumberOfRootPeers        :: !(Last Int)
       , pncTargetNumberOfKnownPeers       :: !(Last Int)
       , pncTargetNumberOfEstablishedPeers :: !(Last Int)
       , pncTargetNumberOfActivePeers      :: !(Last Int)

         -- Enable experimental P2P mode
       , pncEnableP2P :: !(Last NetworkP2PMode)
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
      pncSnapshotInterval
        <- Last . fmap RequestedSnapshotInterval <$> v .:? "SnapshotInterval"
      pncTestEnableDevelopmentNetworkProtocols
        <- Last <$> v .:? "TestEnableDevelopmentNetworkProtocols"

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
                               then Last . Just <$> return (PartialTraceDispatcher partialTraceSelection)
                               else Last . Just <$> return (PartialTracingOnLegacy partialTraceSelection)
                             else Last . Just <$> return PartialTracingOff

      -- Protocol parameters
      protocol <-  v .:? "Protocol" .!= ByronProtocol
      pncProtocolConfig <-
        case protocol of
          ByronProtocol ->
            Last . Just . NodeProtocolConfigurationByron <$> parseByronProtocol v

          ShelleyProtocol ->
            Last . Just . NodeProtocolConfigurationShelley <$> parseShelleyProtocol v

          CardanoProtocol ->
            Last . Just  <$> (NodeProtocolConfigurationCardano <$> parseByronProtocol v
                                                               <*> parseShelleyProtocol v
                                                               <*> parseAlonzoProtocol v
                                                               <*> parseHardForkProtocol v)
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

      -- Enable P2P switch
      p2pSwitch <- v .:? "EnableP2P" .!= Just False
      let pncEnableP2P =
            case p2pSwitch of
              Nothing    -> mempty
              Just False -> Last $ Just DisabledP2PMode
              Just True  -> Last $ Just EnabledP2PMode

      pure PartialNodeConfiguration {
             pncProtocolConfig
           , pncSocketConfig = Last . Just $ SocketConfig mempty mempty mempty pncSocketPath
           , pncDiffusionMode
           , pncSnapshotInterval
           , pncTestEnableDevelopmentNetworkProtocols
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
           , pncMaybeMempoolCapacityOverride
           , pncProtocolIdleTimeout
           , pncTimeWaitTimeout
           , pncAcceptedConnectionsLimit
           , pncTargetNumberOfRootPeers
           , pncTargetNumberOfKnownPeers
           , pncTargetNumberOfEstablishedPeers
           , pncTargetNumberOfActivePeers
           , pncEnableP2P
           }
    where
      parseMempoolCapacityBytesOverride v = parseNoOverride <|> parseOverride
        where
          parseNoOverride = fmap (MempoolCapacityBytesOverride . MempoolCapacityBytes) <$> v .:? "MempoolCapacityBytesOverride"
          parseOverride = do
            maybeString :: Maybe String <- v .:? "MempoolCapacityBytesOverride"
            case maybeString of
              Just "NoOverride" -> return (Just NoMempoolCapacityBytesOverride)
              Just invalid ->  fmap Just . Aeson.parseFail $
                    "Invalid value for 'MempoolCapacityBytesOverride'.  \
                    \Expecting byte count or NoOverride.  Value was: " <> show invalid
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
        npcByronApplicationName     <- v .:? "ApplicationName"
                                         .!= Byron.ApplicationName "cardano-sl"
        npcByronApplicationVersion  <- v .:? "ApplicationVersion" .!= 1
        protVerMajor                <- v .: "LastKnownBlockVersion-Major"
        protVerMinor                <- v .: "LastKnownBlockVersion-Minor"
        protVerAlt                  <- v .: "LastKnownBlockVersion-Alt" .!= 0

        pure NodeByronProtocolConfiguration {
               npcByronGenesisFile
             , npcByronGenesisFileHash
             , npcByronReqNetworkMagic
             , npcByronPbftSignatureThresh
             , npcByronApplicationName
             , npcByronApplicationVersion
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

      parseHardForkProtocol v = do
        npcTestEnableDevelopmentHardForkEras
          <- v .:? "TestEnableDevelopmentHardForkEras"
               .!= False

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

        pure NodeHardForkProtocolConfiguration {
               npcTestEnableDevelopmentHardForkEras,

               npcTestShelleyHardForkAtEpoch,
               npcTestShelleyHardForkAtVersion,

               npcTestAllegraHardForkAtEpoch,
               npcTestAllegraHardForkAtVersion,

               npcTestMaryHardForkAtEpoch,
               npcTestMaryHardForkAtVersion,

               npcTestAlonzoHardForkAtEpoch,
               npcTestAlonzoHardForkAtVersion,

               npcTestBabbageHardForkAtEpoch,
               npcTestBabbageHardForkAtVersion
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
    , pncSnapshotInterval = Last $ Just DefaultSnapshotInterval
    , pncTestEnableDevelopmentNetworkProtocols = Last $ Just False
    , pncTopologyFile = Last . Just $ TopologyFile "configuration/cardano/mainnet-topology.json"
    , pncProtocolFiles = mempty
    , pncValidateDB = mempty
    , pncShutdownConfig = mempty
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
    , pncTargetNumberOfRootPeers        = Last (Just 100)
    , pncTargetNumberOfKnownPeers       = Last (Just 100)
    , pncTargetNumberOfEstablishedPeers = Last (Just 50)
    , pncTargetNumberOfActivePeers      = Last (Just 20)
    , pncEnableP2P                      = Last (Just DisabledP2PMode)
    }

lastOption :: Parser a -> Parser (Last a)
lastOption = fmap Last . optional

makeNodeConfiguration :: PartialNodeConfiguration -> Either String NodeConfiguration
makeNodeConfiguration pnc = do
  configFile <- lastToEither "Missing YAML config file" $ pncConfigFile pnc
  topologyFile <- lastToEither "Missing TopologyFile" $ pncTopologyFile pnc
  databaseFile <- lastToEither "Missing DatabaseFile" $ pncDatabaseFile pnc
  validateDB <- lastToEither "Missing ValidateDB" $ pncValidateDB pnc
  protocolConfig <- lastToEither "Missing ProtocolConfig" $ pncProtocolConfig pnc
  loggingSwitch <- lastToEither "Missing LoggingSwitch" $ pncLoggingSwitch pnc
  logMetrics <- lastToEither "Missing LogMetrics" $ pncLogMetrics pnc
  traceConfig <- first Text.unpack $ partialTraceSelectionToEither $ pncTraceConfig pnc
  diffusionMode <- lastToEither "Missing DiffusionMode" $ pncDiffusionMode pnc
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

  -- TODO: This is not mandatory
  testEnableDevelopmentNetworkProtocols <-
    lastToEither "Missing TestEnableDevelopmentNetworkProtocols" $
      pncTestEnableDevelopmentNetworkProtocols pnc
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
             , ncProtocolConfig = protocolConfig
             , ncSocketConfig = socketConfig
             , ncDiffusionMode = diffusionMode
             , ncSnapshotInterval = snapshotInterval
             , ncTestEnableDevelopmentNetworkProtocols = testEnableDevelopmentNetworkProtocols
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
             , ncAcceptedConnectionsLimit
             , ncTargetNumberOfRootPeers
             , ncTargetNumberOfKnownPeers
             , ncTargetNumberOfEstablishedPeers
             , ncTargetNumberOfActivePeers
             , ncEnableP2P = case enableP2P of
                 EnabledP2PMode  -> SomeNetworkP2PMode Consensus.EnabledP2PMode
                 DisabledP2PMode -> SomeNetworkP2PMode Consensus.DisabledP2PMode
             }

ncProtocol :: NodeConfiguration -> Protocol
ncProtocol nc =
  case ncProtocolConfig nc of
    NodeProtocolConfigurationByron{}   -> ByronProtocol
    NodeProtocolConfigurationShelley{} -> ShelleyProtocol
    NodeProtocolConfigurationCardano{} -> CardanoProtocol

pncProtocol :: PartialNodeConfiguration -> Either Text Protocol
pncProtocol pnc =
  case pncProtocolConfig pnc of
    Last Nothing -> Left "Node protocol configuration not found"
    Last (Just NodeProtocolConfigurationByron{})   -> Right ByronProtocol
    Last (Just NodeProtocolConfigurationShelley{}) -> Right ShelleyProtocol
    Last (Just NodeProtocolConfigurationCardano{}) -> Right CardanoProtocol

parseNodeConfigurationFP :: Maybe ConfigYamlFilePath -> IO PartialNodeConfiguration
parseNodeConfigurationFP Nothing = parseNodeConfigurationFP . getLast $ pncConfigFile defaultPartialNodeConfiguration
parseNodeConfigurationFP (Just (ConfigYamlFilePath fp)) = do
    nc <- decodeFileThrow fp
    -- Make all the files be relative to the location of the config file.
    pure $ adjustFilePaths (takeDirectory fp </>) nc
