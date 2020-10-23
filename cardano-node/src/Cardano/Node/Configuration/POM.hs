{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wno-noncanonical-monoid-instances #-}

module Cardano.Node.Configuration.POM
  ( NodeConfiguration (..)
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
import           Prelude (String)

import           Control.Monad (fail)
import           Data.Aeson
import           Data.Yaml (decodeFileThrow)
import           Generic.Data (gmappend)
import           Generic.Data.Orphans ()
import           Options.Applicative
import           System.FilePath (takeDirectory, (</>))
import           System.Posix.Types (Fd (..))

import qualified Cardano.Chain.Update as Byron
import           Cardano.Crypto (RequiresNetworkMagic (..))
import           Cardano.Node.Protocol.Types (Protocol (..))
import           Cardano.Node.Types
import           Cardano.Tracing.Config
import           Ouroboros.Network.Block (MaxSlotNo (..))
import           Ouroboros.Network.NodeToNode (DiffusionMode (..))

data NodeConfiguration
  = NodeConfiguration
      {  ncNodeIPv4Addr    :: !(Maybe NodeHostIPv4Address)
      ,  ncNodeIPv6Addr    :: !(Maybe NodeHostIPv6Address)
      ,  ncNodePortNumber  :: !(Maybe PortNumber)
          -- | Filepath of the configuration yaml file. This file determines
          -- all the configuration settings required for the cardano node
          -- (logging, tracing, protocol, slot length etc)
       , ncConfigFile      :: !ConfigYamlFilePath
       , ncTopologyFile    :: !TopologyFile
       , ncDatabaseFile    :: !DbFile
       , ncProtocolFiles   :: !ProtocolFilepaths
       , ncValidateDB      :: !Bool
       , ncShutdownIPC     :: !(Maybe Fd)
       , ncShutdownOnSlotSynced :: !MaxSlotNo

        -- Protocol-specific parameters:
       , ncProtocolConfig :: !NodeProtocolConfiguration

         -- Node parameters, not protocol-specific:
       , ncSocketPath     :: !(Maybe SocketPath)
       , ncDiffusionMode  :: !DiffusionMode

         -- BlockFetch configuration
       , ncMaxConcurrencyBulkSync :: !(Maybe MaxConcurrencyBulkSync)
       , ncMaxConcurrencyDeadline :: !(Maybe MaxConcurrencyDeadline)

         -- Logging parameters:
       , ncLoggingSwitch  :: !Bool
       , ncLogMetrics     :: !Bool
       , ncTraceConfig    :: !TraceOptions
       } deriving (Eq, Show)


data PartialNodeConfiguration
  = PartialNodeConfiguration
      {  pncNodeIPv4Addr    :: !(Last NodeHostIPv4Address)
      ,  pncNodeIPv6Addr    :: !(Last NodeHostIPv6Address)
      ,  pncNodePortNumber  :: !(Last PortNumber)
         -- | Filepath of the configuration yaml file. This file determines
         -- all the configuration settings required for the cardano node
         -- (logging, tracing, protocol, slot length etc)
       , pncConfigFile      :: !(Last ConfigYamlFilePath)
       , pncTopologyFile    :: !(Last TopologyFile)
       , pncDatabaseFile    :: !(Last DbFile)
       , pncProtocolFiles   :: !(Last ProtocolFilepaths)
       , pncValidateDB      :: !(Last Bool)
       , pncShutdownIPC     :: !(Last (Maybe Fd))
       , pncShutdownOnSlotSynced :: !(Last MaxSlotNo)

          -- Protocol-specific parameters:
       , pncProtocolConfig :: !(Last NodeProtocolConfiguration)

         -- Node parameters, not protocol-specific:
       , pncSocketPath     :: !(Last SocketPath)
       , pncDiffusionMode  :: !(Last DiffusionMode)

         -- BlockFetch configuration
       , pncMaxConcurrencyBulkSync :: !(Last MaxConcurrencyBulkSync)
       , pncMaxConcurrencyDeadline :: !(Last MaxConcurrencyDeadline)

         -- Logging parameters:
       , pncLoggingSwitch  :: !(Last Bool)
       , pncLogMetrics     :: !(Last Bool)
       , pncTraceConfig    :: !(Last TraceOptions)
       } deriving (Eq, Generic, Show)

instance AdjustFilePaths PartialNodeConfiguration where
  adjustFilePaths f x =
    x { pncProtocolConfig = adjustFilePaths f (pncProtocolConfig x)
      , pncSocketPath     = adjustFilePaths f (pncSocketPath x)
      }

instance Semigroup PartialNodeConfiguration where
  (<>) = gmappend

instance FromJSON PartialNodeConfiguration where
  parseJSON =
    withObject "PartialNodeConfiguration" $ \v -> do

      -- Node parameters, not protocol-specific
      pncSocketPath' <- Last <$> v .:? "SocketPath"
      pncDiffusionMode'
        <- Last . fmap getDiffusionMode <$> v .:? "DiffusionMode"

      -- Blockfetch parameters
      pncMaxConcurrencyBulkSync' <- Last <$> v .:? "MaxConcurrencyBulkSync"
      pncMaxConcurrencyDeadline' <- Last <$> v .:? "MaxConcurrencyDeadline"

      -- Logging parameters
      pncLoggingSwitch' <- v .:? "TurnOnLogging" .!= True
      pncLogMetrics'    <- Last <$> v .:? "TurnOnLogMetrics"
      pncTraceConfig'   <- if pncLoggingSwitch'
                           then Last . Just <$> traceConfigParser v
                           else return . Last $ Just TracingOff

      -- Protocol parameters
      protocol <-  v .:? "Protocol" .!= ByronProtocol
      pncProtocolConfig' <-
        case protocol of
          ByronProtocol ->
            Last . Just . NodeProtocolConfigurationByron <$> parseByronProtocol v

          ShelleyProtocol ->
            Last . Just . NodeProtocolConfigurationShelley <$> parseShelleyProtocol v

          CardanoProtocol ->
            Last . Just  <$> (NodeProtocolConfigurationCardano <$> parseByronProtocol v
                                                               <*> parseShelleyProtocol v
                                                               <*> parseHardForkProtocol v)
      pure PartialNodeConfiguration {
             pncProtocolConfig = pncProtocolConfig'
           , pncSocketPath = pncSocketPath'
           , pncDiffusionMode = pncDiffusionMode'
           , pncMaxConcurrencyBulkSync = pncMaxConcurrencyBulkSync'
           , pncMaxConcurrencyDeadline = pncMaxConcurrencyDeadline'
           , pncLoggingSwitch = Last $ Just pncLoggingSwitch'
           , pncLogMetrics = pncLogMetrics'
           , pncTraceConfig = pncTraceConfig'
           , pncNodeIPv4Addr = mempty
           , pncNodeIPv6Addr = mempty
           , pncNodePortNumber = mempty
           , pncConfigFile = mempty
           , pncTopologyFile = mempty
           , pncDatabaseFile = mempty
           , pncProtocolFiles = mempty
           , pncValidateDB = mempty
           , pncShutdownIPC = mempty
           , pncShutdownOnSlotSynced = mempty
           }
    where
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

        --TODO: these are silly names, allow better aliases:
        protVerMajor    <- v .:  "LastKnownBlockVersion-Major"
        protVerMinor    <- v .:  "LastKnownBlockVersion-Minor"

        pure NodeShelleyProtocolConfiguration {
               npcShelleyGenesisFile
             , npcShelleyGenesisFileHash
             , npcShelleySupportedProtocolVersionMajor = protVerMajor
             , npcShelleySupportedProtocolVersionMinor = protVerMinor
             }

      parseHardForkProtocol v = do
        npcTestShelleyHardForkAtEpoch   <- v .:? "TestShelleyHardForkAtEpoch"
        npcTestShelleyHardForkAtVersion <- v .:? "TestShelleyHardForkAtVersion"
        npcShelleyHardForkNotBeforeEpoch <- v .:? "ShelleyHardForkNotBeforeEpoch"

        npcTestAllegraHardForkAtEpoch   <- v .:? "TestAllegraHardForkAtEpoch"
        npcTestAllegraHardForkAtVersion <- v .:? "TestAllegraHardForkAtVersion"
        npcAllegraHardForkNotBeforeEpoch <- v .:? "AllegraHardForkNotBeforeEpoch"

        npcTestMaryHardForkAtEpoch   <- v .:? "TestMaryHardForkAtEpoch"
        npcTestMaryHardForkAtVersion <- v .:? "TestMaryHardForkAtVersion"
        npcMaryHardForkNotBeforeEpoch <- v .:? "MaryHardForkNotBeforeEpoch"

        pure NodeHardForkProtocolConfiguration {
               npcTestShelleyHardForkAtEpoch,
               npcTestShelleyHardForkAtVersion,
               npcShelleyHardForkNotBeforeEpoch,

               npcTestAllegraHardForkAtEpoch,
               npcTestAllegraHardForkAtVersion,
               npcAllegraHardForkNotBeforeEpoch,

               npcTestMaryHardForkAtEpoch,
               npcTestMaryHardForkAtVersion,
               npcMaryHardForkNotBeforeEpoch
             }

-- | Default configuration is mainnet
defaultPartialNodeConfiguration :: PartialNodeConfiguration
defaultPartialNodeConfiguration =
  PartialNodeConfiguration
    { pncConfigFile = Last . Just $ ConfigYamlFilePath "configuration/cardano/mainnet-config.json"
    , pncDatabaseFile = Last . Just $ DbFile "mainnet/db/"
    , pncLoggingSwitch = Last $ Just True
    , pncSocketPath = mempty
    , pncDiffusionMode = Last $ Just InitiatorAndResponderDiffusionMode
    , pncTopologyFile = Last . Just $ TopologyFile "configuration/cardano/mainnet-topology.json"
    , pncNodeIPv4Addr = mempty
    , pncNodeIPv6Addr = mempty
    , pncNodePortNumber = mempty
    , pncProtocolFiles = mempty
    , pncValidateDB = mempty
    , pncShutdownIPC = mempty
    , pncShutdownOnSlotSynced = mempty
    , pncProtocolConfig = mempty
    , pncMaxConcurrencyBulkSync = mempty
    , pncMaxConcurrencyDeadline = mempty
    , pncLogMetrics = mempty
    , pncTraceConfig = mempty
    }

lastOption :: Parser a -> Parser (Last a)
lastOption = fmap Last . optional

lastToEither :: String -> Last a -> Either String a
lastToEither errMsg (Last x) = maybe (Left errMsg) Right x

makeNodeConfiguration :: PartialNodeConfiguration -> Either String NodeConfiguration
makeNodeConfiguration pnc = do
  configFile <- lastToEither "Missing YAML config file" $ pncConfigFile pnc
  topologyFile <- lastToEither "Missing TopologyFile" $ pncTopologyFile pnc
  databaseFile <- lastToEither "Missing DatabaseFile" $ pncDatabaseFile pnc
  protocolFiles <- lastToEither "Missing ProtocolFiles" $ pncProtocolFiles pnc
  validateDB <- lastToEither "Missing ValidateDB" $ pncValidateDB pnc
  shutdownIPC <- lastToEither "Missing ShutdownIPC" $ pncShutdownIPC pnc
  shutdownOnSlotSynced <- lastToEither "Missing ShutdownOnSlotSynced" $ pncShutdownOnSlotSynced pnc
  protocolConfig <- lastToEither "Missing ProtocolConfig" $ pncProtocolConfig pnc
  loggingSwitch <- lastToEither "Missing LoggingSwitch" $ pncLoggingSwitch pnc
  logMetrics <- lastToEither "Missing LogMetrics" $ pncLogMetrics pnc
  traceConfig <- lastToEither "Missing TraceConfig" $ pncTraceConfig pnc
  diffusionMode <- lastToEither "Missing DiffusionMode" $ pncDiffusionMode pnc
  return $ NodeConfiguration
             { ncNodeIPv4Addr = getLast $ pncNodeIPv4Addr pnc
             , ncNodeIPv6Addr = getLast $ pncNodeIPv6Addr pnc
             , ncNodePortNumber = getLast $ pncNodePortNumber pnc
             , ncConfigFile = configFile
             , ncTopologyFile = topologyFile
             , ncDatabaseFile = databaseFile
             , ncProtocolFiles = protocolFiles
             , ncValidateDB = validateDB
             , ncShutdownIPC = shutdownIPC
             , ncShutdownOnSlotSynced = shutdownOnSlotSynced
             , ncProtocolConfig = protocolConfig
             , ncSocketPath = getLast $ pncSocketPath pnc
             , ncDiffusionMode = diffusionMode
             , ncMaxConcurrencyBulkSync = getLast $ pncMaxConcurrencyBulkSync pnc
             , ncMaxConcurrencyDeadline = getLast $ pncMaxConcurrencyDeadline pnc
             , ncLoggingSwitch = loggingSwitch
             , ncLogMetrics = logMetrics
             , ncTraceConfig = traceConfig
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
