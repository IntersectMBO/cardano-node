{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Node.Types
  ( ConfigYamlFilePath(..)
  , NodeCLI(..)
  , NodeConfiguration(..)
  , Protocol(..)
  , MockProtocol(..)
  , NodeByronProtocolConfiguration(..)
  , NodeHardForkProtocolConfiguration(..)
  , NodeProtocolConfiguration(..)
  , NodeShelleyProtocolConfiguration(..)
  , NodeMockProtocolConfiguration(..)
  , TraceOptions(..)
  , ncProtocol
  , parseNodeConfiguration
  , parseNodeConfigurationFP
  , protocolName
  ) where

import           Cardano.Prelude
import           Prelude (String)

import           Control.Monad.Fail (fail)
import           Data.Aeson
import           Data.Yaml (decodeFileThrow)
import           System.FilePath ((</>), takeDirectory)
import           System.Posix.Types (Fd)

import           Cardano.Api.Typed (EpochNo)
import           Cardano.Config.Types
import           Cardano.Crypto (RequiresNetworkMagic(..))
import qualified Cardano.Chain.Update as Byron
import           Cardano.Node.TraceConfig (TraceOptions(..), traceConfigParser)
import           Ouroboros.Network.Block (MaxSlotNo(..))
import           Ouroboros.Consensus.NodeId (CoreNodeId(..))

--TODO: things will probably be clearer if we don't use these newtype wrappers and instead
-- use records with named fields in the CLI code.

-- | Filepath of the configuration yaml file. This file determines
-- all the configuration settings required for the cardano node
-- (logging, tracing, protocol, slot length etc)
newtype ConfigYamlFilePath = ConfigYamlFilePath
  { unConfigPath :: FilePath }
  deriving newtype (Eq, Show)

data NodeCLI = NodeCLI
  { nodeMode        :: !NodeProtocolMode
  , nodeAddr        :: !(Maybe NodeAddress)
    -- | Filepath of the configuration yaml file. This file determines
    -- all the configuration settings required for the cardano node
    -- (logging, tracing, protocol, slot length etc)
  , configFile      :: !ConfigYamlFilePath
  , topologyFile    :: !TopologyFile
  , databaseFile    :: !DbFile
  , socketFile      :: !(Maybe SocketPath)
  , protocolFiles   :: !ProtocolFilepaths
  , validateDB      :: !Bool
  , shutdownIPC     :: !(Maybe Fd)
  , shutdownOnSlotSynced :: !MaxSlotNo
  }

data NodeConfiguration
  = NodeConfiguration
      { -- Protocol-specific parameters:
         ncProtocolConfig :: NodeProtocolConfiguration

         -- Node parameters, not protocol-specific:
       , ncSocketPath     :: Maybe SocketPath

         -- BlockFetch configuration
       , ncMaxConcurrencyBulkSync :: Maybe MaxConcurrencyBulkSync
       , ncMaxConcurrencyDeadline :: Maybe MaxConcurrencyDeadline

         -- Logging parameters:
       , ncViewMode       :: ViewMode
       , ncLoggingSwitch  :: Bool
       , ncLogMetrics     :: Bool
       , ncTraceConfig    :: TraceOptions
       } deriving Show

class AdjustFilePaths a where
  adjustFilePaths :: (FilePath -> FilePath) -> a -> a

instance AdjustFilePaths NodeConfiguration where
  adjustFilePaths f x@NodeConfiguration {
                        ncProtocolConfig,
                        ncSocketPath
                      } =
    x {
      ncProtocolConfig = adjustFilePaths f ncProtocolConfig,
      ncSocketPath     = adjustFilePaths f ncSocketPath
    }

instance FromJSON NodeConfiguration where
  parseJSON =
    withObject "NodeConfiguration" $ \v -> do

      -- Node parameters, not protocol-specific
      ncSocketPath <- v .:? "SocketPath"

      -- Blockfetch parameters
      ncMaxConcurrencyBulkSync <- v .:? "MaxConcurrencyBulkSync"
      ncMaxConcurrencyDeadline <- v .:? "MaxConcurrencyDeadline"

      -- Logging parameters
      ncViewMode      <- v .:? "ViewMode"         .!= SimpleView
      ncLoggingSwitch <- v .:? "TurnOnLogging"    .!= True
      ncLogMetrics    <- v .:? "TurnOnLogMetrics" .!= True
      ncTraceConfig   <- if ncLoggingSwitch
                           then traceConfigParser v
                           else return TracingOff

      -- Protocol parameters
      protocol <- v .: "Protocol" .!= ByronProtocol
      ncProtocolConfig <-
        case protocol of
          MockProtocol ptcl ->
            NodeProtocolConfigurationMock <$> parseMockProtocol ptcl v

          ByronProtocol ->
            NodeProtocolConfigurationByron <$> parseByronProtocol v

          ShelleyProtocol ->
            NodeProtocolConfigurationShelley <$> parseShelleyProtocol v

          CardanoProtocol ->
            NodeProtocolConfigurationCardano <$> parseByronProtocol v
                                             <*> parseShelleyProtocol v
                                             <*> parseHardForkProtocol v
      pure NodeConfiguration {
             ncProtocolConfig
           , ncSocketPath
           , ncMaxConcurrencyBulkSync
           , ncMaxConcurrencyDeadline
           , ncViewMode
           , ncLoggingSwitch
           , ncLogMetrics
           , ncTraceConfig
           }
    where
      parseMockProtocol npcMockProtocol v = do
        npcMockNodeId       <- v .: "NodeId"
        npcMockNumCoreNodes <- v .: "NumCoreNodes"
        pure NodeMockProtocolConfiguration {
               npcMockProtocol
             , npcMockNodeId
             , npcMockNumCoreNodes
             }

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

        --TODO: these are silly names, allow better aliases:
        protVerMajor    <- v .:  "LastKnownBlockVersion-Major"
        protVerMinor    <- v .:  "LastKnownBlockVersion-Minor"
        protVerMajroMax <- v .:? "MaxKnownMajorProtocolVersion" .!= 1

        pure NodeShelleyProtocolConfiguration {
               npcShelleyGenesisFile
             , npcShelleySupportedProtocolVersionMajor = protVerMajor
             , npcShelleySupportedProtocolVersionMinor = protVerMinor
             , npcShelleyMaxSupportedProtocolVersion   = protVerMajroMax
             }

      parseHardForkProtocol v = do
        npcTestShelleyHardForkAtEpoch   <- v .:? "TestShelleyHardForkAtEpoch"
        npcTestShelleyHardForkAtVersion <- v .:? "TestShelleyHardForkAtVersion"
        npcShelleyHardForkNotBeforeEpoch <- v .:? "ShelleyHardForkNotBeforeEpoch"
        pure NodeHardForkProtocolConfiguration {
               npcTestShelleyHardForkAtEpoch,
               npcTestShelleyHardForkAtVersion,
               npcShelleyHardForkNotBeforeEpoch
             }

data Protocol = MockProtocol !MockProtocol
              | ByronProtocol
              | ShelleyProtocol
              | CardanoProtocol
  deriving (Eq, Show, Generic)

instance FromJSON Protocol where
  parseJSON =
    withText "Protocol" $ \str -> case str of

      -- The new names
      "MockBFT"   -> pure (MockProtocol MockBFT)
      "MockPBFT"  -> pure (MockProtocol MockPBFT)
      "MockPraos" -> pure (MockProtocol MockPraos)
      "Byron"     -> pure ByronProtocol
      "Shelley"   -> pure ShelleyProtocol
      "Cardano"   -> pure CardanoProtocol

      -- The old names
      "BFT"       -> pure (MockProtocol MockBFT)
    --"MockPBFT"  -- same as new name
      "Praos"     -> pure (MockProtocol MockPraos)
      "RealPBFT"  -> pure ByronProtocol
      "TPraos"    -> pure ShelleyProtocol

      _           -> fail $ "Parsing of Protocol failed. "
                         <> show str <> " is not a valid protocol"

deriving instance NFData Protocol
deriving instance NoUnexpectedThunks Protocol

data MockProtocol = MockBFT
                  | MockPBFT
                  | MockPraos
  deriving (Eq, Show, Generic)

deriving instance NFData MockProtocol
deriving instance NoUnexpectedThunks MockProtocol

data NodeProtocolConfiguration =
       NodeProtocolConfigurationMock    NodeMockProtocolConfiguration
     | NodeProtocolConfigurationByron   NodeByronProtocolConfiguration
     | NodeProtocolConfigurationShelley NodeShelleyProtocolConfiguration
     | NodeProtocolConfigurationCardano NodeByronProtocolConfiguration
                                        NodeShelleyProtocolConfiguration
                                        NodeHardForkProtocolConfiguration
  deriving Show

data NodeShelleyProtocolConfiguration =
     NodeShelleyProtocolConfiguration {
       npcShelleyGenesisFile  :: !GenesisFile

       -- | These declare the version of the protocol that the node is prepared
       -- to run. This is usually the version of the protocol in use on the
       -- chain now, but during protocol updates this version will be the one
       -- that we declare that we are ready to move to. This is the endorsement
       -- mechanism for determining when enough block producers are ready to
       -- move to the next version.
       --
     , npcShelleySupportedProtocolVersionMajor :: !Natural
     , npcShelleySupportedProtocolVersionMinor :: !Natural

       -- | The maximum major version of the protocol this node supports.
       -- If the actual version ever goes higher than this then the node
       -- will stop with an appropriate error message.
     , npcShelleyMaxSupportedProtocolVersion :: !Natural
     }
  deriving Show

data NodeByronProtocolConfiguration =
     NodeByronProtocolConfiguration {
       npcByronGenesisFile         :: !GenesisFile
     , npcByronReqNetworkMagic     :: !RequiresNetworkMagic
     , npcByronPbftSignatureThresh :: !(Maybe Double)

       -- | Update application name.
     , npcByronApplicationName     :: !Byron.ApplicationName

       -- | Application (ie software) version.
     , npcByronApplicationVersion  :: !Byron.NumSoftwareVersion

       -- | These declare the version of the protocol that the node is prepared
       -- to run. This is usually the version of the protocol in use on the
       -- chain now, but during protocol updates this version will be the one
       -- that we declare that we are ready to move to. This is the endorsement
       -- mechanism for determining when enough block producers are ready to
       -- move to the next version.
       --
     , npcByronSupportedProtocolVersionMajor :: !Word16
     , npcByronSupportedProtocolVersionMinor :: !Word16
     , npcByronSupportedProtocolVersionAlt   :: !Word8
     }
  deriving Show

data NodeMockProtocolConfiguration =
     NodeMockProtocolConfiguration {
       npcMockProtocol     :: MockProtocol
     , npcMockNodeId       :: CoreNodeId
     , npcMockNumCoreNodes :: Word64
     }
  deriving Show

-- | Configuration relating to a hard forks themselves, not the specific eras.
--
data NodeHardForkProtocolConfiguration =
     NodeHardForkProtocolConfiguration {

       -- | If we have knowledge about when the Shelley hard fork is then we
       -- have an opportunity to optimise the bulk sync slightly.
       --
       npcShelleyHardForkNotBeforeEpoch :: Maybe EpochNo

       -- | For testing purposes we support specifying that the hard fork
       -- happens at an exact epoch number (ie the first epoch of the new era).
       --
       -- Obviously if this is used, all the nodes in the test cluster must be
       -- configured the same, or they will disagree.
       --
     , npcTestShelleyHardForkAtEpoch :: Maybe EpochNo

       -- | For testing purposes we support specifying that the hard fork
       -- happens at a given major protocol version. For example this can be
       -- used to cause the Shelley hard fork to occur at the transition from
       -- protocol version 0 to version 1 (rather than the default of from 1 to
       -- 2) which can make the test setup simpler.
       --
       -- Obviously if this is used, all the nodes in the test cluster must be
       -- configured the same, or they will disagree.
       --
     , npcTestShelleyHardForkAtVersion :: Maybe Word
     }
  deriving Show

instance AdjustFilePaths NodeProtocolConfiguration where

  adjustFilePaths f (NodeProtocolConfigurationMock pc) =
    NodeProtocolConfigurationMock (adjustFilePaths f pc)

  adjustFilePaths f (NodeProtocolConfigurationByron pc) =
    NodeProtocolConfigurationByron (adjustFilePaths f pc)

  adjustFilePaths f (NodeProtocolConfigurationShelley pc) =
    NodeProtocolConfigurationShelley (adjustFilePaths f pc)

  adjustFilePaths f (NodeProtocolConfigurationCardano pcb pcs pch) =
    NodeProtocolConfigurationCardano (adjustFilePaths f pcb)
                                     (adjustFilePaths f pcs)
                                     pch


instance AdjustFilePaths NodeMockProtocolConfiguration where
  adjustFilePaths _f x = x -- Contains no file paths

instance AdjustFilePaths NodeByronProtocolConfiguration where
  adjustFilePaths f x@NodeByronProtocolConfiguration {
                        npcByronGenesisFile
                      } =
    x { npcByronGenesisFile = adjustFilePaths f npcByronGenesisFile }

instance AdjustFilePaths NodeShelleyProtocolConfiguration where
  adjustFilePaths f x@NodeShelleyProtocolConfiguration {
                        npcShelleyGenesisFile
                      } =
    x { npcShelleyGenesisFile = adjustFilePaths f npcShelleyGenesisFile }

instance AdjustFilePaths SocketPath where
  adjustFilePaths f (SocketPath p) = SocketPath (f p)

instance AdjustFilePaths GenesisFile where
  adjustFilePaths f (GenesisFile p) = GenesisFile (f p)

instance AdjustFilePaths a => AdjustFilePaths (Maybe a) where
  adjustFilePaths f = fmap (adjustFilePaths f)

ncProtocol :: NodeConfiguration -> Protocol
ncProtocol nc =
    case ncProtocolConfig nc of
      NodeProtocolConfigurationMock npc  -> MockProtocol (npcMockProtocol npc)
      NodeProtocolConfigurationByron{}   -> ByronProtocol
      NodeProtocolConfigurationShelley{} -> ShelleyProtocol
      NodeProtocolConfigurationCardano{} -> CardanoProtocol

parseNodeConfiguration :: NodeCLI -> IO NodeConfiguration
parseNodeConfiguration NodeCLI{configFile} = parseNodeConfigurationFP configFile

parseNodeConfigurationFP :: ConfigYamlFilePath -> IO NodeConfiguration
parseNodeConfigurationFP (ConfigYamlFilePath fp) = do
    nc <- decodeFileThrow fp
    -- Make all the files be relative to the location of the config file.
    pure $ adjustFilePaths (takeDirectory fp </>) nc

-- | A human readable name for the protocol
--
protocolName :: Protocol -> String
protocolName (MockProtocol MockBFT)   = "Mock BFT"
protocolName (MockProtocol MockPBFT)  = "Mock PBFT"
protocolName (MockProtocol MockPraos) = "Mock Praos"
protocolName  ByronProtocol           = "Byron"
protocolName  ShelleyProtocol         = "Shelley"
protocolName  CardanoProtocol         = "Byron; Shelley"
