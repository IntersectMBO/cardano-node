{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Config.Types
    ( CardanoEnvironment (..)
    , CBORObject (..)
    , PoolMetaDataFile (..)
    , CertificateFile (..)
    , ConfigYamlFilePath (..)
    , ConfigError (..)
    , DbFile (..)
    , GenesisFile (..)
    , KESMetricsData (..)
    , MaxKESEvolutions (..)
    , MetaDataFile (..)
    , OperationalCertStartKESPeriod (..)
    , HasKESMetricsData (..)
    , NodeAddress (..)
    , NodeConfiguration (..)
    , NodeProtocolConfiguration (..)
    , NodeMockProtocolConfiguration (..)
    , NodeByronProtocolConfiguration (..)
    , NodeShelleyProtocolConfiguration (..)
    , NodeHostAddress (..)
    , Protocol (..)
    , MockProtocol (..)
    , ncProtocol
    , protocolName
    , NodeCLI (..)
    , NodeProtocolMode (..)
    , SigningKeyFile (..)
    , SomeConsensusProtocolConstraints
    , ProtocolFilepaths (..)
    , SomeConsensusProtocol (..)
    , TopologyFile (..)
    , TraceConstraints
    , SocketPath (..)
    , UpdateProposalFile (..)
    , ViewMode (..)
    , Fd (..)
    , parseNodeConfiguration
    , parseNodeConfigurationFP
    , SomeNodeClientProtocol(..)
    , parseNodeHostAddress
    ) where

import           Prelude (show)
import           Cardano.Prelude hiding (show)

import           Data.Aeson
import           Data.IP (IP)
import           Data.String (String)
import qualified Data.Text as Text
import           Data.Yaml (decodeFileThrow)
import           Control.Monad.Fail
import           Network.Socket (PortNumber)
import           System.FilePath ((</>), takeDirectory)
import           System.Posix.Types (Fd(Fd))

import           Cardano.BM.Tracing (ToObject, Transformable)
import qualified Cardano.Chain.Update as Update
import           Cardano.Chain.Slotting (EpochSlots)
import           Cardano.Crypto.KES.Class (Period)
import           Cardano.Crypto.ProtocolMagic (RequiresNetworkMagic)
import           Ouroboros.Consensus.Block (Header, BlockProtocol, ForgeState(..))
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import qualified Ouroboros.Consensus.Cardano as Consensus (Protocol, ProtocolClient)
import           Ouroboros.Consensus.Config (TopLevelConfig (..))
import           Ouroboros.Consensus.HeaderValidation (OtherHeaderEnvelopeError)
import           Ouroboros.Consensus.Ledger.Abstract (LedgerError)
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTxId, HasTxId, HasTxs(..),
                   LedgerSupportsMempool(..))
import           Ouroboros.Consensus.Mock.Ledger.Block (SimpleBlock)
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import           Ouroboros.Consensus.Node.Run (RunNode)
import           Ouroboros.Consensus.NodeId (CoreNodeId(..))
import           Ouroboros.Consensus.Protocol.Abstract (CannotLead, ValidationErr)
import           Ouroboros.Consensus.Util.Condense (Condense (..))
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Ledger.Mempool (GenTx, TxId)
import           Ouroboros.Consensus.Shelley.Protocol (ConsensusConfig (..),
                   TPraosIsCoreNode (..), TPraosParams (..))
import           Ouroboros.Consensus.Shelley.Protocol.Crypto.HotKey (HotKey (..))

import           Ouroboros.Network.Block (HeaderHash, MaxSlotNo(..))

import           Cardano.Config.Orphanage ()
import           Cardano.Config.TraceConfig
import           Cardano.Crypto (RequiresNetworkMagic(..))

import           Shelley.Spec.Ledger.OCert (KESPeriod (..), OCert (..))


-- | Errors for the cardano-config module.
data ConfigError
    = ConfigErrorFileNotFound !FilePath

-- | Instance for showing the @ConfigError@.
instance Show ConfigError where
    show (ConfigErrorFileNotFound fp)
        = "File '" <> fp <> "' not found!"

-- | Specify what the CBOR file is
-- i.e a block, a tx, etc
data CBORObject = CBORBlockByron EpochSlots
                | CBORDelegationCertificateByron
                | CBORTxByron
                | CBORUpdateProposalByron
                | CBORVoteByron
                deriving Show

--------------------------------------------------------------------------------
-- Cardano Environment
--------------------------------------------------------------------------------

-- | Just a placeholder for now.
data CardanoEnvironment = NoEnvironment
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Cardano Configuration Data Structures
--------------------------------------------------------------------------------

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

-- | Mock protocols requires different parameters to real protocols.
-- Therefore we distinguish this at the top level on the command line.
data NodeProtocolMode = MockProtocolMode
                      | RealProtocolMode

data ProtocolFilepaths =
     ProtocolFilepaths {
       byronCertFile   :: !(Maybe FilePath)
     , byronKeyFile    :: !(Maybe FilePath)
     , shelleyKESFile  :: !(Maybe FilePath)
     , shelleyVRFFile  :: !(Maybe FilePath)
     , shelleyCertFile :: !(Maybe FilePath)
     }

--TODO: things will probably be clearer if we don't use these newtype wrappers and instead
-- use records with named fields in the CLI code.

-- | Filepath of the configuration yaml file. This file determines
-- all the configuration settings required for the cardano node
-- (logging, tracing, protocol, slot length etc)
newtype ConfigYamlFilePath = ConfigYamlFilePath
  { unConfigPath :: FilePath }
  deriving newtype (Eq, Show)

newtype TopologyFile = TopologyFile
  { unTopology :: FilePath }
  deriving newtype Show

newtype DbFile = DbFile
  { unDB :: FilePath }
  deriving newtype Show

newtype GenesisFile = GenesisFile
  { unGenesisFile :: FilePath }
  deriving stock (Eq, Ord)
  deriving newtype (IsString, Show)

instance FromJSON GenesisFile where
  parseJSON (String genFp) = pure . GenesisFile $ Text.unpack genFp
  parseJSON invalid = panic $ "Parsing of GenesisFile failed due to type mismatch. "
                           <> "Encountered: " <> (Text.pack $ show invalid)

-- Encompasses stake certificates, stake pool certificates,
-- genesis delegate certificates and MIR certificates.
newtype CertificateFile = CertificateFile
  { unCertificateFile :: FilePath }
  deriving newtype (Eq, Show)

newtype PoolMetaDataFile = PoolMetaDataFile
  { unPoolMetaDataFile :: FilePath }
  deriving newtype (Eq, Show)

newtype MetaDataFile = MetaDataFile
  { unMetaDataFile :: FilePath }
  deriving newtype (Eq, Show)

newtype UpdateProposalFile = UpdateProposalFile
  { unUpdateProposalFile :: FilePath }
  deriving newtype (Eq, Show)

newtype SocketPath = SocketPath
  { unSocketPath :: FilePath }
  deriving stock (Eq, Ord)
  deriving newtype (FromJSON, IsString, Show)

newtype SigningKeyFile = SigningKeyFile
  { unSigningKeyFile ::  FilePath }
  deriving stock (Eq, Ord)
  deriving newtype (IsString, Show)

data NodeConfiguration =
     NodeConfiguration {
       -- Protocol-specific parameters:
       ncProtocolConfig :: NodeProtocolConfiguration

       -- Node parameters, not protocol-specific:
     , ncSocketPath     :: Maybe SocketPath

       -- Logging parameters:
     , ncViewMode       :: ViewMode
     , ncLoggingSwitch  :: Bool
     , ncLogMetrics     :: Bool
     , ncTraceConfig    :: TraceOptions
     }
  deriving Show

data NodeProtocolConfiguration =
       NodeProtocolConfigurationMock    NodeMockProtocolConfiguration
     | NodeProtocolConfigurationByron   NodeByronProtocolConfiguration
     | NodeProtocolConfigurationShelley NodeShelleyProtocolConfiguration
     | NodeProtocolConfigurationCardano NodeByronProtocolConfiguration
                                        NodeShelleyProtocolConfiguration
  deriving Show

data NodeMockProtocolConfiguration =
     NodeMockProtocolConfiguration {
       npcMockProtocol     :: MockProtocol
     , npcMockNodeId       :: CoreNodeId
     , npcMockNumCoreNodes :: Word64
     }
  deriving Show

data NodeByronProtocolConfiguration =
     NodeByronProtocolConfiguration {
       npcByronGenesisFile         :: !GenesisFile
     , npcByronReqNetworkMagic     :: !RequiresNetworkMagic
     , npcByronPbftSignatureThresh :: !(Maybe Double)

       -- | Update application name.
     , npcByronApplicationName     :: !Update.ApplicationName

       -- | Application (ie software) version.
     , npcByronApplicationVersion  :: !Update.NumSoftwareVersion

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

instance FromJSON NodeConfiguration where
  parseJSON =
    withObject "NodeConfiguration" $ \v -> do

      -- Node parameters, not protocol-specific
      ncSocketPath <- v .:? "SocketPath"

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
      pure NodeConfiguration {
             ncProtocolConfig
           , ncSocketPath
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
                                         .!= Update.ApplicationName "cardano-sl"
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


parseNodeConfigurationFP :: ConfigYamlFilePath -> IO NodeConfiguration
parseNodeConfigurationFP (ConfigYamlFilePath fp) = do
    nc <- decodeFileThrow fp
    -- Make all the files be relative to the location of the config file.
    pure $ adjustFilePaths (takeDirectory fp </>) nc

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

instance AdjustFilePaths NodeProtocolConfiguration where

  adjustFilePaths f (NodeProtocolConfigurationMock pc) =
    NodeProtocolConfigurationMock (adjustFilePaths f pc)

  adjustFilePaths f (NodeProtocolConfigurationByron pc) =
    NodeProtocolConfigurationByron (adjustFilePaths f pc)

  adjustFilePaths f (NodeProtocolConfigurationShelley pc) =
    NodeProtocolConfigurationShelley (adjustFilePaths f pc)

  adjustFilePaths f (NodeProtocolConfigurationCardano pcb pcs) =
    NodeProtocolConfigurationCardano (adjustFilePaths f pcb)
                                     (adjustFilePaths f pcs)

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


parseNodeConfiguration :: NodeCLI -> IO NodeConfiguration
parseNodeConfiguration NodeCLI{configFile} = parseNodeConfigurationFP configFile

data Protocol = MockProtocol !MockProtocol
              | ByronProtocol
              | ShelleyProtocol
              | CardanoProtocol
  deriving (Eq, Show, Generic)

deriving instance NFData Protocol
deriving instance NoUnexpectedThunks Protocol

data MockProtocol = MockBFT
                  | MockPBFT
                  | MockPraos
  deriving (Eq, Show, Generic)

deriving instance NFData MockProtocol
deriving instance NoUnexpectedThunks MockProtocol

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

-- | A human readable name for the protocol
--
protocolName :: Protocol -> String
protocolName (MockProtocol MockBFT)   = "Mock BFT"
protocolName (MockProtocol MockPBFT)  = "Mock PBFT"
protocolName (MockProtocol MockPraos) = "Mock Praos"
protocolName  ByronProtocol           = "Byron"
protocolName  ShelleyProtocol         = "Shelley"
protocolName  CardanoProtocol         = "Byron; Shelley"

ncProtocol :: NodeConfiguration -> Protocol
ncProtocol nc =
    case ncProtocolConfig nc of
      NodeProtocolConfigurationMock npc  -> MockProtocol (npcMockProtocol npc)
      NodeProtocolConfigurationByron{}   -> ByronProtocol
      NodeProtocolConfigurationShelley{} -> ShelleyProtocol
      NodeProtocolConfigurationCardano{} -> CardanoProtocol


-- Node can be run in two modes.
data ViewMode = LiveView    -- Live mode with TUI
              | SimpleView  -- Simple mode, just output text.
              deriving (Eq, Show)

instance FromJSON ViewMode where
  parseJSON (String str) = case str of
                            "LiveView" -> pure LiveView
                            "SimpleView" -> pure SimpleView
                            view -> panic $ "Parsing of ViewMode: "
                                          <> view <> " failed. "
                                          <> view <> " is not a valid view mode"
  parseJSON invalid = panic $ "Parsing of ViewMode failed due to type mismatch. "
                            <> "Encountered: " <> (Text.pack $ show invalid)

--------------------------------------------------------------------------------
-- Cardano Topology Related Data Structures
--------------------------------------------------------------------------------

-- | IPv4 address with a port number.
data NodeAddress = NodeAddress
  { naHostAddress :: !NodeHostAddress
  , naPort :: !PortNumber
  } deriving (Eq, Ord, Show)

instance Condense NodeAddress where
  condense (NodeAddress addr port) = show addr ++ ":" ++ show port

instance FromJSON NodeAddress where
  parseJSON = withObject "NodeAddress" $ \v -> do
    NodeAddress
      <$> v .: "addr"
      <*> ((fromIntegral :: Int -> PortNumber) <$> v .: "port")

instance ToJSON NodeAddress where
  toJSON na =
    object
      [ "addr" .= toJSON (naHostAddress na)
      , "port" .= (fromIntegral (naPort na) :: Int)
      ]

-- Embedding a Maybe inside a newtype is somewhat icky but this seems to work
-- and removing the Maybe breaks the functionality in a subtle way that is difficult
-- to diagnose.
newtype NodeHostAddress
  = NodeHostAddress { unNodeHostAddress :: Maybe IP }
  deriving newtype Show
  deriving (Eq, Ord)

instance FromJSON NodeHostAddress where
  parseJSON (String ipStr) =
    case readMaybe $ Text.unpack ipStr of
      Just ip -> pure $ NodeHostAddress (Just ip)
      Nothing -> panic $ "Parsing of IP failed: " <> ipStr
  parseJSON Null = pure $ NodeHostAddress Nothing
  parseJSON invalid = panic $ "Parsing of IP failed due to type mismatch. "
                            <> "Encountered: " <> (Text.pack $ show invalid) <> "\n"

parseNodeHostAddress :: String -> Either String NodeHostAddress
parseNodeHostAddress str =
   maybe (Left $ "Failed to parse: " ++ str) (Right . NodeHostAddress . Just) $ readMaybe str

instance ToJSON NodeHostAddress where
  toJSON mha =
    case unNodeHostAddress mha of
      Just ip -> String (Text.pack $ show ip)
      Nothing -> Null

--------------------------------------------------------------------------------
-- Protocol & Tracing Related
--------------------------------------------------------------------------------

type SomeConsensusProtocolConstraints blk =
     ( HasKESMetricsData blk
     , RunNode blk
     , TraceConstraints blk
     , Transformable Text IO (ForgeState blk)
     )

data SomeConsensusProtocol where

     SomeConsensusProtocol :: SomeConsensusProtocolConstraints blk
                           => Consensus.Protocol IO blk (BlockProtocol blk)
                           -> SomeConsensusProtocol


-- | KES-related data to be traced as metrics.
data KESMetricsData
  = NoKESMetricsData
  -- ^ The current protocol does not support KES.
  | TPraosKESMetricsData
      !Period
      -- ^ The current KES period of the hot key, relative to the start KES
      -- period of the operational certificate.
      !MaxKESEvolutions
      -- ^ The configured max KES evolutions.
      !OperationalCertStartKESPeriod
      -- ^ The start KES period of the configured operational certificate.

-- | The maximum number of evolutions that a KES key can undergo before it is
-- considered expired.
newtype MaxKESEvolutions = MaxKESEvolutions Word64

-- | The start KES period of the configured operational certificate.
data OperationalCertStartKESPeriod
  = NoOperationalCertConfigured
  | OperationalCertStartKESPeriod !Period

class HasKESMetricsData blk where
  getKESMetricsData :: ProtocolInfo m blk -> ForgeState blk -> KESMetricsData

  -- Default to 'NoKESMetricsData'
  getKESMetricsData _ _ = NoKESMetricsData

instance HasKESMetricsData (ShelleyBlock c) where
  getKESMetricsData protoInfo forgeState =
      TPraosKESMetricsData currKesPeriod maxKesEvos oCertStartKesPeriod
    where
      HotKey { hkEvolution = currKesPeriod } = chainIndepState forgeState

      oCertStartKesPeriod =
        case pInfoLeaderCreds of
          Nothing -> NoOperationalCertConfigured
          Just (TPraosIsCoreNode{tpraosIsCoreNodeOpCert}, _) ->
            let KESPeriod kp = ocertKESPeriod tpraosIsCoreNodeOpCert
            in OperationalCertStartKESPeriod kp

      maxKesEvos =
        MaxKESEvolutions
          . tpraosMaxKESEvo
          . tpraosParams
          . configConsensus
          $ pInfoConfig

      ProtocolInfo
        { pInfoConfig
        , pInfoLeaderCreds
        } = protoInfo

instance HasKESMetricsData ByronBlock where

instance HasKESMetricsData (SimpleBlock a b) where

-- | Tracing-related constraints for monitoring purposes.
--
-- When you need a 'Show' or 'Condense' instance for more types, just add the
-- appropriate constraint here. There's no need to modify the consensus
-- code-base, unless the corresponding instance is missing. Note we are aiming to
-- remove all `Condense` constaints by defining the relevant 'ToObject' instance
-- in 'cardano-node'
type TraceConstraints blk =
    ( Condense blk
    , Condense [blk]
    , Condense (Header blk)
    , Condense (HeaderHash blk)
    , Condense (GenTx blk)
    , Condense (TxId (GenTx blk))
    , HasTxs blk
    , HasTxId (GenTx blk)
    , Show (ApplyTxErr blk)
    , Show (GenTx blk)
    , Show (GenTxId blk)
    , Show blk
    , Show (TxId (GenTx blk))
    , ToJSON   (TxId (GenTx blk))
    , ToObject (ApplyTxErr blk)
    , ToObject (GenTx blk)
    , ToObject (Header blk)
    , ToObject (LedgerError blk)
    , ToObject (OtherHeaderEnvelopeError blk)
    , ToObject (ValidationErr (BlockProtocol blk))
    , ToObject (CannotLead (BlockProtocol blk))
    )

--------------------------------------------------------------------------------
-- Node client requirements
--------------------------------------------------------------------------------

data SomeNodeClientProtocol where

     SomeNodeClientProtocol
       :: RunNode blk
       => Consensus.ProtocolClient blk (BlockProtocol blk)
       -> SomeNodeClientProtocol
