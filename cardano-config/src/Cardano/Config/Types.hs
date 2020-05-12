{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Config.Types
    ( CardanoEnvironment (..)
    , CBORObject (..)
    , CertificateFile (..)
    , ConfigYamlFilePath (..)
    , ConfigError (..)
    , DbFile (..)
    , GenesisFile (..)
    , LastKnownBlockVersion (..)
    , NodeAddress (..)
    , NodeConfiguration (..)
    , NodeHostAddress (..)
    , Protocol (..)
    , NodeCLI (..)
    , NodeProtocolMode (..)
    , SigningKeyFile (..)
    , ProtocolFilepaths (..)
    , SomeConsensusProtocol (..)
    , TopologyFile (..)
    , TraceConstraints
    , SocketPath (..)
    , Update (..)
    , UpdateProposalFile (..)
    , ViewMode (..)
    , Fd (..)
    , parseNodeConfiguration
    , parseNodeConfigurationFP
    , SomeNodeClientProtocol(..)
    ) where

import           Prelude (show)
import           Cardano.Prelude hiding (show)

import           Data.Aeson
import qualified Data.IP as IP
import qualified Data.Text as T
import           Data.Yaml (decodeFileThrow)
import           Network.Socket (PortNumber)
import           System.FilePath ((</>), takeDirectory)
import           System.Posix.Types (Fd(Fd))

import           Cardano.BM.Tracing (ToObject)
import qualified Cardano.Chain.Update as Update
import           Cardano.Chain.Slotting (EpochSlots)
import           Cardano.Crypto.ProtocolMagic (RequiresNetworkMagic)
import           Ouroboros.Consensus.Block (Header, BlockProtocol)
import qualified Ouroboros.Consensus.Cardano as Consensus (Protocol, ProtocolClient)
import           Ouroboros.Consensus.HeaderValidation (OtherHeaderEnvelopeError)
import           Ouroboros.Consensus.Ledger.Abstract (LedgerError)
import           Ouroboros.Consensus.Mempool.API
                   (GenTx, GenTxId, HasTxId, HasTxs(..), ApplyTxErr, TxId)
import           Ouroboros.Consensus.Node.Run (RunNode)
import           Ouroboros.Consensus.NodeId (NodeId(..))
import           Ouroboros.Consensus.Protocol.Abstract (ValidationErr)
import           Ouroboros.Consensus.Util.Condense (Condense (..))
import           Ouroboros.Network.Block (HeaderHash, MaxSlotNo(..))

import           Cardano.Config.Orphanage ()
import           Cardano.Config.TraceConfig
import           Cardano.Crypto (RequiresNetworkMagic(..))

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
  , nodeAddr        :: !NodeAddress
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
  deriving newtype Show
  deriving (Eq, Ord, IsString)

instance FromJSON GenesisFile where
  parseJSON (String genFp) = pure . GenesisFile $ T.unpack genFp
  parseJSON invalid = panic $ "Parsing of GenesisFile failed due to type mismatch. "
                           <> "Encountered: " <> (T.pack $ show invalid)

-- Encompasses staking certificates, stake pool certificates,
-- genesis delegate certificates and MIR certificates.
newtype CertificateFile = CertificateFile
  { unCertificateFile :: FilePath }
  deriving newtype (Eq, Show)

newtype UpdateProposalFile = UpdateProposalFile
  { unUpdateProposalFile :: FilePath }
  deriving newtype (Eq, Show)

newtype SocketPath = SocketPath
  { unSocketPath :: FilePath }
  deriving newtype (Show, FromJSON)
  deriving (Eq, Ord, IsString)

newtype SigningKeyFile = SigningKeyFile
  { unSigningKeyFile ::  FilePath }
  deriving newtype Show
  deriving (Eq, Ord, IsString)

data NodeConfiguration =
  NodeConfiguration
    { ncProtocol :: Protocol
    , ncGenesisFile :: GenesisFile
    , ncNodeId :: Maybe NodeId
    , ncNumCoreNodes :: Maybe Word64
    , ncReqNetworkMagic :: RequiresNetworkMagic
    , ncPbftSignatureThresh :: Maybe Double
    , ncLoggingSwitch :: Bool
    , ncLogMetrics :: Bool
    , ncSocketPath :: Maybe SocketPath
    , ncTraceConfig :: TraceConfig
    , ncViewMode :: ViewMode
    , ncUpdate :: Update
    } deriving Show

instance FromJSON NodeConfiguration where
  parseJSON = withObject "NodeConfiguration" $ \v -> do
                nId <- v .:? "NodeId"
                ptcl <- v .: "Protocol" .!= RealPBFT
                genFile <- v .: "GenesisFile" .!= "genesis/genesis.json"
                numCoreNode <- v .:? "NumCoreNodes"
                rNetworkMagic <- v .:? "RequiresNetworkMagic" .!= RequiresNoMagic
                pbftSignatureThresh <- v .:? "PBftSignatureThreshold"
                vMode <- v .:? "ViewMode" .!= LiveView
                socketPath <- v .:? "SocketPath"

                -- Update Parameters
                appName <- v .:? "ApplicationName" .!= Update.ApplicationName "cardano-sl"
                appVersion <- v .:? "ApplicationVersion" .!= 1
                lkBlkVersionMajor <- v .:? "LastKnownBlockVersion-Major" .!= 0
                lkBlkVersionMinor <- v .:? "LastKnownBlockVersion-Minor" .!= 2
                lkBlkVersionAlt <- v .:? "LastKnownBlockVersion-Alt" .!= 0

                -- Logging
                loggingSwitch <- v .:? "TurnOnLogging" .!= True
                logMetrics <- v .:? "TurnOnLogMetrics" .!= True
                traceConfig <- if not loggingSwitch
                               then pure traceConfigMute
                               else traceConfigParser v

                pure $ NodeConfiguration
                         { ncProtocol = ptcl
                         , ncGenesisFile = genFile
                         , ncNodeId = nId
                         , ncNumCoreNodes = numCoreNode
                         , ncReqNetworkMagic = rNetworkMagic
                         , ncPbftSignatureThresh = pbftSignatureThresh
                         , ncLoggingSwitch = loggingSwitch
                         , ncLogMetrics = logMetrics
                         , ncSocketPath = socketPath
                         , ncTraceConfig = traceConfig
                         , ncViewMode = vMode
                         , ncUpdate = (Update appName appVersion (LastKnownBlockVersion
                                                                    lkBlkVersionMajor
                                                                    lkBlkVersionMinor
                                                                    lkBlkVersionAlt))
                         }

parseNodeConfigurationFP :: ConfigYamlFilePath -> IO NodeConfiguration
parseNodeConfigurationFP (ConfigYamlFilePath fp) = do
 nc  <- decodeFileThrow fp
 let genFile = unGenesisFile $ ncGenesisFile nc
 -- Make genesis file relative to configuration yaml filepath.
 let d = takeDirectory fp
 pure $ nc { ncGenesisFile = GenesisFile $ d </> genFile }

parseNodeConfiguration :: NodeCLI -> IO NodeConfiguration
parseNodeConfiguration NodeCLI{configFile} = parseNodeConfigurationFP configFile

data Protocol = BFT
              | Praos
              | MockPBFT
              | RealPBFT
              | TPraos
              deriving (Eq, Show)

instance FromJSON Protocol where
  parseJSON (String str) = case str of
                            "BFT" -> pure BFT
                            "Praos" -> pure Praos
                            "MockPBFT" -> pure MockPBFT
                            "RealPBFT" -> pure RealPBFT
                            "TPraos"   -> pure TPraos
                            ptcl -> panic $ "Parsing of Protocol: "
                                          <> ptcl <> " failed. "
                                          <> ptcl <> " is not a valid protocol"
  parseJSON invalid  = panic $ "Parsing of Protocol failed due to type mismatch. "
                             <> "Encountered: " <> (T.pack $ show invalid)

-- TODO: migrate to Update.SoftwareVersion
data Update = Update
    { upApplicationName       :: !Update.ApplicationName
    -- ^ Update application name.
    , upApplicationVersion    :: !Update.NumSoftwareVersion
    -- application version.
    , upLastKnownBlockVersion :: !LastKnownBlockVersion
    -- ^ Update last known block version.
    } deriving (Eq, Show)

-- TODO: migrate to Update.ProtocolVersion
data LastKnownBlockVersion = LastKnownBlockVersion
    { lkbvMajor :: !Word16
    -- ^ Last known block version major.
    , lkbvMinor :: !Word16
    -- ^ Last known block version minor.
    , lkbvAlt   :: !Word8
    -- ^ Last known block version alternative.
    } deriving (Eq, Show)

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
                            <> "Encountered: " <> (T.pack $ show invalid)

--------------------------------------------------------------------------------
-- Cardano Topology Related Data Structures
--------------------------------------------------------------------------------

-- | IPv4 address with a port number.
data NodeAddress = NodeAddress
  { naHostAddress :: !(Maybe NodeHostAddress)
  , naPort :: !PortNumber
  } deriving (Eq, Ord, Show)

instance Condense NodeAddress where
  condense (NodeAddress addr port) = show addr ++ ":" ++ show port

instance FromJSON NodeAddress where
  parseJSON = withObject "NodeAddress" $ \v -> do
    NodeAddress
      <$> (maybe Nothing (Just . NodeHostAddress) <$> readMaybe <$> v .: "addr")
      <*> ((fromIntegral :: Int -> PortNumber) <$> v .: "port")

instance ToJSON NodeAddress where
  toJSON na =
    object
      [ "addr" .= maybe "null" toJSON (naHostAddress na)
      , "port" .= (fromIntegral (naPort na) :: Int)
      ]

newtype NodeHostAddress
  = NodeHostAddress { unNodeHostAddress :: IP.IP }
  deriving newtype Show
  deriving (Eq, Ord)

instance FromJSON NodeHostAddress where
  parseJSON (String ipStr) =
    case readMaybe $ T.unpack ipStr of
      Just ip -> pure $ NodeHostAddress ip
      Nothing -> panic $ "Parsing of IP failed: " <> ipStr
  parseJSON invalid = panic $ "Parsing of IP failed due to type mismatch. "
                            <> "Encountered: " <> (T.pack $ show invalid)

instance ToJSON NodeHostAddress where
  toJSON mha =
    String (T.pack . show $ unNodeHostAddress mha)

--------------------------------------------------------------------------------
-- Protocol & Tracing Related
--------------------------------------------------------------------------------

data SomeConsensusProtocol where

     SomeConsensusProtocol :: (RunNode blk, TraceConstraints blk)
                           => Consensus.Protocol blk (BlockProtocol blk)
                           -> SomeConsensusProtocol


-- | Tracing-related constraints for monitoring purposes.
--
-- When you need a 'Show' or 'Condense' instance for more types, just add the
-- appropriate constraint here. There's no need to modify the consensus
-- code-base, unless the corresponding instance is missing.
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
    , Show (Header blk)
    , Show (TxId (GenTx blk))
    , ToJSON   (TxId (GenTx blk))
    , ToObject (GenTx blk)
    , ToObject (Header blk)
    , ToObject (LedgerError blk)
    , ToObject (OtherHeaderEnvelopeError blk)
    , ToObject (ValidationErr (BlockProtocol blk))
    )

--------------------------------------------------------------------------------
-- Node client requirements
--------------------------------------------------------------------------------

data SomeNodeClientProtocol where

     SomeNodeClientProtocol
       :: RunNode blk
       => Consensus.ProtocolClient blk (BlockProtocol blk)
       -> SomeNodeClientProtocol
