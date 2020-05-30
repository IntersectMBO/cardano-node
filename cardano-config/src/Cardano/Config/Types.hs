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
    , CertificateFile (..)
    , ConfigYamlFilePath (..)
    , ConfigError (..)
    , DbFile (..)
    , GenesisFile (..)
    , HasTxMaxSize (..)
    , LastKnownBlockVersion (..)
    , NodeAddress (..)
    , NodeConfiguration (..)
    , NodeHostAddress (..)
    , Protocol (..)
    , NodeCLI (..)
    , NodeProtocolMode (..)
    , SigningKeyFile (..)
    , SomeConsensusProtocolConstraints
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
    , parseNodeHostAddress
    ) where

import           Prelude (show)
import           Cardano.Prelude hiding (show)

import           Data.Aeson
import           Data.IP (IP)
import           Data.String (String)
import qualified Data.Text as Text
import           Data.Yaml (decodeFileThrow)
import           Network.Socket (PortNumber)
import           System.FilePath ((</>), takeDirectory)
import           System.Posix.Types (Fd(Fd))

import           Cardano.BM.Tracing (ToObject, Transformable)
import qualified Cardano.Chain.Update as Update
import           Cardano.Chain.Slotting (EpochSlots)
import           Cardano.Crypto.ProtocolMagic (RequiresNetworkMagic)
import           Ouroboros.Consensus.Block (Header, BlockProtocol, ForgeState)
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import           Ouroboros.Consensus.Byron.Ledger (byronLedgerState)
import qualified Ouroboros.Consensus.Cardano as Consensus (Protocol, ProtocolClient)
import           Ouroboros.Consensus.HeaderValidation (OtherHeaderEnvelopeError)
import           Ouroboros.Consensus.Ledger.Abstract (LedgerError)
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState, ledgerState)
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTxId, HasTxId, HasTxs(..),
                   LedgerSupportsMempool(..))
import           Ouroboros.Consensus.Mock.Ledger.Block (SimpleBlock)
import           Ouroboros.Consensus.Node.Run (RunNode)
import           Ouroboros.Consensus.NodeId (NodeId(..))
import           Ouroboros.Consensus.Protocol.Abstract (CannotLead, ValidationErr)
import           Ouroboros.Consensus.Util.Condense (Condense (..))
import           Ouroboros.Consensus.Shelley.Ledger (shelleyState, getPParams)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Ledger.Mempool (GenTx, TxId)

import           Ouroboros.Network.Block (HeaderHash, MaxSlotNo(..))

import           Cardano.Chain.Block (adoptedProtocolParameters, cvsUpdateState)
import           Cardano.Chain.Update (ppMaxBlockSize)
import           Cardano.Config.Orphanage ()
import           Cardano.Config.TraceConfig
import           Cardano.Crypto (RequiresNetworkMagic(..))

import           Shelley.Spec.Ledger.PParams (_maxTxSize)

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
  deriving stock (Eq, Ord)
  deriving newtype (FromJSON, IsString, Show)

newtype SigningKeyFile = SigningKeyFile
  { unSigningKeyFile ::  FilePath }
  deriving stock (Eq, Ord)
  deriving newtype (IsString, Show)

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
    , ncTraceConfig :: TraceOptions
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
                               then return TracingOff
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
              deriving (Eq, Generic, Show)

deriving instance NFData Protocol
deriving instance NoUnexpectedThunks Protocol

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
                             <> "Encountered: " <> (Text.pack $ show invalid)

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
     ( HasTxMaxSize (ExtLedgerState blk)
     , RunNode blk
     , TraceConstraints blk
     , Transformable Text IO (ForgeState blk)
     )

data SomeConsensusProtocol where

     SomeConsensusProtocol :: SomeConsensusProtocolConstraints blk
                           => Consensus.Protocol IO blk (BlockProtocol blk)
                           -> SomeConsensusProtocol

class HasTxMaxSize ledgerState where
  getMaxTxSize :: ledgerState -> Word32

instance HasTxMaxSize (ExtLedgerState (ShelleyBlock c)) where
  getMaxTxSize = fromIntegral . _maxTxSize . getPParams . shelleyState . ledgerState

instance HasTxMaxSize (ExtLedgerState ByronBlock) where
  getMaxTxSize =
    fromIntegral . ppMaxBlockSize . adoptedProtocolParameters
      . cvsUpdateState . byronLedgerState . ledgerState

instance HasTxMaxSize (ExtLedgerState (SimpleBlock a b)) where
  getMaxTxSize = const 4242 -- Does not matter for mock blocks.

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
