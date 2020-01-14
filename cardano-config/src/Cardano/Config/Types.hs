{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Config.Types
    ( ConfigError (..)
    , ConfigYamlFilePath (..)
    , CardanoConfiguration (..)
    , CardanoEnvironment (..)
    , Core (..)
    , DbFile (..)
    , DelegationCertFile (..)
    , GenesisFile (..)
    , MiscellaneousFilepaths (..)
    , NodeCLI (..)
    , NodeConfiguration (..)
    , SigningKeyFile (..)
    , SocketFile (..)
    , TopologyFile( ..)
    -- * specific for @Core@
    -- * rest
    , LastKnownBlockVersion (..)
    , Protocol (..)
    , NTP (..)
    , Update (..)
    , Block (..)
    , Node (..)
    , ViewMode (..)
    , TraceOptions (..)
    , parseNodeConfiguration
    ) where

import           Prelude (String, show)
import           Cardano.Prelude

import           Data.Aeson
import qualified Data.Text as T
import           Data.Yaml (decodeFileThrow)

import qualified Cardano.Chain.Update as Update
import           Cardano.BM.Data.Tracer (TracingVerbosity (..))
import           Cardano.Crypto.ProtocolMagic (RequiresNetworkMagic)
import qualified Ouroboros.Consensus.BlockchainTime as Consensus
import           Ouroboros.Consensus.NodeId (NodeId(..))

import           Cardano.Config.Topology
import           Cardano.Config.Orphanage ()
import           Cardano.Crypto (RequiresNetworkMagic(..))

--------------------------------------------------------------------------------
-- Cardano Environment
--------------------------------------------------------------------------------

-- | Just a placeholder for now.
data CardanoEnvironment = NoEnvironment
    deriving (Eq, Show)

-- | Exception type for configuration-related errors.
data ConfigError
  = PartialConfigValue !String

instance Show ConfigError where
  show (PartialConfigValue name)
    = "Undefined CardanoConfiguration value: " <> name

instance Exception ConfigError

--------------------------------------------------------------------------------
-- Cardano Configuration Data Structures
--------------------------------------------------------------------------------
-- | The basic configuration structure. It should contain all the required
-- configuration parameters for the modules to work.

data CardanoConfiguration = CardanoConfiguration
    { ccLogPath             :: !FilePath
    -- ^ The location of the log files on the filesystem.
    , ccLogConfig           :: !(Maybe FilePath)
    -- ^ The location of the log configuration on the filesystem.
    , ccDBPath              :: !FilePath
    -- ^ The location of the DB on the filesystem.
    , ccSocketDir           :: !FilePath
    -- ^ Directory with local sockets:  ${dir}/node-{core,relay}-${node-id}.socket.
    , ccApplicationLockFile :: !FilePath
    -- ^ The location of the application lock file that is used
    -- as a semaphore se we can run just one application
    -- instance at a time.
    , ccTraceOptions        :: !TraceOptions
    -- ^ Tracer options
    , ccTopologyInfo        :: !TopologyInfo
    -- ^ The network topology.
    , ccNodeAddress         :: !NodeAddress
    -- ^ The node ip address and port number.
    , ccProtocol            :: !Protocol
    -- ^ The selected protocol.
    , ccViewMode            :: !ViewMode
    -- ^ View mode of the TUI
    , ccLogMetrics          :: !Bool
    -- ^ Flag to capture log metrics or not.
    , ccCore                :: !Core
    , ccUpdate              :: !Update
    , ccNode                :: !Node
    } deriving (Eq, Show)

data NodeCLI = NodeCLI
    { mscFp :: !MiscellaneousFilepaths
    , genesisHash :: !Text
    , nodeAddr :: !NodeAddress
    , configFp :: !ConfigYamlFilePath
    , traceOpts :: !TraceOptions
    , validateDB :: !Bool
    } deriving Show

-- | Filepath of the configuration yaml file. This file determines
-- all the configuration settings required for the cardano node
-- (logging, tracing, protocol, slot length etc)
newtype ConfigYamlFilePath = ConfigYamlFilePath
  { unConfigPath :: FilePath }
  deriving Show

data MiscellaneousFilepaths = MiscellaneousFilepaths
  { topFile :: !TopologyFile
  , dBFile :: !DbFile
  , genesisFile :: !GenesisFile
  , delegCertFile :: !(Maybe DelegationCertFile)
  , signKeyFile :: !(Maybe SigningKeyFile)
  , socketFile :: !SocketFile
  } deriving Show

newtype TopologyFile = TopologyFile
  { unTopology :: FilePath }
  deriving Show

newtype DbFile = DbFile
  { unDB :: FilePath }
  deriving Show

newtype GenesisFile = GenesisFile
  { unGenesisFile :: FilePath }
  deriving (Eq, Ord, Show, IsString)

newtype DelegationCertFile = DelegationCertFile
  { unDelegationCert :: FilePath }
  deriving Show

newtype SocketFile = SocketFile
  { unSocket :: FilePath }
  deriving Show

newtype SigningKeyFile = SigningKeyFile
  { unSigningKey ::  FilePath }
  deriving (Eq, Ord, Show, IsString)

data NodeConfiguration =
    NodeConfiguration
      { ncProtocol :: Protocol
      , ncNodeId :: Maybe NodeId
      , ncNumCoreNodes :: Maybe Int
      , ncReqNetworkMagic :: RequiresNetworkMagic
      , ncPbftSignatureThresh :: Maybe Double
      , ncLoggingSwitch :: Bool
      , ncLogMetrics :: Bool
      , ncViewMode :: ViewMode
      , ncUpdate :: Update
      } deriving (Show)

instance FromJSON NodeConfiguration where
    parseJSON = withObject "NodeConfiguration" $ \v -> do
                  nId <- v .:? "NodeId"
                  ptcl <- v .: "Protocol" .!= RealPBFT
                  numCoreNode <- v .:? "NumCoreNodes"
                  rNetworkMagic <- v .:? "RequiresNetworkMagic" .!= RequiresNoMagic
                  pbftSignatureThresh <- v .:? "PBftSignatureThreshold"
                  loggingSwitch <- v .:? "TurnOnLogging" .!= True
                  vMode <- v .:? "ViewMode" .!= LiveView
                  logMetrics <- v .:? "TurnOnLogMetrics" .!= True

                  -- Update Parameters
                  appName <- v .:? "ApplicationName" .!= Update.ApplicationName "cardano-sl"
                  appVersion <- v .:? "ApplicationVersion" .!= 1
                  lkBlkVersionMajor <- v .:? "LastKnownBlockVersion-Major" .!= 0
                  lkBlkVersionMinor <- v .:? "LastKnownBlockVersion-Minor" .!= 2
                  lkBlkVersionAlt <- v .:? "LastKnownBlockVersion-Alt" .!= 0

                  pure $ NodeConfiguration
                           ptcl
                           nId
                           numCoreNode
                           rNetworkMagic
                           pbftSignatureThresh
                           loggingSwitch
                           logMetrics
                           vMode
                           (Update appName appVersion (LastKnownBlockVersion
                                                         lkBlkVersionMajor
                                                         lkBlkVersionMinor
                                                         lkBlkVersionAlt))


parseNodeConfiguration :: FilePath -> IO NodeConfiguration
parseNodeConfiguration fp = decodeFileThrow fp

-- | Core configuration.
-- For now, we only store the path to the genesis file(s) and their hash.
-- The rest is in the hands of the modules/features that need to use it.
-- The info flow is:
-- __genesis config ---> genesis file__
-- And separately:
-- __genesis file ---> runtime config ---> running node__
-- __static config ---> ...__
data Core = Core
    { coGenesisFile                 :: !FilePath
    -- ^ Genesis source file JSON.
    , coGenesisHash                 :: !Text
    -- ^ Genesis previous block hash.
    , coNodeId                      :: !(Maybe Int) -- TODO: Remove!
    -- ^ Core node ID, the number of the node.
    , coNumCoreNodes                :: !(Maybe Int)
    -- ^ The number of the core nodes.
    , coStaticKeySigningKeyFile     :: !(Maybe FilePath)
    -- ^ Static key signing file.
    , coStaticKeyDlgCertFile        :: !(Maybe FilePath)
    -- ^ Static key delegation certificate.
    , coRequiresNetworkMagic        :: !RequiresNetworkMagic
    -- ^ Do we require the network byte indicator for mainnet, testnet or staging?
    , coPBftSigThd                  :: !(Maybe Double)
    -- ^ PBFT signature threshold system parameters

    } deriving (Eq, Show)

-- TODO:  we don't want ByronLegacy in Protocol.  Let's wrap Protocol with another
-- sum type for cases where it's required.
data Protocol = ByronLegacy
              | BFT
              | Praos
              | MockPBFT
              | RealPBFT
              deriving (Eq, Show)

instance FromJSON Protocol where
  parseJSON (String str) = case str of
                            "ByronLegacy" -> pure ByronLegacy
                            "BFT" -> pure BFT
                            "Praos" -> pure Praos
                            "MockPBFT" -> pure MockPBFT
                            "RealPBFT" -> pure RealPBFT
                            ptcl -> panic $ "Parsing of Protocol: "
                                          <> ptcl <> " failed. "
                                          <> ptcl <> " is not a valid protocol"
  parseJSON invalid  = panic $ "Parsing of Protocol failed due to type mismatch. "
                             <> "Encountered: " <> (T.pack $ Prelude.show invalid)


data NTP = NTP
    { ntpResponseTimeout :: !Int
    , ntpPollDelay       :: !Int
    , ntpServers         :: ![Text]
    } deriving (Eq, Show)

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

data Block = Block
    { blNetworkDiameter        :: !Int
      -- ^Estimated time needed to broadcast message from one node to all other nodes.
    , blRecoveryHeadersMessage :: !Int
      -- ^Maximum amount of headers node can put into headers message while in "after offline" or "recovery" mode.
    , blStreamWindow           :: !Int
      -- ^ Number of blocks to have inflight
    , blNonCriticalCQBootstrap :: !Double
      -- ^ If chain quality in bootstrap era is less than this value, non critical misbehavior will be reported.
    , blNonCriticalCQ          :: !Double
      -- ^ If chain quality after bootstrap era is less than this value, non critical misbehavior will be reported.
    , blCriticalCQ             :: !Double
      -- ^ If chain quality after bootstrap era is less than this value, critical misbehavior will be reported.
    , blCriticalCQBootstrap    :: !Double
      -- ^ If chain quality in bootstrap era is less than this value, critical misbehavior will be reported.
    , blCriticalForkThreshold  :: !Int
      -- ^ Number of blocks such that if so many blocks are rolled back, it requires immediate reaction.
    , blFixedTimeCQ            :: !Int
      -- ^ Chain quality will be also calculated for this amount of seconds.
    } deriving (Eq, Show)

--- | Top-level Cardano SL node configuration
data Node = Node
    { noSlotLength                      :: !Consensus.SlotLength
    -- ^ Slot length time.
    , noNetworkConnectionTimeout        :: !Int
    -- ^ Network connection timeout in milliseconds.
    , noHandshakeTimeout                :: !Int
    -- ^ Protocol acknowledgement timeout in milliseconds.
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
  parseJSON invalid  = panic $ "Parsing of ViewMode failed due to type mismatch. "
                             <> "Encountered: " <> (T.pack $ Prelude.show invalid)

-- | Detailed tracing options. Each option enables a tracer
--   which verbosity to the log output.
data TraceOptions = TraceOptions
  { traceVerbosity :: !TracingVerbosity
  , traceChainDB :: !Bool
    -- ^ By default we use 'readableChainDB' tracer, if on this it will use
    -- more verbose tracer

    -- Consensus Tracers --
  , traceChainSyncClient :: !Bool
  , traceChainSyncHeaderServer :: !Bool
  , traceChainSyncBlockServer :: !Bool
  , traceBlockFetchDecisions :: !Bool
  , traceBlockFetchClient :: !Bool
  , traceBlockFetchServer :: !Bool
  , traceTxInbound :: !Bool
  , traceTxOutbound :: !Bool
  , traceLocalTxSubmissionServer :: !Bool
  , traceMempool :: !Bool
  , traceForge :: !Bool
    -----------------------

    -- Protocol Tracers --
  , traceChainSyncProtocol :: !Bool
    -- There's two variants of the block fetch tracer and for now
    -- at least we'll set them both together from the same flags.
  , traceBlockFetchProtocol :: !Bool
  , traceBlockFetchProtocolSerialised :: !Bool
  , traceTxSubmissionProtocol :: !Bool
  , traceLocalChainSyncProtocol :: !Bool
  , traceLocalTxSubmissionProtocol :: !Bool
  , traceIpSubscription :: !Bool
    -----------------------

  , traceDnsSubscription :: !Bool
  , traceDnsResolver :: !Bool
  , traceErrorPolicy :: !Bool
  , traceMux :: !Bool
  } deriving (Eq, Show)
