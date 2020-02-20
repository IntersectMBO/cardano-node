{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Config.Types
    ( ConfigYamlFilePath (..)
    , CardanoEnvironment (..)
    , DbFile (..)
    , DelegationCertFile (..)
    , GenesisFile (..)
    , LastKnownBlockVersion (..)
    , MiscellaneousFilepaths (..)
    , NodeAddress (..)
    , NodeConfiguration (..)
    , NodeHostAddress (..)
    , Protocol (..)
    , NodeMockCLI (..)
    , NodeCLI (..)
    , NodeProtocolMode (..)
    , SigningKeyFile (..)
    , TopologyFile (..)
    , TraceOptions (..)
    , SocketPath (..)
    , Update (..)
    , ViewMode (..)
    , parseNodeConfiguration
    , parseNodeConfigurationFP
    ) where

import           Prelude (read)
import           Cardano.Prelude

import           Data.Aeson
import qualified Data.IP as IP
import qualified Data.Text as T
import           Data.Yaml (decodeFileThrow)
import           Network.Socket (PortNumber)

import qualified Cardano.Chain.Update as Update
import           Cardano.BM.Data.Tracer (TracingVerbosity (..))
import           Cardano.Crypto.ProtocolMagic (RequiresNetworkMagic)
import           Ouroboros.Consensus.NodeId (NodeId(..))
import           Ouroboros.Consensus.Util.Condense (Condense (..))

import           Cardano.Config.Orphanage ()
import           Cardano.Crypto (RequiresNetworkMagic(..))

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
    { mscFp :: !MiscellaneousFilepaths
    , genesisHash :: !Text
    , nodeAddr :: !NodeAddress
    , configFp :: !ConfigYamlFilePath
    , validateDB :: !Bool
    }

data NodeMockCLI = NodeMockCLI
    { mockMscFp :: !MiscellaneousFilepaths
    , mockGenesisHash :: !Text
    , mockNodeAddr :: !NodeAddress
    , mockConfigFp :: !ConfigYamlFilePath
    , mockValidateDB :: !Bool
    } deriving Show

-- | Mock protocols requires different parameters to real protocols.
-- Therefore we distinguish this at the top level on the command line.
data NodeProtocolMode = MockProtocolMode NodeMockCLI
                      | RealProtocolMode NodeCLI

-- | Filepath of the configuration yaml file. This file determines
-- all the configuration settings required for the cardano node
-- (logging, tracing, protocol, slot length etc)
newtype ConfigYamlFilePath = ConfigYamlFilePath
  { unConfigPath :: FilePath }
  deriving Show

data MiscellaneousFilepaths = MiscellaneousFilepaths
  { topFile :: !TopologyFile
  , dBFile :: !DbFile
  , genesisFile :: !(Maybe GenesisFile)
  , delegCertFile :: !(Maybe DelegationCertFile)
  , signKeyFile :: !(Maybe SigningKeyFile)
  , socketFile :: !SocketPath
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

data SocketPath = SocketFile
  { unSocket :: FilePath }
  deriving (Eq, Ord, Show)

newtype SigningKeyFile = SigningKeyFile
  { unSigningKey ::  FilePath }
  deriving (Eq, Ord, Show, IsString)

data NodeConfiguration =
    NodeConfiguration
      { ncProtocol :: Protocol
      , ncNodeId :: Maybe NodeId
      , ncNumCoreNodes :: Maybe Word64
      , ncReqNetworkMagic :: RequiresNetworkMagic
      , ncPbftSignatureThresh :: Maybe Double
      , ncLoggingSwitch :: Bool
      , ncLogMetrics :: Bool
      , ncTraceOptions :: TraceOptions
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

                  -- Trace Options
                  tOptions <- TraceOptions
                                <$> v .:? "TracingVerbosity" .!= NormalVerbosity
                                <*> v .:? "TraceBlockFetchClient" .!= False
                                <*> v .:? "TraceBlockFetchDecisions" .!= True
                                <*> v .:? "TraceBlockFetchProtocol" .!= False
                                <*> v .:? "TraceBlockFetchProtocolSerialised" .!= False
                                <*> v .:? "TraceBlockFetchServer" .!= False
                                <*> v .:? "TraceChainDb" .!= True
                                <*> v .:? "TraceChainSyncClient" .!= True
                                <*> v .:? "TraceChainSyncBlockServer" .!= False
                                <*> v .:? "TraceChainSyncHeaderServer" .!= False
                                <*> v .:? "TraceChainSyncProtocol" .!= False
                                <*> v .:? "TraceDNSResolver" .!= False
                                <*> v .:? "TraceDNSSubscription" .!= True
                                <*> v .:? "TraceErrorPolicy" .!= True
                                <*> v .:? "TraceLocalErrorPolicy" .!= True
                                <*> v .:? "TraceForge" .!= True
                                <*> v .:? "TraceIpSubscription" .!= True
                                <*> v .:? "TraceLocalChainSyncProtocol" .!= False
                                <*> v .:? "TraceLocalTxSubmissionProtocol" .!= False
                                <*> v .:? "TraceLocalTxSubmissionServer" .!= False
                                <*> v .:? "TraceLocalStateQueryProtocol" .!= False
                                <*> v .:? "TraceMempool" .!= True
                                <*> v .:? "TraceMux" .!= True
                                <*> v .:? "TraceTxInbound" .!= False
                                <*> v .:? "TraceTxOutbound" .!= False
                                <*> v .:? "TraceTxSubmissionProtocol" .!= False

                  pure $ NodeConfiguration
                           ptcl
                           nId
                           numCoreNode
                           rNetworkMagic
                           pbftSignatureThresh
                           loggingSwitch
                           logMetrics
                           tOptions
                           vMode
                           (Update appName appVersion (LastKnownBlockVersion
                                                         lkBlkVersionMajor
                                                         lkBlkVersionMinor
                                                         lkBlkVersionAlt))


parseNodeConfigurationFP :: FilePath -> IO NodeConfiguration
parseNodeConfigurationFP fp = decodeFileThrow fp

parseNodeConfiguration :: NodeProtocolMode -> IO NodeConfiguration
parseNodeConfiguration npm =
  case npm of
    MockProtocolMode (NodeMockCLI _ _ _ cy _) -> decodeFileThrow $ unConfigPath cy
    RealProtocolMode (NodeCLI _ _ _ cy _) -> decodeFileThrow $ unConfigPath cy

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
  parseJSON invalid  = panic $ "Parsing of ViewMode failed due to type mismatch. "
                             <> "Encountered: " <> (T.pack $ show invalid)

-- | Detailed tracing options. Each option enables a tracer
--   which verbosity to the log output.
data TraceOptions = TraceOptions
  { traceVerbosity :: !TracingVerbosity
  , traceBlockFetchClient :: !Bool
  , traceBlockFetchDecisions :: !Bool
  , traceBlockFetchProtocol :: !Bool
  , traceBlockFetchProtocolSerialised :: !Bool
  , traceBlockFetchServer :: !Bool
  , traceChainDB :: !Bool
  , traceChainSyncClient :: !Bool
  , traceChainSyncBlockServer :: !Bool
  , traceChainSyncHeaderServer :: !Bool
  , traceChainSyncProtocol :: !Bool
  , traceDnsResolver :: !Bool
  , traceDnsSubscription :: !Bool
  , traceErrorPolicy :: !Bool
  , traceLocalErrorPolicy :: !Bool
  , traceForge :: !Bool
  , traceIpSubscription :: !Bool
  , traceLocalChainSyncProtocol :: !Bool
  , traceLocalTxSubmissionProtocol :: !Bool
  , traceLocalTxSubmissionServer :: !Bool
  , traceLocalStateQueryProtocol :: !Bool
  , traceMempool :: !Bool
  , traceMux :: !Bool
  , traceTxInbound :: !Bool
  , traceTxOutbound :: !Bool
  , traceTxSubmissionProtocol :: !Bool
  } deriving (Eq, Show)

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
      <$> (NodeHostAddress . Just <$> read <$> v .: "addr")
      <*> ((fromIntegral :: Int -> PortNumber) <$> v .: "port")

newtype NodeHostAddress = NodeHostAddress { getAddress :: Maybe IP.IP }
                          deriving (Eq, Ord, Show)

instance FromJSON NodeHostAddress where
  parseJSON (String ipStr) = case readMaybe $ T.unpack ipStr of
                               Just ip -> pure . NodeHostAddress $ Just ip
                               Nothing -> pure $ NodeHostAddress Nothing
  parseJSON invalid = panic $ "Parsing of IP failed due to type mismatch. "
                            <> "Encountered: " <> (T.pack $ show invalid)
