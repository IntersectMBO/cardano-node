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
    , NodeCLI (..)
    , NodeConfiguration (..)
    , Protocol (..)
    , SigningKeyFile (..)
    , SocketFile (..)
    , TopologyFile (..)
    , TraceOptions (..)
    , Update (..)
    , ViewMode (..)
    , parseNodeConfiguration
    ) where

import           Prelude (show)
import           Cardano.Prelude

import           Data.Aeson
import qualified Data.Text as T
import           Data.Yaml (decodeFileThrow)

import qualified Cardano.Chain.Update as Update
import           Cardano.BM.Data.Tracer (TracingVerbosity (..))
import           Cardano.Crypto.ProtocolMagic (RequiresNetworkMagic)
import           Ouroboros.Consensus.NodeId (NodeId(..))

import           Cardano.Config.Topology
import           Cardano.Config.Orphanage ()
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
      , ncTraceOptions :: TraceOptions
      , ncViewMode :: ViewMode
      , ncUpdate :: Update
      } deriving (Show)

instance FromJSON NodeConfiguration where
    parseJSON = withObject "NodeConfiguration" $ \v -> do
                  nId <- v .:? "NodeId"
                  ptcl <- v .: "Protocol"
                  numCoreNode <- v .:? "NumCoreNodes"
                  rNetworkMagic <- v .: "RequiresNetworkMagic"
                  pbftSignatureThresh <- v .:? "PBftSignatureThreshold"
                  loggingSwitch <- v .: "TurnOnLogging"
                  vMode <- v .: "ViewMode"
                  logMetrics <- v .: "TurnOnLogMetrics"

                  -- Update Parameters
                  appName <- v .: "ApplicationName"
                  appVersion <- v .: "ApplicationVersion"
                  lkBlkVersionMajor <- v .: "LastKnownBlockVersion-Major"
                  lkBlkVersionMinor <- v .: "LastKnownBlockVersion-Minor"
                  lkBlkVersionAlt <- v .: "LastKnownBlockVersion-Alt"

                  -- Trace Options
                  tOptions <- TraceOptions
                                <$> v .: "TracingVerbosity"
                                <*> v .: "TraceBlockFetchClient"
                                <*> v .: "TraceBlockFetchDecisions"
                                <*> v .: "TraceBlockFetchProtocol"
                                <*> v .: "TraceBlockFetchProtocolSerialised"
                                <*> v .: "TraceBlockFetchServer"
                                <*> v .: "TraceChainDb"
                                <*> v .: "TraceChainSyncClient"
                                <*> v .: "TraceChainSyncBlockServer"
                                <*> v .: "TraceChainSyncHeaderServer"
                                <*> v .: "TraceChainSyncProtocol"
                                <*> v .: "TraceDNSResolver"
                                <*> v .: "TraceDNSSubscription"
                                <*> v .: "TraceErrorPolicy"
                                <*> v .: "TraceForge"
                                <*> v .: "TraceIpSubscription"
                                <*> v .: "TraceLocalChainSyncProtocol"
                                <*> v .: "TraceLocalTxSubmissionProtocol"
                                <*> v .: "TraceLocalTxSubmissionServer"
                                <*> v .: "TraceMempool"
                                <*> v .: "TraceMux"
                                <*> v .: "TraceTxInbound"
                                <*> v .: "TraceTxOutbound"
                                <*> v .: "TraceTxSubmissionProtocol"

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


parseNodeConfiguration :: FilePath -> IO NodeConfiguration
parseNodeConfiguration fp = decodeFileThrow fp

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
                             <> "Encountered: " <> (T.pack $ Prelude.show invalid)

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
  , traceForge :: !Bool
  , traceIpSubscription :: !Bool
  , traceLocalChainSyncProtocol :: !Bool
  , traceLocalTxSubmissionProtocol :: !Bool
  , traceLocalTxSubmissionServer :: !Bool
  , traceMempool :: !Bool
  , traceMux :: !Bool
  , traceTxInbound :: !Bool
  , traceTxOutbound :: !Bool
  , traceTxSubmissionProtocol :: !Bool
  } deriving (Eq, Show)
