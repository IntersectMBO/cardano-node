{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Node.Types
  ( -- * Configuration
    AdjustFilePaths(..)
  , ConfigError(..)
  , ConfigYamlFilePath(..)
  , DbFile(..)
  , GenesisFile(..)
  , ProtocolFilepaths (..)
  , GenesisHash(..)
  , MaxConcurrencyBulkSync(..)
  , MaxConcurrencyDeadline(..)
    -- * Node addresses
  , NodeAddress'(..)
  , NodeIPAddress
  , nodeAddressToSockAddr
  , NodeIPv4Address
  , NodeIPv6Address
  , NodeDnsAddress
  , nodeIPv4ToIPAddress
  , nodeIPv6ToIPAddress
  , nodeDnsAddressToDomainAddress
  , NodeHostIPAddress (..)
  , nodeHostIPAddressToSockAddr
  , NodeHostIPv4Address (..)
  , NodeHostIPv6Address (..)
  , nodeHostIPv4AddressToIPAddress
  , nodeHostIPv6AddressToIPAddress
  , NodeHostDnsAddress (..)
  , nodeHostDnsAddressToDomain
  , PortNumber
  , SocketPath(..)
  , TopologyFile(..)
    -- * Consensus protocol configuration
  , NodeByronProtocolConfiguration(..)
  , NodeHardForkProtocolConfiguration(..)
  , NodeProtocolConfiguration(..)
  , NodeShelleyProtocolConfiguration(..)
  , ViewMode(..)
  , protocolName
  ) where

import           Cardano.Prelude
import           Prelude (String)

import           Data.Aeson
import           Data.IP (IP (..), IPv4, IPv6)
import qualified Data.IP as IP
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Network.Socket (PortNumber, SockAddr (..))
import qualified Network.DNS as DNS (Domain)

import           Cardano.Api.Typed (EpochNo)
import qualified Cardano.Chain.Update as Byron
import           Cardano.Crypto (RequiresNetworkMagic (..))
import qualified Cardano.Crypto.Hash as Crypto
import           Cardano.Node.Protocol.Types (Protocol (..))
import           Ouroboros.Network.PeerSelection.RootPeersDNS (DomainAddress (..))

--TODO: things will probably be clearer if we don't use these newtype wrappers and instead
-- use records with named fields in the CLI code.

-- | Errors for the cardano-config module.
newtype ConfigError = ConfigErrorFileNotFound FilePath
    deriving Show

-- | Filepath of the configuration yaml file. This file determines
-- all the configuration settings required for the cardano node
-- (logging, tracing, protocol, slot length etc)
newtype ConfigYamlFilePath = ConfigYamlFilePath
  { unConfigPath :: FilePath }
  deriving newtype (Eq, Show)

newtype DbFile = DbFile
  { unDB :: FilePath }
  deriving newtype (Eq, Show)

newtype GenesisFile = GenesisFile
  { unGenesisFile :: FilePath }
  deriving stock (Eq, Ord)
  deriving newtype (IsString, Show)

instance FromJSON GenesisFile where
  parseJSON (String genFp) = pure . GenesisFile $ Text.unpack genFp
  parseJSON invalid = panic $ "Parsing of GenesisFile failed due to type mismatch. "
                           <> "Encountered: " <> Text.pack (show invalid)

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
                            <> "Encountered: " <> Text.pack (show invalid)

newtype MaxConcurrencyBulkSync = MaxConcurrencyBulkSync
  { unMaxConcurrencyBulkSync :: Word }
  deriving stock (Eq, Ord)
  deriving newtype (FromJSON, Show)

newtype MaxConcurrencyDeadline = MaxConcurrencyDeadline
  { unMaxConcurrencyDeadline :: Word }
  deriving stock (Eq, Ord)
  deriving newtype (FromJSON, Show)


-- | IPv4 or IPv6 address with a port number.
data NodeAddress' addr = NodeAddress
  { naHostAddress :: !addr
  , naPort :: !PortNumber
  } deriving (Eq, Ord, Show, Functor)

type NodeIPAddress   = NodeAddress' NodeHostIPAddress
type NodeIPv4Address = NodeAddress' NodeHostIPv4Address
type NodeIPv6Address = NodeAddress' NodeHostIPv6Address
type NodeDnsAddress  = NodeAddress' NodeHostDnsAddress


instance FromJSON addr => FromJSON (NodeAddress' addr) where
  parseJSON = withObject "NodeAddress" $ \v -> do
    NodeAddress
      <$> v .: "addr"
      <*> ((fromIntegral :: Int -> PortNumber) <$> v .: "port")

instance ToJSON addr => ToJSON (NodeAddress' addr) where
  toJSON na =
    object
      [ "addr" .= toJSON (naHostAddress na)
      , "port" .= (fromIntegral (naPort na) :: Int)
      ]


nodeIPv4ToIPAddress :: NodeIPv4Address -> NodeIPAddress
nodeIPv4ToIPAddress = fmap nodeHostIPv4AddressToIPAddress

nodeIPv6ToIPAddress :: NodeIPv6Address -> NodeIPAddress
nodeIPv6ToIPAddress = fmap nodeHostIPv6AddressToIPAddress

nodeDnsAddressToDomainAddress :: NodeDnsAddress -> DomainAddress
nodeDnsAddressToDomainAddress NodeAddress { naHostAddress = NodeHostDnsAddress dns, naPort }
  = DomainAddress (Text.encodeUtf8 dns) naPort

nodeAddressToSockAddr :: NodeIPAddress -> SockAddr
nodeAddressToSockAddr (NodeAddress addr port) =
  case unNodeHostIPAddress addr of
    IP.IPv4 ipv4 -> SockAddrInet  port   (IP.toHostAddress ipv4)
    IP.IPv6 ipv6 -> SockAddrInet6 port 0 (IP.toHostAddress6 ipv6) 0

nodeHostIPAddressToSockAddr :: NodeIPAddress -> SockAddr
nodeHostIPAddressToSockAddr NodeAddress { naHostAddress = NodeHostIPAddress ip, naPort } =
    case ip of
      IPv4 ipv4 -> SockAddrInet  (fromIntegral naPort)   (IP.toHostAddress ipv4)
      IPv6 ipv6 -> SockAddrInet6 (fromIntegral naPort) 0 (IP.toHostAddress6 ipv6) 0


newtype NodeHostIPv4Address
  = NodeHostIPv4Address { unNodeHostIPv4Address :: IPv4 }
  deriving newtype Show
  deriving (Eq, Ord)

instance FromJSON NodeHostIPv4Address where
  parseJSON (String ipStr) =
    case readMaybe $ Text.unpack ipStr of
      Just ip -> pure $ NodeHostIPv4Address ip
      Nothing -> panic $ "Parsing of IPv4 failed: " <> ipStr
  parseJSON invalid = panic $ "Parsing of IPv4 failed due to type mismatch. "
                            <> "Encountered: " <> Text.pack (show invalid) <> "\n"

instance ToJSON NodeHostIPv4Address where
  toJSON (NodeHostIPv4Address ip) = String (Text.pack $ show ip)


newtype NodeHostIPv6Address
  = NodeHostIPv6Address { unNodeHostIPv6Address :: IPv6 }
  deriving newtype Show
  deriving (Eq, Ord)

instance FromJSON NodeHostIPv6Address where
  parseJSON (String ipStr) =
    case readMaybe $ Text.unpack ipStr of
      Just ip -> pure $ NodeHostIPv6Address ip
      Nothing -> panic $ "Parsing of IPv6 failed: " <> ipStr
  parseJSON invalid = panic $ "Parsing of IPv6 failed due to type mismatch. "
                            <> "Encountered: " <> Text.pack (show invalid) <> "\n"
instance ToJSON NodeHostIPv6Address where
  toJSON (NodeHostIPv6Address ip) = String (Text.pack $ show ip)


newtype NodeHostIPAddress
  = NodeHostIPAddress { unNodeHostIPAddress :: IP }
  deriving newtype Show
  deriving (Eq, Ord)

instance FromJSON NodeHostIPAddress where
  parseJSON (String ipStr) =
    case readMaybe $ Text.unpack ipStr of
      Just ip -> pure $ NodeHostIPAddress ip
      Nothing -> panic $ "Parsing of IP failed: " <> ipStr
  parseJSON invalid = panic $ "Parsing of IP failed due to type mismatch. "
                            <> "Encountered: " <> Text.pack (show invalid) <> "\n"

instance ToJSON NodeHostIPAddress where
  toJSON (NodeHostIPAddress ip) = String (Text.pack $ show ip)


nodeHostIPv6AddressToIPAddress :: NodeHostIPv6Address -> NodeHostIPAddress
nodeHostIPv6AddressToIPAddress (NodeHostIPv6Address ip) = NodeHostIPAddress (IPv6 ip)

nodeHostIPv4AddressToIPAddress :: NodeHostIPv4Address -> NodeHostIPAddress
nodeHostIPv4AddressToIPAddress (NodeHostIPv4Address ip) = NodeHostIPAddress (IPv4 ip)


-- | Domain name.
--
newtype NodeHostDnsAddress
  = NodeHostDnsAddress { unNodeHostDnsAddress :: Text }
  deriving newtype Show
  deriving (Eq, Ord)

nodeHostDnsAddressToDomain :: NodeHostDnsAddress -> DNS.Domain
nodeHostDnsAddressToDomain = Text.encodeUtf8 . unNodeHostDnsAddress


class AdjustFilePaths a where
  adjustFilePaths :: (FilePath -> FilePath) -> a -> a


data ProtocolFilepaths =
     ProtocolFilepaths {
       byronCertFile   :: !(Maybe FilePath)
     , byronKeyFile    :: !(Maybe FilePath)
     , shelleyKESFile  :: !(Maybe FilePath)
     , shelleyVRFFile  :: !(Maybe FilePath)
     , shelleyCertFile :: !(Maybe FilePath)
     } deriving (Eq, Show)

newtype GenesisHash = GenesisHash (Crypto.Hash Crypto.Blake2b_256 ByteString)
  deriving newtype (Eq, Show, ToJSON, FromJSON)

data NodeProtocolConfiguration =
       NodeProtocolConfigurationByron   NodeByronProtocolConfiguration
     | NodeProtocolConfigurationShelley NodeShelleyProtocolConfiguration
     | NodeProtocolConfigurationCardano NodeByronProtocolConfiguration
                                        NodeShelleyProtocolConfiguration
                                        NodeHardForkProtocolConfiguration
  deriving (Eq, Show)

data NodeShelleyProtocolConfiguration =
     NodeShelleyProtocolConfiguration {
       npcShelleyGenesisFile     :: !GenesisFile
     , npcShelleyGenesisFileHash :: !(Maybe GenesisHash)

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
  deriving (Eq, Show)

data NodeByronProtocolConfiguration =
     NodeByronProtocolConfiguration {
       npcByronGenesisFile         :: !GenesisFile
     , npcByronGenesisFileHash     :: !(Maybe GenesisHash)
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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

newtype SocketPath = SocketPath
  { unSocketPath :: FilePath }
  deriving stock (Eq, Ord)
  deriving newtype (FromJSON, IsString, Show)

newtype TopologyFile = TopologyFile
  { unTopology :: FilePath }
  deriving newtype (Show, Eq)

instance AdjustFilePaths NodeProtocolConfiguration where

  adjustFilePaths f (NodeProtocolConfigurationByron pc) =
    NodeProtocolConfigurationByron (adjustFilePaths f pc)

  adjustFilePaths f (NodeProtocolConfigurationShelley pc) =
    NodeProtocolConfigurationShelley (adjustFilePaths f pc)

  adjustFilePaths f (NodeProtocolConfigurationCardano pcb pcs pch) =
    NodeProtocolConfigurationCardano (adjustFilePaths f pcb)
                                     (adjustFilePaths f pcs)
                                     pch

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


instance AdjustFilePaths (Last NodeProtocolConfiguration) where

  adjustFilePaths f (Last (Just (NodeProtocolConfigurationByron pc))) =
    Last . Just $ NodeProtocolConfigurationByron (adjustFilePaths f pc)

  adjustFilePaths f (Last (Just (NodeProtocolConfigurationShelley pc))) =
    Last . Just $ NodeProtocolConfigurationShelley (adjustFilePaths f pc)

  adjustFilePaths f (Last (Just (NodeProtocolConfigurationCardano pcb pcs pch))) =
    Last . Just $ NodeProtocolConfigurationCardano (adjustFilePaths f pcb)
                                                   (adjustFilePaths f pcs)
                                                   pch
  adjustFilePaths _ (Last Nothing) = Last Nothing

instance AdjustFilePaths (Last SocketPath) where
  adjustFilePaths f (Last (Just (SocketPath p))) = Last . Just $ SocketPath (f p)
  adjustFilePaths _ (Last Nothing) = Last Nothing

-- | A human readable name for the protocol
--
protocolName :: Protocol -> String
protocolName ByronProtocol   = "Byron"
protocolName ShelleyProtocol = "Shelley"
protocolName CardanoProtocol = "Byron; Shelley"
