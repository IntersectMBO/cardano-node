{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
module Cardano.Node.NodeAddress
  ( -- * Node addresses
    NodeAddress'(..)
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
  )
where

import Cardano.Prelude
import Prelude (fail)

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..),
                   (.:), (.=), withObject, object)
import Data.IP (IP (..), IPv4, IPv6)
import Data.IP qualified as IP
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Network.DNS qualified as DNS (Domain)
import Network.Socket (PortNumber, SockAddr (..))

import Ouroboros.Network.PeerSelection.RootPeersDNS (DomainAccessPoint (..))


-- | IPv4 or IPv6 address with a port number.
data NodeAddress' addr = NodeAddress
  { naHostAddress :: !addr
  , naPort        :: !PortNumber
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

nodeDnsAddressToDomainAddress :: NodeDnsAddress -> DomainAccessPoint
nodeDnsAddressToDomainAddress NodeAddress { naHostAddress = NodeHostDnsAddress dns, naPort }
  = DomainAccessPoint (Text.encodeUtf8 dns) naPort

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
      Nothing -> fail $ "Parsing of IPv4 failed: " <> Text.unpack ipStr
  parseJSON invalid = fail $ "Parsing of IPv4 failed due to type mismatch. "
                           <> "Encountered: " <> show invalid <> "\n"

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
      Nothing -> fail $ "Parsing of IPv6 failed: " <> Text.unpack ipStr
  parseJSON invalid = fail $ "Parsing of IPv6 failed due to type mismatch. "
                          <> "Encountered: " <> show invalid <> "\n"
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
      Nothing -> fail $ "Parsing of IP failed: " <> Text.unpack ipStr
  parseJSON invalid = fail $ "Parsing of IP failed due to type mismatch. "
                          <> "Encountered: " <> show invalid <> "\n"

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


-- | Socket path
--
newtype SocketPath = SocketPath
  { unSocketPath :: FilePath }
  deriving stock (Eq, Ord)
  deriving newtype (FromJSON, IsString, Show)
