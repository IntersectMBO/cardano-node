{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Topology where

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString as B
import qualified Data.IP as IP
import           Data.String.Conv (toS)
import           Text.Read (readMaybe)
import           Network.Socket

import           Ouroboros.Consensus.NodeId (NodeId(..))
import           Ouroboros.Consensus.Util.Condense (Condense (..))

-- | A data structure bundling together a node identifier and the path to
-- the topology file.
data TopologyInfo = TopologyInfo {
    node         :: NodeId
  , topologyFile :: FilePath
  }

-- | IPv4 address with a port number.
--
data NodeAddress = NodeAddress {
      naHostAddress :: !IP.IP
    , naPort        :: !PortNumber
    }
  deriving (Eq, Ord, Show)

instance Condense NodeAddress where
    condense NodeAddress {naHostAddress, naPort}
      = show naHostAddress ++ ":" ++ show naPort

instance FromJSON NodeAddress where
    parseJSON = withObject "NodeAddress" $ \v -> do
      NodeAddress
      <$> (read <$> v .: "addr")
      <*> ((fromIntegral :: Int -> PortNumber) <$> v .: "port")

nodeAddressToSockAddr
    :: NodeAddress
    -> SockAddr
nodeAddressToSockAddr NodeAddress {naHostAddress, naPort} = case naHostAddress of
    IP.IPv4 ipv4 -> SockAddrInet  naPort   (IP.toHostAddress  ipv4)
    IP.IPv6 ipv6 -> SockAddrInet6 naPort 0 (IP.toHostAddress6 ipv6) 0

nodeAddressInfo :: NodeAddress -> AddrInfo
nodeAddressInfo na@NodeAddress {naHostAddress}
    = AddrInfo {
        addrFlags      = addrFlags defaultHints
      , addrFamily     = case naHostAddress of
                          IP.IPv4{} -> AF_INET
                          IP.IPv6{} -> AF_INET6
      , addrSocketType = Stream
      , addrProtocol   = addrProtocol defaultHints
      , addrAddress    = nodeAddressToSockAddr na
      , addrCanonName  = Nothing
    }

-- | Domain name with port number
--
data RemoteAddress = RemoteAddress {
    raAddress :: !String
  -- ^ either a dns address or ip address
  , raPort    :: !PortNumber
  -- ^ port number of the destination
  , raValency :: !Int
  -- ^ if a dns address is given valency governs to how many resolved ip addresses
  -- should we maintain acctive (hot) connection;
  -- if an ip address is given valency is used as a boolean value, @0@ means to
  -- ignore the address;
  }
  deriving (Eq, Ord, Show)


-- | Parse 'raAddress' field as an IP address; if it parses and the valency is
-- non zero return corresponding NodeAddress.
--
remoteAddressToNodeAddress
  :: RemoteAddress
  -> Maybe NodeAddress
remoteAddressToNodeAddress RemoteAddress {raAddress, raPort, raValency} =
    case readMaybe raAddress of
      Nothing -> Nothing
      Just naHostAddress | raValency /= 0 -> Just (NodeAddress {
                                                      naHostAddress
                                                    , naPort = raPort
                                                    })
                         | otherwise      -> Nothing


instance Condense RemoteAddress where
    condense RemoteAddress {raAddress, raPort, raValency} = raAddress ++ ":" ++ show raPort ++ " (" ++ show raValency ++ ")"

instance FromJSON RemoteAddress where
    parseJSON = withObject "RemoteAddress" $ \v -> RemoteAddress
      <$> v .: "addr"
      <*> ((fromIntegral :: Int -> PortNumber) <$> v .: "port")
      <*> (v .: "valency")

data NodeSetup = NodeSetup {
    nodeAddress :: !NodeAddress
  , producers   :: ![RemoteAddress]
  }
  deriving Show

instance FromJSON NodeId where
    parseJSON v = CoreId <$> parseJSON v

deriveFromJSON defaultOptions ''NodeSetup

data NetworkTopology = NetworkTopology [NodeSetup]
  deriving Show

deriveFromJSON defaultOptions ''NetworkTopology

readTopologyFile :: FilePath -> IO (Either String NetworkTopology)
readTopologyFile topo = do
    eitherDecode . toS <$> B.readFile topo

