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
import           Network.Socket

import           Ouroboros.Consensus.NodeId (NodeId(..))
import           Ouroboros.Consensus.Util.Condense (Condense (..))

-- | A data structure bundling together a node identifier and the path to
-- the topology file.
data TopologyInfo = TopologyInfo {
    node         :: NodeId
  , topologyFile :: FilePath
  }

-- | IPv4 or IPv6 address with port number
--
data NodeAddress = NodeAddress {
      naHostAddress :: !IP.IP
    , naPort        :: !PortNumber
    }
  deriving (Eq, Ord, Show)

nodeAddressToSockAddr
    :: NodeAddress
    -> SockAddr
nodeAddressToSockAddr NodeAddress {naHostAddress, naPort} = case naHostAddress of
    IP.IPv4 ipv4 -> SockAddrInet  naPort   (IP.toHostAddress  ipv4)
    IP.IPv6 ipv6 -> SockAddrInet6 naPort 0 (IP.toHostAddress6 ipv6) 0

instance Condense NodeAddress where
    condense NodeAddress {naHostAddress, naPort}
      = show naHostAddress ++ ":" ++ show naPort

instance FromJSON NodeAddress where
    parseJSON = withObject "NodeAddress" $ \v -> do
      NodeAddress
      <$> (read <$> v .: "addr")
      <*> ((fromIntegral :: Int -> PortNumber) <$> v .: "port")

nodeAddressInfo :: NodeAddress -> AddrInfo
nodeAddressInfo NodeAddress {naHostAddress, naPort}
    = AddrInfo {
        addrFlags      = addrFlags defaultHints
      , addrFamily     = case naHostAddress of
                          IP.IPv4{} -> AF_INET
                          IP.IPv6{} -> AF_INET6
      , addrSocketType = Stream
      , addrProtocol   = addrProtocol defaultHints
      , addrAddress    =
          case naHostAddress of
            IP.IPv4 ipv4 -> SockAddrInet  naPort   (IP.toHostAddress  ipv4)
            IP.IPv6 ipv6 -> SockAddrInet6 naPort 0 (IP.toHostAddress6 ipv6) 0
      , addrCanonName  = Nothing
    }

data NodeSetup = NodeSetup {
    nodeAddress :: NodeAddress
  , producers   :: [NodeAddress]
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
