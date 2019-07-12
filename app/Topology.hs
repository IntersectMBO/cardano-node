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

-- | IPv4 address with port number
--
-- TODO: this type should be extended to take into account IPv6 addresses.
--
data NodeAddress = NodeAddress HostName ServiceName
  deriving (Eq, Ord, Show)

nodeAddressToSockAddr :: NodeAddress -> SockAddr
nodeAddressToSockAddr (NodeAddress addr port) = SockAddrInet (read port) (IP.toHostAddress (read addr))

instance Condense NodeAddress where
    condense (NodeAddress addr port) = addr ++ ":" ++ show port

instance FromJSON NodeAddress where
    parseJSON = withObject "NodeAddress" $ \v -> NodeAddress
      <$> v .: "addr"
      <*> v .: "port"

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
