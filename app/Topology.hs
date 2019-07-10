{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Topology where

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString as B
import           Data.List (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
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
    nodeId      :: NodeId  -- TODO: do we need this?
  , nodeAddress :: NodeAddress
  , producers   :: [NodeAddress]
  }
  deriving Show

instance FromJSON NodeId where
    parseJSON v = CoreId <$> parseJSON v

deriveFromJSON defaultOptions ''NodeSetup

data NetworkTopology = NetworkTopology [NodeSetup]
  deriving Show

deriveFromJSON defaultOptions ''NetworkTopology

type NetworkMap = Map NodeId NodeSetup

toNetworkMap :: NetworkTopology -> NetworkMap
toNetworkMap (NetworkTopology xs) =
    foldl' (\acc ns -> M.insert (nodeId ns) ns acc) mempty xs

readTopologyFile :: FilePath -> IO (Either String NetworkTopology)
readTopologyFile topo = do
    eitherDecode . toS <$> B.readFile topo
