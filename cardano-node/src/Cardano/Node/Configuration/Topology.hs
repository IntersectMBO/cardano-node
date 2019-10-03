{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Node.Configuration.Topology
  ( TopologyError(..)
  , TopologyInfo(..)
  , NetworkTopology(..)
  , NodeAddress(..)
  , NodeSetup(..)
  , RemoteAddress(..)
  , createNodeAddress
  , nodeAddressInfo
  , nodeAddressToSockAddr
  , readTopologyFile
  , remoteAddressToNodeAddress
  )
where

import           Cardano.Prelude hiding (toS)
import           Prelude (String, read)

import           Control.Exception (IOException)
import qualified Control.Exception as Exception
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString as BS
import qualified Data.IP as IP
import           Data.String.Conv (toS)
import           Text.Read (readMaybe)
import           Network.Socket

import           Ouroboros.Consensus.NodeId (NodeId(..))
import           Ouroboros.Consensus.Util.Condense (Condense (..))


newtype TopologyError = NodeIdNotFoundInToplogyFile FilePath deriving Show

-- | A data structure bundling together a node identifier and the path to
-- the topology file.
data TopologyInfo = TopologyInfo
  { node :: NodeId
  , topologyFile :: FilePath
  } deriving Show

-- | IPv4 address with a port number.
data NodeAddress = NodeAddress
  { naHostAddress :: !(Maybe IP.IP)
  , naPort :: !PortNumber
  } deriving (Eq, Ord, Show)

instance Condense NodeAddress where
  condense (NodeAddress addr port) = show addr ++ ":" ++ show port

instance FromJSON NodeAddress where
  parseJSON = withObject "NodeAddress" $ \v -> do
    NodeAddress
      <$> (Just <$> read <$> v .: "addr")
      <*> ((fromIntegral :: Int -> PortNumber) <$> v .: "port")

nodeAddressToSockAddr :: NodeAddress -> SockAddr
nodeAddressToSockAddr (NodeAddress addr port) =
  case addr of
    Just (IP.IPv4 ipv4) -> SockAddrInet port $ IP.toHostAddress ipv4
    Just (IP.IPv6 ipv6) -> SockAddrInet6 port 0 (IP.toHostAddress6 ipv6) 0
    Nothing             -> SockAddrInet port 0 -- Could also be any IPv6 addr

nodeAddressInfo :: NodeAddress -> IO [AddrInfo]
nodeAddressInfo (NodeAddress hostAddr port) = do
  let hints = defaultHints {
                addrFlags = [AI_PASSIVE, AI_ADDRCONFIG]
              , addrSocketType = Stream
              }
  getAddrInfo (Just hints) (fmap show hostAddr) (Just $ show port)

-- | Domain name with port number
--
data RemoteAddress = RemoteAddress
  { raAddress :: !String
  -- ^ either a dns address or ip address
  , raPort :: !PortNumber
  -- ^ port number of the destination
  , raValency :: !Int
  -- ^ if a dns address is given valency governs
  -- to how many resolved ip addresses
  -- should we maintain acctive (hot) connection;
  -- if an ip address is given valency is used as
  -- a boolean value, @0@ means to ignore the address;
  } deriving (Eq, Ord, Show)


-- | Parse 'raAddress' field as an IP address; if it parses and the valency is
-- non zero return corresponding NodeAddress.
--
remoteAddressToNodeAddress:: RemoteAddress-> Maybe NodeAddress
remoteAddressToNodeAddress (RemoteAddress addrStr port val) =
  case readMaybe addrStr of
    Nothing -> Nothing
    Just addr -> if val /= 0
                 then Just $ NodeAddress (Just addr) port
                 else Nothing


instance Condense RemoteAddress where
  condense (RemoteAddress addr port val) =
    addr ++ ":" ++ show port ++ " (" ++ show val ++ ")"

instance FromJSON RemoteAddress where
  parseJSON = withObject "RemoteAddress" $ \v ->
    RemoteAddress
      <$> v .: "addr"
      <*> ((fromIntegral :: Int -> PortNumber) <$> v .: "port")
      <*> (v .: "valency")

data NodeSetup = NodeSetup
  { nodeId :: !Int
  , nodeAddress :: !NodeAddress
  , producers :: ![RemoteAddress]
  } deriving Show

instance FromJSON NodeId where
  parseJSON v = CoreId <$> parseJSON v

deriveFromJSON defaultOptions ''NodeSetup

data NetworkTopology = NetworkTopology [NodeSetup]
  deriving Show

deriveFromJSON defaultOptions ''NetworkTopology

-- | Creates a 'NodeAddress' if it exists in a given 'NetworkTopology'.
createNodeAddress
  :: NodeId
  -> NetworkTopology
  -> FilePath
  -> Either TopologyError NodeAddress
createNodeAddress _nodeId (NetworkTopology nodeSetups) fp =
  case maybeNode of
    Nothing -> Left $ NodeIdNotFoundInToplogyFile fp
    Just (NodeSetup _ anAddress _) -> Right anAddress
 where
  idInt :: Int
  idInt = case _nodeId of
            CoreId i -> i
            RelayId i -> i
  -- Search 'NetworkTopology' for a given 'NodeId'
  maybeNode :: Maybe NodeSetup
  maybeNode = find (\(NodeSetup nId _ _) -> idInt == nId) nodeSetups

readTopologyFile :: FilePath -> IO (Either String NetworkTopology)
readTopologyFile topo = do
  eBs <- Exception.try $ BS.readFile topo
  case eBs of
    Left e -> pure . Left $ handler e
    Right bs -> pure . eitherDecode $ toS bs
 where
  handler :: IOException -> String
  handler e = "Cardano.Node.Configuration.Topology.readTopologyFile: "
              ++ displayException e
