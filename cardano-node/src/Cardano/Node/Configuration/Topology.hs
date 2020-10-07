{-# LANGUAGE OverloadedStrings #-}

module Cardano.Node.Configuration.Topology
  ( TopologyError(..)
  , NetworkTopology(..)
  , NodeHostAddress(..)
  , NodeSetup(..)
  , RemoteAddress(..)
  , nodeAddressToSockAddr
  , readTopologyFile
  , remoteAddressToNodeAddress
  )
where

import           Cardano.Prelude
import           Prelude (String)

import qualified Control.Exception as Exception
import           Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.IP as IP
import qualified Data.Text as Text
import           Network.Socket (PortNumber, SockAddr (..))

import           Cardano.Node.Configuration.POM (NodeConfiguration (..))
import           Cardano.Node.Types

import           Ouroboros.Consensus.Util.Condense (Condense (..))


newtype TopologyError
  = NodeIdNotFoundInToplogyFile FilePath
  deriving Show

nodeAddressToSockAddr :: NodeAddress -> SockAddr
nodeAddressToSockAddr (NodeAddress addr port) =
  case unNodeHostAddress addr of
    Just (IP.IPv4 ipv4) -> SockAddrInet port $ IP.toHostAddress ipv4
    Just (IP.IPv6 ipv6) -> SockAddrInet6 port 0 (IP.toHostAddress6 ipv6) 0
    Nothing             -> SockAddrInet port 0 -- Could also be any IPv6 addr

-- | Domain name with port number
--
data RemoteAddress = RemoteAddress
  { raAddress :: !String
  -- ^ Either a DNS address or IP address
  , raPort :: !PortNumber
  -- ^ Port number of the destination
  , raValency :: !Int
  -- ^ If a DNS address is given valency governs
  -- to how many resolved IP addresses
  -- should we maintain active (hot) connection;
  -- if an IP address is given valency is used as
  -- a Boolean value, @0@ means to ignore the address;
  } deriving (Eq, Ord, Show)


-- | Parse 'raAddress' field as an IP address; if it parses and the valency is
-- non zero return corresponding NodeAddress.
--
remoteAddressToNodeAddress :: RemoteAddress-> Maybe NodeAddress
remoteAddressToNodeAddress (RemoteAddress addrStr port val) =
  case readMaybe addrStr of
    Nothing -> Nothing
    Just addr -> if val /= 0
                 then Just $ NodeAddress (NodeHostAddress $ Just addr) port
                 else Nothing


instance Condense RemoteAddress where
  condense (RemoteAddress addr port val) =
    addr ++ ":" ++ show port ++ " (" ++ show val ++ ")"

instance FromJSON RemoteAddress where
  parseJSON = withObject "RemoteAddress" $ \v ->
    RemoteAddress
      <$> v .: "addr"
      <*> ((fromIntegral :: Int -> PortNumber) <$> v .: "port")
      <*> v .: "valency"

instance ToJSON RemoteAddress where
  toJSON ra =
    object
      [ "addr" .= raAddress ra
      , "port" .= (fromIntegral (raPort ra) :: Int)
      , "valency" .= raValency ra
      ]

data NodeSetup = NodeSetup
  { nodeId :: !Word64
  , nodeAddress :: !NodeAddress
  , producers :: ![RemoteAddress]
  } deriving (Eq, Show)

instance FromJSON NodeSetup where
  parseJSON = withObject "NodeSetup" $ \o ->
                NodeSetup
                  <$> o .: "nodeId"
                  <*> o .: "nodeAddress"
                  <*> o .: "producers"

instance ToJSON NodeSetup where
  toJSON ns =
    object
      [ "nodeId" .= nodeId ns
      , "nodeAddress" .= nodeAddress ns
      , "producers" .= producers ns
      ]

data NetworkTopology = MockNodeTopology ![NodeSetup]
                     | RealNodeTopology ![RemoteAddress]
  deriving (Eq, Show)

instance FromJSON NetworkTopology where
  parseJSON = withObject "NetworkTopology" $ \o -> asum
                [ MockNodeTopology <$> o .: "MockProducers"
                , RealNodeTopology <$> o .: "Producers"
                ]

instance ToJSON NetworkTopology where
  toJSON top =
    case top of
      MockNodeTopology nss -> object [ "MockProducers" .= toJSON nss ]
      RealNodeTopology ras -> object [ "Producers" .= toJSON ras ]

-- | Read the `NetworkTopology` configuration from the specified file.
-- While running a real protocol, this gives your node its own address and
-- other remote peers it will attempt to connect to.
readTopologyFile :: NodeConfiguration -> IO (Either Text NetworkTopology)
readTopologyFile nc = do
  eBs <- Exception.try $ BS.readFile (unTopology $ ncTopologyFile nc)

  case eBs of
    Left e -> return . Left $ handler e
    Right bs -> return . first handlerJSON . eitherDecode $ LBS.fromStrict bs

 where
  handler :: IOException -> Text
  handler e = Text.pack $ "Cardano.Node.Configuration.Topology.readTopologyFile: "
                        ++ displayException e
  handlerJSON :: String -> Text
  handlerJSON err = "Is your topology file formatted correctly? \
                    \The port and valency fields should be numerical. " <> Text.pack err
