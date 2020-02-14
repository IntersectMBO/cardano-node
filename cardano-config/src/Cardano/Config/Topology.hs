{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Config.Topology
  ( TopologyError(..)
  , NetworkTopology(..)
  , NodeHostAddress(..)
  , NodeSetup(..)
  , RemoteAddress(..)
  , nodeAddressInfo
  , nodeAddressToSockAddr
  , readTopologyFile
  , remoteAddressToNodeAddress
  )
where

import           Cardano.Prelude hiding (toS)
import           Prelude (String)

import           Control.Exception (IOException)
import qualified Control.Exception as Exception
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString as BS
import qualified Data.IP as IP
import           Data.String.Conv (toS)
import qualified Data.Text as T
import           Text.Read (readMaybe)
import           Network.Socket

import           Cardano.Config.Types

import           Ouroboros.Consensus.Util.Condense (Condense (..))


newtype TopologyError = NodeIdNotFoundInToplogyFile FilePath deriving Show

nodeAddressToSockAddr :: NodeAddress -> SockAddr
nodeAddressToSockAddr (NodeAddress addr port) =
  case getAddress addr of
    Just (IP.IPv4 ipv4) -> SockAddrInet port $ IP.toHostAddress ipv4
    Just (IP.IPv6 ipv6) -> SockAddrInet6 port 0 (IP.toHostAddress6 ipv6) 0
    Nothing             -> SockAddrInet port 0 -- Could also be any IPv6 addr

nodeAddressInfo :: NodeProtocolMode -> IO [AddrInfo]
nodeAddressInfo npm = do
  (NodeAddress hostAddr port) <-
    case npm of
      (RealProtocolMode (NodeCLI _ _ nodeAddr' _ _)) -> pure nodeAddr'
      (MockProtocolMode (NodeMockCLI _ _ nodeAddr' _ _)) -> pure nodeAddr'
  let hints = defaultHints {
                addrFlags = [AI_PASSIVE, AI_ADDRCONFIG]
              , addrSocketType = Stream
              }
  getAddrInfo (Just hints) (fmap show $ getAddress hostAddr) (Just $ show port)

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
      <*> (v .: "valency")

data NodeSetup = NodeSetup
  { nodeId :: !Word64
  , nodeAddress :: !NodeAddress
  , producers :: ![RemoteAddress]
  } deriving Show

deriveFromJSON defaultOptions ''NodeSetup

data NetworkTopology = MockNodeTopology ![NodeSetup]
                     | RealNodeTopology ![RemoteAddress]
  deriving Show

instance FromJSON NetworkTopology where
  parseJSON = withObject "NetworkTopology" $ \o -> asum
                [ MockNodeTopology <$> (genericParseJSON defaultOptions Null)
                , RealNodeTopology <$> o .: "Producers"
                ]

-- | Read the `NetworkTopology` configuration from the specified file.
-- While running a real protocol, this gives your node its own address and
-- other remote peers it will attempt to connect to.
readTopologyFile :: NodeProtocolMode -> IO (Either Text NetworkTopology)
readTopologyFile npm = do
  topo  <- case npm of
             (RealProtocolMode (NodeCLI mscFp' _ _ _ _)) -> pure . unTopology $ topFile mscFp'
             (MockProtocolMode (NodeMockCLI mscFp' _ _ _ _)) -> pure . unTopology $ topFile mscFp'

  eBs <- Exception.try $ BS.readFile topo

  case eBs of
    Left e -> pure . Left $ handler e
    Right bs -> pure . first T.pack . eitherDecode $ toS bs

 where
  handler :: IOException -> Text
  handler e = T.pack $ "Cardano.Node.Configuration.Topology.readTopologyFile: "
                     ++ displayException e
