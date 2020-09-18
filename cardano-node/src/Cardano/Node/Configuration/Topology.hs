{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Node.Configuration.Topology
  ( TopologyError(..)
  , NetworkTopology(..)
  , NodeHostIPAddress(..)
  , NodeHostIPv4Address(..)
  , NodeHostIPv6Address(..)
  , NodeSetup(..)
  , RemoteAddress(..)
  , PeerAdvertise(..)
  , UseLedger(..)
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
import qualified Data.Text as Text

import           Cardano.Node.Configuration.POM (NodeConfiguration (..))
import           Cardano.Slotting.Slot (SlotNo (..))
import           Cardano.Node.Types

import           Ouroboros.Network.NodeToNode (PeerAdvertise (..))
import           Ouroboros.Network.PeerSelection.LedgerPeers (UseLedgerAfter (..))
import           Ouroboros.Consensus.Util.Condense (Condense (..))


newtype TopologyError
  = NodeIdNotFoundInToplogyFile FilePath
  deriving Show

-- | Domain name with port number
--
data RemoteAddress = RemoteAddress
  { raAddress   :: !Text
  -- ^ Either a dns address or an ip address.
  , raPort      :: !PortNumber
  -- ^ Port number of the destination.
  , raAdvertise :: !PeerAdvertise
  -- ^ Advertise the peer through gossip protocol.
  -- ^ If a DNS address is given valency governs
  } deriving (Eq, Show)


-- | Parse 'raAddress' field as an IP address; if it parses and the valency is
-- non zero return corresponding NodeAddress.
--
remoteAddressToNodeAddress
  :: RemoteAddress
  -> Either (NodeIPAddress,  PeerAdvertise)
            (NodeDnsAddress, PeerAdvertise)
remoteAddressToNodeAddress RemoteAddress { raAddress, raPort, raAdvertise } =
    case readMaybe (Text.unpack raAddress) of
      Nothing   -> Right ( NodeAddress (NodeHostDnsAddress raAddress) raPort
                         , raAdvertise )
      Just addr -> Left  ( NodeAddress (NodeHostIPAddress addr) raPort
                         , raAdvertise )


instance Condense RemoteAddress where
  condense (RemoteAddress addr port val) =
    Text.unpack addr ++ ":" ++ show port ++ " (" ++ show val ++ ")"

instance FromJSON RemoteAddress where
  parseJSON = withObject "RemoteAddress" $ \v ->
    RemoteAddress
      <$> v .: "addr"
      <*> ((fromIntegral :: Int -> PortNumber) <$> v .: "port")
      <*> (bool DoNotAdvertisePeer DoAdvertisePeer <$> v .: "advertise")

instance ToJSON RemoteAddress where
  toJSON ra =
    object
      [ "addr" .= raAddress ra
      , "port" .= (fromIntegral (raPort ra) :: Int)
      , "advertise" .= case raAdvertise ra of
                          DoNotAdvertisePeer -> False
                          DoAdvertisePeer    -> True
      ]

newtype UseLedger = UseLedger UseLedgerAfter deriving (Eq, Show)

instance FromJSON UseLedger where
  parseJSON (Data.Aeson.Number n) =
    if n >= 0 then return $ UseLedger $ UseLedgerAfter $ SlotNo $ floor n
              else return $ UseLedger $ DontUseLedger
  parseJSON _ = mzero

instance ToJSON UseLedger where
  toJSON (UseLedger (UseLedgerAfter (SlotNo n))) = Number $ fromIntegral n
  toJSON (UseLedger DontUseLedger)               = Number (-1)

data NodeSetup = NodeSetup
  { nodeId :: !Word64
  , nodeIPv4Address :: !(Maybe NodeIPv4Address)
  , nodeIPv6Address :: !(Maybe NodeIPv6Address)
  , producers :: ![RemoteAddress]
  , useLedger :: !UseLedger
  } deriving (Eq, Show)

instance FromJSON NodeSetup where
  parseJSON = withObject "NodeSetup" $ \o ->
                NodeSetup
                  <$> o .: "nodeId"
                  <*> o .: "nodeIPv4Address"
                  <*> o .: "nodeIPv6Address"
                  <*> o .: "producers"
                  <*> (o .:? "useLedgerAfterSlot" .!= (UseLedger DontUseLedger))

instance ToJSON NodeSetup where
  toJSON ns =
    object
      [ "nodeId" .= nodeId ns
      , "nodeIPv4Address" .= nodeIPv4Address ns
      , "nodeIPv6Address" .= nodeIPv6Address ns
      , "producers" .= producers ns
      , "useLedgerAfterSlot" .= useLedger ns
      ]

data NetworkTopology = MockNodeTopology ![NodeSetup]
                     | RealNodeTopology ![RemoteAddress] !UseLedger
  deriving (Eq, Show)

instance FromJSON NetworkTopology where
  parseJSON = withObject "NetworkTopology" $ \o -> asum
                [ MockNodeTopology <$> o .: "MockProducers"
                , RealNodeTopology <$> o .: "Producers"
                                   <*> (o .:? "useLedgerAfterSlot" .!= (UseLedger DontUseLedger))
                ]

instance ToJSON NetworkTopology where
  toJSON top =
    case top of
      MockNodeTopology nss -> object [ "MockProducers" .= toJSON nss ]
      RealNodeTopology ras ul -> object [ "Producers" .= toJSON ras
                                        ,  "useLedgerAfterSlot" .= toJSON ul
                                        ]

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
