{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Node.Configuration.TopologyP2P
  ( TopologyError(..)
  , NetworkTopology(..)
  , PublicRootPeers(..)
  , LocalRootPeersGroups(..)
  , LocalRootPeers(..)
  , RootAddress(..)
  , NodeHostIPAddress(..)
  , NodeHostIPv4Address(..)
  , NodeHostIPv6Address(..)
  , NodeSetup(..)
  , PeerAdvertise(..)
  , UseLedger(..)
  , nodeAddressToSockAddr
  , readTopologyFile
  , readTopologyFileOrError
  , rootAddressToRelayAddress
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
import           Cardano.Node.Configuration.Topology (TopologyError (..))

import           Ouroboros.Network.NodeToNode (PeerAdvertise (..))
import           Ouroboros.Network.PeerSelection.LedgerPeers (UseLedgerAfter (..), RelayAddress (..))

newtype UseLedger = UseLedger UseLedgerAfter deriving (Eq, Show)

instance FromJSON UseLedger where
  parseJSON (Data.Aeson.Number n) =
    if n >= 0 then return $ UseLedger $ UseLedgerAfter $ SlotNo $ floor n
              else return $ UseLedger   DontUseLedger
  parseJSON _ = mzero

instance ToJSON UseLedger where
  toJSON (UseLedger (UseLedgerAfter (SlotNo n))) = Number $ fromIntegral n
  toJSON (UseLedger DontUseLedger)               = Number (-1)

data NodeSetup = NodeSetup
  { nodeId :: !Word64
  , nodeIPv4Address :: !(Maybe NodeIPv4Address)
  , nodeIPv6Address :: !(Maybe NodeIPv6Address)
  , producers :: ![RootAddress]
  , useLedger :: !UseLedger
  } deriving (Eq, Show)

instance FromJSON NodeSetup where
  parseJSON = withObject "NodeSetup" $ \o ->
                NodeSetup
                  <$> o .:  "nodeId"
                  <*> o .:  "nodeIPv4Address"
                  <*> o .:  "nodeIPv6Address"
                  <*> o .:  "producers"
                  <*> o .:? "useLedgerAfterSlot" .!= UseLedger DontUseLedger

instance ToJSON NodeSetup where
  toJSON ns =
    object
      [ "nodeId"             .= nodeId ns
      , "nodeIPv4Address"    .= nodeIPv4Address ns
      , "nodeIPv6Address"    .= nodeIPv6Address ns
      , "producers"          .= producers ns
      , "useLedgerAfterSlot" .= useLedger ns
      ]

data RootAddress = RootAddress
  { addrs :: [RelayAddress]
  , advertise :: PeerAdvertise
  } deriving (Eq, Show)

instance FromJSON RootAddress where
  parseJSON = withObject "RootAddress" $ \o ->
                RootAddress
                  <$> o .:  "addrs"
                  <*> o .:? "advertise" .!= DoNotAdvertisePeer

instance ToJSON RootAddress where
  toJSON ra =
    object
      [ "addrs" .= addrs ra
      , "advertise" .= advertise ra
      ]

-- | Transforms a 'RootAddress' into a pair of 'RelayAddress' and its
-- corresponding 'PeerAdvertise' value.
--
rootAddressToRelayAddress
  :: RootAddress
  -> [(RelayAddress, PeerAdvertise)]
rootAddressToRelayAddress RootAddress { addrs, advertise } =
    [ (ra, advertise) | ra <- addrs ]

data LocalRootPeers = LocalRootPeers
  { localRoots :: RootAddress
  , valency :: Int
  } deriving (Eq, Show)

instance FromJSON LocalRootPeers where
  parseJSON = withObject "LocalRootPeers" $ \o ->
                LocalRootPeers
                  <$> o .: "localRoots"
                  <*> o .: "valency"

instance ToJSON LocalRootPeers where
  toJSON lrpg =
    object
      [ "localRoots" .= localRoots lrpg
      , "valency" .= valency lrpg
      ]

newtype LocalRootPeersGroups = LocalRootPeersGroups
  { groups :: [LocalRootPeers]
  } deriving (Eq, Show)

instance FromJSON LocalRootPeersGroups where
  parseJSON = withObject "LocalRootPeersGroups" $ \o ->
                LocalRootPeersGroups
                  <$> o .: "groups"

instance ToJSON LocalRootPeersGroups where
  toJSON lrpg =
    object
      [ "groups" .= groups lrpg
      ]

newtype PublicRootPeers = PublicRootPeers
  { publicRoots :: RootAddress
  } deriving (Eq, Show)

instance FromJSON PublicRootPeers where
  parseJSON = withObject "PublicRootPeers" $ \o ->
                PublicRootPeers
                  <$> o .: "publicRoots"

instance ToJSON PublicRootPeers where
  toJSON prp =
    object
      [ "publicRoots" .= publicRoots prp
      ]

data NetworkTopology = RealNodeTopology !LocalRootPeersGroups ![PublicRootPeers] !UseLedger
  deriving (Eq, Show)

instance FromJSON NetworkTopology where
  parseJSON = withObject "NetworkTopology" $ \o ->
                RealNodeTopology <$> (o .: "LocalRoots"                                     )
                                 <*> (o .: "PublicRoots"                                    )
                                 <*> (o .:? "useLedgerAfterSlot" .!= UseLedger DontUseLedger)

instance ToJSON NetworkTopology where
  toJSON top =
    case top of
      RealNodeTopology lrpg prp ul -> object [ "LocalRoots"         .= lrpg
                                             , "PublicRoots"        .= prp
                                             , "useLedgerAfterSlot" .= ul
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
                    \Expecting P2P Topology file format. \
                    \The port and valency fields should be numerical. \
                    \If you specified the correct topology file \
                    \make sure that you correctly setup EnableP2P \
                    \configuration flag. " <> Text.pack err

readTopologyFileOrError :: NodeConfiguration -> IO NetworkTopology
readTopologyFileOrError nc =
      readTopologyFile nc
  >>= either (\err -> panic $ "Cardano.Node.Run.handleSimpleNodeP2P.readTopologyFile: "
                           <> err)
             pure
