{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- To avoid having a "cardano-node" dependency, topology types were almost
-- identically copied from `Cardano.Node.Configuration.Topology` and
-- `Cardano.Node.Configuration.TopologyP2P` (and its dependencies).
-- One difference is that we replaced `HotValency` and `WarmValency` with a
-- single `Valency` type.
--------------------------------------------------------------------------------

module Cardano.Benchmarking.Topology.Projection (
  projection
, projectionP2P
, projectionExplorer
, projectionChainDB
) where

--------------------------------------------------------------------------------

import           Prelude
import           Data.Bool (bool)
import           Data.Word (Word64)
import           GHC.Generics
import           Text.Read (readMaybe)
-- Package: aeson.
import qualified Data.Aeson as Aeson
-- Package: iproute.
import qualified Data.IP as IP
-- Package: network.
import qualified Network.DNS as DNS
import qualified Network.Socket as Socket
-- Package: text.
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
-- Package: self.
import qualified Cardano.Benchmarking.Topology.Types as Types

--------------------------------------------------------------------------------

projection :: Types.Topology -> Int -> Int -> NetworkTopology
projection topology i basePort = RealNodeTopology $
  map
    (\name ->
      let node = getCoreNodeByName topology name
      in RemoteAddress {
           raAddress = "127.0.0.1"
         , raPort = toEnum (basePort + Types.nodeId node)
         , raValency = 1
         }
    )
    (getCoreNodeProducersById topology i)

projectionP2P :: Types.Topology -> Int -> Int -> NetworkTopologyP2P
projectionP2P topology i basePort = RealNodeTopologyP2P
  {
    ntLocalRootPeersGroups = LocalRootPeersGroups {
      groups =
        map
          (\name ->
            let node = getCoreNodeByName topology name
                v = length $ Types.producers node
            in LocalRootPeersGroup {
                 localRoots = RootConfig {
                   rootAccessPoints = [
                     RelayAccessAddress
                      "127.0.0.1"
                      (toEnum $ basePort + Types.nodeId node)
                   ]
                 , rootAdvertise = DoNotAdvertisePeer
                 }
               , trustable = IsTrustable
               , valency = Valency v
               }
          )
          (getCoreNodeProducersById topology i)
    }
  , ntPublicRootPeers = []
  , ntUseLedgerPeers = DontUseLedgerPeers
  , ntUseBootstrapPeers = DontUseBootstrapPeers
  }

projectionExplorer :: Int -> Int -> NetworkTopology
projectionExplorer srcIndices basePort = RealNodeTopology $
  map
    (\i ->
      RemoteAddress {
         raAddress = "127.0.0.1"
       , raPort = toEnum (basePort + i)
       , raValency = 1
       }
    )
    [0..(srcIndices - 1)]

-- ChainDB servers are just "{Producers:[]}".
projectionChainDB :: Types.Topology -> NetworkTopology
projectionChainDB _ = RealNodeTopology []

--------------------------------------------------------------------------------

getCoreNodeByName :: Types.Topology -> String -> Types.Node
getCoreNodeByName topology name =
  (!! 0) $
    filter
      ((== name) . Types.name)
      (Types.coreNodes topology)

getCoreNodeProducersById :: Types.Topology -> Int -> [String]
getCoreNodeProducersById topology i = Types.producers $
  (!! 0) $
    filter
      ((== i) . Types.nodeId)
      (Types.coreNodes topology)

--------------------------------------------------------------------------------
-- Projection for a non-P2P topology. ------------------------------------------
--------------------------------------------------------------------------------

{-- Example output:
{
  "Producers": [
    {
      "addr": "127.0.0.1",
      "port": 30001,
      "valency": 1
    }
  ]
}
--}

-----------------------------------------
-- Cardano.Node.Configuration.Topology --
-----------------------------------------

-- https://github.com/IntersectMBO/cardano-node/blob/52b708f37cd3dc92a188717deae2a6a60117f696/cardano-node/src/Cardano/Node/Configuration/Topology.hs#L115

-- `NetworkTopology` without the `MockNodeTopology` constructor.
data NetworkTopology = RealNodeTopology ![RemoteAddress]
                     | MockNodeTopology
  deriving (Eq, Show)

instance Aeson.FromJSON NetworkTopology where
  parseJSON = Aeson.withObject "NetworkTopology" $ \o -> RealNodeTopology
    <$> o Aeson..: "Producers"

instance Aeson.ToJSON NetworkTopology where
  toJSON (RealNodeTopology ras) =
    Aeson.object [ "Producers" Aeson..= Aeson.toJSON ras ]
  toJSON _ = error "Unsupported" -- `MockNodeTopology` constructor.

-- https://github.com/IntersectMBO/cardano-node/blob/52b708f37cd3dc92a188717deae2a6a60117f696/cardano-node/src/Cardano/Node/Configuration/Topology.hs#L42

-- | Domain name with port number
--
data RemoteAddress = RemoteAddress
  { raAddress :: !Text.Text
  -- ^ Either a dns address or an ip address.
  , raPort    :: !Socket.PortNumber
  -- ^ Port number of the destination.
  , raValency :: !Int
  -- ^ If a DNS address is given valency governs
  -- to how many resolved IP addresses
  -- should we maintain active (hot) connection;
  -- if an IP address is given valency is used as
  -- a Boolean value, @0@ means to ignore the address;
  } deriving (Eq, Ord, Show)

instance Aeson.FromJSON RemoteAddress where
  parseJSON = Aeson.withObject "RemoteAddress" $ \v -> RemoteAddress
    <$> v Aeson..: "addr"
    <*> ((fromIntegral :: Int -> Socket.PortNumber) <$> v Aeson..: "port")
    <*> v Aeson..: "valency"

instance Aeson.ToJSON RemoteAddress where
  toJSON ra = Aeson.object
    [ "addr"    Aeson..= raAddress ra
    , "port"    Aeson..= (fromIntegral (raPort ra) :: Int)
    , "valency" Aeson..= raValency ra
    ]

--------------------------------------------------------------------------------
-- Projection for a P2P topology. ----------------------------------------------
--------------------------------------------------------------------------------

{-- Example:
{
  "localRoots": [
    {
      "accessPoints": [
        {
          "address": "127.0.0.1",
          "port": 3001
        }
      ],
      "advertise": false,
      "valency": 6
    }
  ]
  "publicRoots": [],
  "useLedgerAfterSlot": -1
}
--}

--------------------------------------------
-- Cardano.Node.Configuration.TopologyP2P --
--------------------------------------------

-- https://github.com/IntersectMBO/cardano-node/blob/52b708f37cd3dc92a188717deae2a6a60117f696/cardano-node/src/Cardano/Node/Configuration/TopologyP2P.hs#L184

-- Renamed `NetworkTopology`.
data NetworkTopologyP2P = RealNodeTopologyP2P
  { ntLocalRootPeersGroups :: !LocalRootPeersGroups
  , ntPublicRootPeers      :: ![PublicRootPeers]
  , ntUseLedgerPeers       :: !UseLedgerPeers
  , ntUseBootstrapPeers    :: !UseBootstrapPeers
{- TODO:
  , ntPeerSnapshotPath     :: !(Maybe PeerSnapshotFile)
-}
  }
  deriving (Eq, Show)

instance Aeson.FromJSON NetworkTopologyP2P where
  parseJSON = Aeson.withObject "NetworkTopologyP2P" $ \o -> RealNodeTopologyP2P
    <$> (o Aeson..:  "localRoots"                                        )
    <*> (o Aeson..:  "publicRoots"                                       )
    <*> (o Aeson..:? "useLedgerAfterSlot" Aeson..!= DontUseLedgerPeers   )
    <*> (o Aeson..:? "bootstrapPeers"     Aeson..!= DontUseBootstrapPeers)
{- TODO:
    <*> (o Aeson..:? "peerSnapshotFile"                                  )
-}

instance Aeson.ToJSON NetworkTopologyP2P where
  toJSON top = Aeson.object
    [ "localRoots"         Aeson..= ntLocalRootPeersGroups top
    , "publicRoots"        Aeson..= ntPublicRootPeers      top
    , "useLedgerAfterSlot" Aeson..= ntUseLedgerPeers       top
    , "bootstrapPeers"     Aeson..= ntUseBootstrapPeers    top
{- TODO:
    , "peerSnapshotFile"   Aeson..= ntPeerSnapshotPath     top
-}
    ]

-- https://github.com/IntersectMBO/cardano-node/blob/52b708f37cd3dc92a188717deae2a6a60117f696/cardano-node/src/Cardano/Node/Configuration/TopologyP2P.hs#L87

-- | Each root peer consists of a list of access points and a shared
-- 'PeerAdvertise' field.
--
data RootConfig = RootConfig
  { rootAccessPoints :: [RelayAccessPoint]
    -- ^ a list of relay access points, each of which is either an ip address
    -- or domain name and a port number.
  , rootAdvertise    :: PeerAdvertise
    -- ^ 'advertise' configures whether the root should be advertised through
    -- peer sharing.
  } deriving (Eq, Show)

instance Aeson.FromJSON RootConfig where
  parseJSON = Aeson.withObject "RootConfig" $ \o -> RootConfig
    <$> o Aeson..:  "accessPoints"
    <*> o Aeson..:? "advertise" Aeson..!= DoNotAdvertisePeer

instance Aeson.ToJSON RootConfig where
  toJSON ra = Aeson.object
    [ "accessPoints" Aeson..= rootAccessPoints ra
    , "advertise"    Aeson..= rootAdvertise ra
    ]

-- Replaced `HotValency` and `WarmValency` with `Valency`.
-- https://github.com/IntersectMBO/cardano-node/blob/52b708f37cd3dc92a188717deae2a6a60117f696/cardano-node/src/Cardano/Node/Configuration/TopologyP2P.hs#L125

-- | A local root peers group.  Local roots are treated by the outbound
-- governor in a special way.  The node will make sure that a node has the
-- requested number ('valency'/'hotValency') of connections to the local root peer group.
-- 'warmValency' value is the value of warm/established connections that the node
-- will attempt to maintain. By default this value will be equal to 'hotValency'.
--
data LocalRootPeersGroup = LocalRootPeersGroup
  { localRoots        :: RootConfig
  , trustable         :: PeerTrustable
  , valency           :: Valency
    -- ^ 'trustable' configures whether the root should be trusted in fallback
    -- state.
{- TODO:
  , rootDiffusionMode :: DiffusionMode
    -- ^ diffusion mode; used for local root peers.
-}
  } deriving (Eq, Show)

-- | Does not use the 'FromJSON' instance of 'RootConfig', so that
-- 'accessPoints', 'advertise', 'valency' and 'warmValency' fields are attached
-- to the same object.
instance Aeson.FromJSON LocalRootPeersGroup where
  parseJSON = Aeson.withObject "LocalRootPeersGroup" $ \o -> do
    LocalRootPeersGroup
      <$> Aeson.parseJSON (Aeson.Object o)
      <*> o Aeson..:? "trustable" Aeson..!= IsNotTrustable
      <*> o Aeson..:  "valency"
{- TODO:
          -- deserialise via NodeDiffusionMode
      <*> (maybe InitiatorAndResponderDiffusionMode getDiffusionMode
            <$> o .:? "diffusionMode")
-}

instance Aeson.ToJSON LocalRootPeersGroup where
  toJSON lrpg = Aeson.object
    [ "accessPoints"  Aeson..= rootAccessPoints (localRoots lrpg)
    , "advertise"     Aeson..= rootAdvertise (localRoots lrpg)
    , "trustable"     Aeson..= trustable lrpg
    , "valency"       Aeson..= valency lrpg
{- TODO:
      -- serialise via NodeDiffusionMode
    , "diffusionMode" Aeson..= NodeDiffusionMode (rootDiffusionMode lrpg)
-}
    ]

newtype LocalRootPeersGroups = LocalRootPeersGroups
  { groups :: [LocalRootPeersGroup]
  } deriving (Eq, Show)

instance Aeson.FromJSON LocalRootPeersGroups where
  parseJSON = fmap LocalRootPeersGroups . Aeson.parseJSONList

instance Aeson.ToJSON LocalRootPeersGroups where
  toJSON = Aeson.toJSONList . groups

newtype PublicRootPeers = PublicRootPeers
  { publicRoots :: RootConfig
  } deriving (Eq, Show)

instance Aeson.FromJSON PublicRootPeers where
  parseJSON = fmap PublicRootPeers . Aeson.parseJSON

instance Aeson.ToJSON PublicRootPeers where
  toJSON = Aeson.toJSON . publicRoots

------------------------------------------------------
-- Ouroboros.Network.PeerSelection.RelayAccessPoint --
------------------------------------------------------

-- https://github.com/IntersectMBO/ouroboros-network/blob/588130b5f0494a42c19da3bd44211ff7607b5831/ouroboros-network-api/src/Ouroboros/Network/PeerSelection/RelayAccessPoint.hs#L58

data RelayAccessPoint =
    RelayAccessDomain  !DNS.Domain !Socket.PortNumber
  | RelayAccessAddress !IP.IP      !Socket.PortNumber
  deriving (Eq, Ord, Show)

-- https://github.com/IntersectMBO/ouroboros-network/blob/588130b5f0494a42c19da3bd44211ff7607b5831/ouroboros-network-api/src/Ouroboros/Network/PeerSelection/RelayAccessPoint.hs#L142

instance Aeson.FromJSON RelayAccessPoint where
  parseJSON = Aeson.withObject "RelayAccessPoint" $ \v -> do
    addr <- v Aeson..: "address"
    port <- v Aeson..: "port"
    return (toRelayAccessPoint addr port)

instance Aeson.ToJSON RelayAccessPoint where
  toJSON (RelayAccessDomain addr port) = Aeson.object
      [ "address" Aeson..= TextEncoding.decodeUtf8 addr
      , "port"    Aeson..= (fromIntegral port :: Int)
      ]
  toJSON (RelayAccessAddress ip port) = Aeson.object
      [ "address" Aeson..= Text.pack (show ip)
      , "port"    Aeson..= (fromIntegral port :: Int)
      ]

-- | Parse a address field as either an IP address or a DNS address.
-- Returns corresponding RelayAccessPoint.
--
toRelayAccessPoint :: Text.Text -> Int -> RelayAccessPoint
toRelayAccessPoint address port =
    case readMaybe (Text.unpack address) of
      Nothing   -> RelayAccessDomain
                     (TextEncoding.encodeUtf8 address)
                     (fromIntegral port)
      Just addr -> RelayAccessAddress addr (fromIntegral port)

---------------------------------------------------
-- Ouroboros.Network.PeerSelection.PeerAdvertise --
---------------------------------------------------

-- https://github.com/IntersectMBO/ouroboros-network/blob/588130b5f0494a42c19da3bd44211ff7607b5831/ouroboros-network-api/src/Ouroboros/Network/PeerSelection/PeerAdvertise.hs#L13

-- | Should this peer be advertised to other peers asking for known peers?
-- For certain peers specified by configuration it would be an appropriate
-- policy to keep them private.
--
data PeerAdvertise =
    DoNotAdvertisePeer
  | DoAdvertisePeer
  deriving (Eq, Show, Ord)

instance Aeson.FromJSON PeerAdvertise where
  parseJSON = Aeson.withBool "PeerAdvertise" $
      return . bool DoNotAdvertisePeer DoAdvertisePeer

instance Aeson.ToJSON PeerAdvertise where
  toJSON DoAdvertisePeer    = Aeson.Bool True
  toJSON DoNotAdvertisePeer = Aeson.Bool False

------------------------------------------------------
-- Ouroboros.Network.PeerSelection.LedgerPeers.Type --
------------------------------------------------------

-- https://github.com/IntersectMBO/ouroboros-network/blob/588130b5f0494a42c19da3bd44211ff7607b5831/ouroboros-network-api/src/Ouroboros/Network/PeerSelection/LedgerPeers/Type.hs#L178

-- | Only use the ledger after the given slot number.
data UseLedgerPeers =
    DontUseLedgerPeers
  | UseLedgerPeers AfterSlot
  deriving (Eq, Show, Generic)

-- | Only use the ledger after the given slot number.
data AfterSlot =
    Always
  | After SlotNo
  deriving (Eq, Show)

-- `FromJSON`/`ToJSON` from "Cardano.Tracing.OrphanInstances.Network".

-- https://github.com/IntersectMBO/cardano-node/blob/52b708f37cd3dc92a188717deae2a6a60117f696/cardano-node/src/Cardano/Tracing/OrphanInstances/Network.hs#L2784

instance Aeson.FromJSON UseLedgerPeers where
  parseJSON (Aeson.Number slot) = return $
    case compare slot 0 of
      GT -> UseLedgerPeers (After (SlotNo (floor slot)))
      EQ -> UseLedgerPeers Always
      LT -> DontUseLedgerPeers
  parseJSON invalid = fail $ "Parsing of slot number failed due to type mismatch. "
                            <> "Encountered: " <> show invalid

-- https://github.com/IntersectMBO/cardano-node/blob/52b708f37cd3dc92a188717deae2a6a60117f696/cardano-node/src/Cardano/Tracing/OrphanInstances/Network.hs#L2811

instance Aeson.ToJSON UseLedgerPeers where
  toJSON DontUseLedgerPeers                  = Aeson.Number (-1)
  toJSON (UseLedgerPeers Always)             = Aeson.Number 0
  toJSON (UseLedgerPeers (After (SlotNo s))) = Aeson.Number (fromIntegral s)

---------------------------
-- Cardano.Slotting.Slot --
---------------------------

-- | The 0-based index for the Ourboros time slot.
newtype SlotNo = SlotNo {unSlotNo :: Word64}
  deriving (Eq, Ord, Show)

----------------------------------------------------------
-- Ouroboros.Network.PeerSelection.State.LocalRootPeers --
----------------------------------------------------------

-- Replaced `HotValency` and `WarmValency` with `Valency`.
-- https://github.com/IntersectMBO/ouroboros-network/blob/588130b5f0494a42c19da3bd44211ff7607b5831/ouroboros-network/src/Ouroboros/Network/PeerSelection/State/LocalRootPeers.hs#L80

newtype Valency = Valency { getValency :: Int }
  deriving (Show, Eq, Ord)

-- | Newtype wrapper representing hot valency value from local root group
-- configuration
--
newtype HotValency = HotValency { getHotValency :: Int }
  deriving (Show, Eq, Ord)

-- | Newtype wrapper representing warm valency value from local root group
-- configuration
--
newtype WarmValency = WarmValency { getWarmValency :: Int }
  deriving (Show, Eq, Ord)

-- `FromJSON`/`ToJSON` from "Cardano.Tracing.OrphanInstances.Network".

-- Replaced `HotValency` and `WarmValency` with `Valency`.
-- https://github.com/IntersectMBO/cardano-node/blob/52b708f37cd3dc92a188717deae2a6a60117f696/cardano-node/src/Cardano/Tracing/OrphanInstances/Network.hs#L1638

instance Aeson.ToJSON Valency where
  toJSON (Valency v) = Aeson.toJSON v

instance Aeson.ToJSON HotValency where
  toJSON (HotValency v) = Aeson.toJSON v

instance Aeson.ToJSON WarmValency where
  toJSON (WarmValency v) = Aeson.toJSON v

instance Aeson.FromJSON Valency where
  parseJSON v = Valency <$> Aeson.parseJSON v

instance Aeson.FromJSON HotValency where
  parseJSON v = HotValency <$> Aeson.parseJSON v

instance Aeson.FromJSON WarmValency where
  parseJSON v = WarmValency <$> Aeson.parseJSON v

---------------------------------------------------
-- Ouroboros.Network.PeerSelection.PeerTrustable --
---------------------------------------------------

-- https://github.com/IntersectMBO/ouroboros-network/blob/588130b5f0494a42c19da3bd44211ff7607b5831/ouroboros-network-api/src/Cardano/Network/PeerSelection/PeerTrustable.hs#L13

-- | Is this Peer trustable as a bootstrap peer?
--
-- This trustability flag is used on local root peers (pre-genesis) to
-- distinguish which locally configured peer is considered safe to trust for
-- bootstrap purposes
--
data PeerTrustable =
    IsTrustable
  | IsNotTrustable
  deriving (Eq, Show, Ord, Generic)

-- `FromJSON`/`ToJSON` from "Cardano.Tracing.OrphanInstances.Network".

-- https://github.com/IntersectMBO/cardano-node/blob/52b708f37cd3dc92a188717deae2a6a60117f696/cardano-node/src/Cardano/Tracing/OrphanInstances/Network.hs#L2824

instance Aeson.FromJSON PeerTrustable where
  parseJSON = Aeson.withBool "PeerTrustable" $ \b ->
    pure $ if b then IsTrustable
                else IsNotTrustable

instance Aeson.ToJSON PeerTrustable where
  toJSON IsTrustable = Aeson.Bool True
  toJSON IsNotTrustable = Aeson.Bool False

-----------------------------------------------
-- Ouroboros.Network.PeerSelection.Bootstrap --
-----------------------------------------------

-- https://github.com/IntersectMBO/ouroboros-network/blob/588130b5f0494a42c19da3bd44211ff7607b5831/ouroboros-network-api/src/Cardano/Network/PeerSelection/Bootstrap.hs#L15

data UseBootstrapPeers =
    DontUseBootstrapPeers
  | UseBootstrapPeers [RelayAccessPoint]
  deriving (Eq, Show, Ord, Generic)

-- `FromJSON`/`ToJSON` from "Cardano.Tracing.OrphanInstances.Network".

-- https://github.com/IntersectMBO/cardano-node/blob/52b708f37cd3dc92a188717deae2a6a60117f696/cardano-node/src/Cardano/Tracing/OrphanInstances/Network.hs#L2816

instance Aeson.ToJSON UseBootstrapPeers where
  toJSON DontUseBootstrapPeers   = Aeson.Null
  toJSON (UseBootstrapPeers dps) = Aeson.toJSON dps

instance Aeson.FromJSON UseBootstrapPeers where
  parseJSON Aeson.Null = pure DontUseBootstrapPeers
  parseJSON v          = UseBootstrapPeers <$> Aeson.parseJSON v
