{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Test.Cardano.Node.Gen
  ( genNetworkTopology
  , genNetworkTopologyEncoding
  , genNodeHostIPv4Address
  , genNodeHostIPv6Address
  , genNodeHostIPAddress
  , genNodeIPAddress
  , genNodeIPv4Address
  , genNodeIPv6Address
  ) where

import           Cardano.Api (textShow)

import           Cardano.Network.Diffusion.Topology (CardanoNetworkTopology)
import           Cardano.Network.NodeToNode.Version
import           Cardano.Network.PeerSelection.Bootstrap
import           Cardano.Network.PeerSelection.PeerTrustable
import           Cardano.Node.Configuration.NodeAddress (NodeAddress' (..), NodeHostIPAddress (..),
                   NodeHostIPv4Address (..), NodeHostIPv6Address (..), NodeIPAddress,
                   NodeIPv4Address, NodeIPv6Address)
import           Cardano.Node.Types
import           Cardano.Slotting.Slot (SlotNo (..))
import           Ouroboros.Network.ConnectionManager.Types (Provenance (..))
import           Ouroboros.Network.Diffusion.Topology (LocalRootPeersGroup (..),
                   LocalRootPeersGroups (..), LocalRoots (..), NetworkTopology (..),
                   PublicRootPeers (..), RootConfig (..))
import           Ouroboros.Network.PeerSelection.LedgerPeers.Type (AfterSlot (..),
                   UseLedgerPeers (..))
import           Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise (..))
import           Ouroboros.Network.PeerSelection.RelayAccessPoint (RelayAccessPoint (..))
import           Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency (..),
                   WarmValency (..))

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IP as IP
import qualified Data.Vector as Vector
import           Data.Word (Word32)

import           Hedgehog (Gen)
import           Hedgehog.Corpus (cooking)
import qualified Hedgehog.Gen as Gen
import           Hedgehog.Internal.Gen ()
import qualified Hedgehog.Range as Range

-- TODO parameterize generators
genNetworkTopology :: Gen CardanoNetworkTopology
genNetworkTopology =
  Gen.choice
    [ NetworkTopology <$> genLocalRootPeersGroups
                      <*> Gen.list (Range.linear 0 10) genPublicRootPeers
                      <*> genUseLedgerPeers
                      <*> (fmap unPeerSnapshotFile <$> genPeerSnapshotPath)
                      <*> genUseBootstrapPeers
    ]

-- | Generate valid encodings of p2p topology files
--
genNetworkTopologyEncoding :: Gen LBS.ByteString
genNetworkTopologyEncoding = Aeson.encode <$> genNetworkTopologyValue

-- | Generate a Aeson.Object which encodes a p2p topology.
--
genNetworkTopologyValue :: Gen Aeson.Object
genNetworkTopologyValue =
    (\a b c -> Aeson.KeyMap.fromList
                 [ ("localRoots", Aeson.Array . Vector.fromList $ a)
                 , ("publicRoots", Aeson.Array . Vector.fromList $ b)
                 , ("useLedgerAfter", Aeson.Number . fromIntegral $ c)
                 ]
    ) <$> Gen.list (Range.constantFrom 0 0 10) genLocalRootsValue
      <*> Gen.list (Range.constantFrom 0 0 10) genPublicRootsValue
      <*> Gen.int (Range.constantFrom 0 0 100)
  where
    genLocalRootsValue :: Gen Aeson.Value
    genLocalRootsValue =
      (\a b c -> Aeson.Object $ Aeson.KeyMap.fromList
                   [ ("accessPoints", Aeson.Array . Vector.fromList $ a)
                   , ("advertise", Aeson.Bool b)
                   , ("valency", Aeson.Number (fromIntegral c))
                   ]
      ) <$> Gen.list (Range.constantFrom 0 0 10) genAccessPointValue
        <*> Gen.bool
        <*> Gen.int (Range.constantFrom 0 0 100)

    genPublicRootsValue :: Gen Aeson.Value
    genPublicRootsValue =
      (\a b -> Aeson.Object $ Aeson.KeyMap.fromList
                 [ ("accessPoints", Aeson.Array . Vector.fromList $ a)
                 , ("advertise", Aeson.Bool b)
                 ]
      ) <$> Gen.list (Range.constantFrom 0 0 10) genAccessPointValue
        <*> Gen.bool

    genAccessPointValue :: Gen Aeson.Value
    genAccessPointValue =
      (\a -> Aeson.Object $ Aeson.KeyMap.fromList
                 [ ("address", Aeson.String (textShow $ naHostAddress a))
                 , ("port", Aeson.Number (fromIntegral $ naPort a))
                 ]
      ) <$> genNodeIPAddress


genNodeAddress' :: Gen addr -> Gen (NodeAddress' addr)
genNodeAddress' genAddr =
  NodeAddress
    <$> genAddr
    <*> fmap fromIntegral (Gen.word16 $ Range.linear 100 20000)

genNodeHostIPv4Address :: Gen NodeHostIPv4Address
genNodeHostIPv4Address =
  NodeHostIPv4Address . IP.toIPv4w <$> Gen.enumBounded

genNodeHostIPv6Address :: Gen NodeHostIPv6Address
genNodeHostIPv6Address =
    NodeHostIPv6Address . IP.toIPv6w <$> genFourWord32
  where
    genFourWord32 :: Gen (Word32, Word32, Word32, Word32)
    genFourWord32 =
       (,,,) <$> Gen.enumBounded
             <*> Gen.enumBounded
             <*> Gen.enumBounded
             <*> Gen.enumBounded

genNodeHostIPAddress :: Gen NodeHostIPAddress
genNodeHostIPAddress =
  NodeHostIPAddress
    <$> Gen.choice
          [ IP.IPv4 . unNodeHostIPv4Address <$> genNodeHostIPv4Address
          , IP.IPv6 . unNodeHostIPv6Address <$> genNodeHostIPv6Address
          ]

genNodeIPAddress :: Gen NodeIPAddress
genNodeIPAddress = genNodeAddress' genNodeHostIPAddress

genNodeIPv4Address :: Gen NodeIPv4Address
genNodeIPv4Address = genNodeAddress' genNodeHostIPv4Address

genNodeIPv6Address :: Gen NodeIPv6Address
genNodeIPv6Address = genNodeAddress' genNodeHostIPv6Address

-- Generates only fully qualified domain names.
--
genRelayAddress :: Gen RelayAccessPoint
genRelayAddress =
  Gen.choice
    [ RelayAccessDomain
        . (<> ".")
        <$> Gen.element cooking
        <*> (fromIntegral <$> Gen.int (Range.linear 1000 9000))
    , RelayAccessSRVDomain . (<> ".") <$> Gen.element cooking
    , RelayAccessAddress
        <$> Gen.choice
              [ IP.IPv4 . unNodeHostIPv4Address <$> genNodeHostIPv4Address
              , IP.IPv6 . unNodeHostIPv6Address <$> genNodeHostIPv6Address
              ]
        <*> (fromIntegral <$> Gen.int (Range.linear 1000 9000))
    ]

genRootConfig :: Gen RootConfig
genRootConfig = do
  RootConfig
    <$> Gen.list (Range.linear 0 6) genRelayAddress
    <*> Gen.element [DoAdvertisePeer, DoNotAdvertisePeer]

genProvenance :: Gen Provenance
genProvenance = Gen.element [Outbound, Inbound]

genLocalRoots :: Gen LocalRoots
genLocalRoots =
  LocalRoots
    <$> genRootConfig
    <*> genProvenance

genLocalRootPeersGroup :: Gen (LocalRootPeersGroup PeerTrustable)
genLocalRootPeersGroup = do
    ra <- genLocalRoots
    hval <- Gen.int (Range.linear 0 (length (rootAccessPoints (rootConfig ra))))
    wval <- WarmValency <$> Gen.int (Range.linear 0 hval)
    LocalRootPeersGroup ra (HotValency hval) wval InitiatorAndResponderDiffusionMode <$> genPeerTrustable

genLocalRootPeersGroups :: Gen (LocalRootPeersGroups PeerTrustable)
genLocalRootPeersGroups =
  LocalRootPeersGroups
    <$> Gen.list (Range.linear 0 6) genLocalRootPeersGroup

genPublicRootPeers :: Gen PublicRootPeers
genPublicRootPeers =
  PublicRootPeers
    <$> genRootConfig

genUseLedgerPeers :: Gen UseLedgerPeers
genUseLedgerPeers = do
    slot <- Gen.integral (Range.linear (-1) 10) :: Gen Integer
    return $ case compare slot 0 of
      GT -> UseLedgerPeers $ After $ SlotNo $ fromIntegral slot
      EQ -> UseLedgerPeers Always
      LT -> DontUseLedgerPeers

genUseBootstrapPeers :: Gen UseBootstrapPeers
genUseBootstrapPeers = do
  domains <- Gen.list (Range.linear 0 6) genRelayAddress
  Gen.element [ DontUseBootstrapPeers , UseBootstrapPeers domains ]

genPeerSnapshotPath :: Gen (Maybe PeerSnapshotFile)
genPeerSnapshotPath =
  Gen.element
    [ Nothing
    , Just . PeerSnapshotFile $ "dummy"
    ]

genPeerTrustable :: Gen PeerTrustable
genPeerTrustable = Gen.element [ IsNotTrustable, IsTrustable ]
