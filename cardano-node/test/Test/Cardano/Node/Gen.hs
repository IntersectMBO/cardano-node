{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
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
  , genNodeSetup
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.Vector as Vector
import qualified Data.ByteString.Lazy as LBS

import           Cardano.Prelude

import           Cardano.Node.Configuration.TopologyP2P (NetworkTopology (..), PublicRootPeers (..),
                   LocalRootPeersGroups (..), LocalRootPeersGroup (..), RootConfig (..),
                   NodeSetup (..), PeerAdvertise (..), UseLedger (..))
import           Cardano.Node.Configuration.NodeAddress (NodeAddress' (..), NodeHostIPAddress (..),
                   NodeHostIPv4Address (..), NodeHostIPv6Address (..),
                   NodeIPAddress, NodeIPv4Address, NodeIPv6Address)
import           Cardano.Slotting.Slot (SlotNo (..))
import           Ouroboros.Network.PeerSelection.LedgerPeers (UseLedgerAfter (..))
import           Ouroboros.Network.PeerSelection.RelayAccessPoint (
                   DomainAccessPoint (..), RelayAccessPoint (..))


import qualified Data.IP as IP

import           Hedgehog (Gen)
import           Hedgehog.Corpus (cooking)
import qualified Hedgehog.Gen as Gen
import           Hedgehog.Internal.Gen ()
import qualified Hedgehog.Range as Range

genNetworkTopology :: Gen NetworkTopology
genNetworkTopology =
  Gen.choice
    [ RealNodeTopology <$> genLocalRootPeersGroups
                       <*> Gen.list (Range.linear 0 10) genPublicRootPeers
                       <*> genUseLedger
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
                 [ ("address", Aeson.String (show $ naHostAddress a))
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

genNodeSetup :: Gen NodeSetup
genNodeSetup =
  NodeSetup
    <$> Gen.word64 (Range.linear 0 10000)
    <*> Gen.maybe (genNodeAddress' genNodeHostIPv4Address)
    <*> Gen.maybe (genNodeAddress' genNodeHostIPv6Address)
    <*> Gen.list (Range.linear 0 6) genRootConfig
    <*> genUseLedger

genDomainAddress :: Gen DomainAccessPoint
genDomainAddress =
  DomainAccessPoint
    <$> Gen.element cooking
    <*> (fromIntegral <$> Gen.int (Range.linear 1000 9000))

genRelayAddress :: Gen RelayAccessPoint
genRelayAddress = do
  isDomain <- Gen.bool
  if isDomain
    then RelayDomainAccessPoint <$> genDomainAddress
    else RelayAccessAddress
          <$> Gen.choice
                [ IP.IPv4 . unNodeHostIPv4Address <$> genNodeHostIPv4Address
                , IP.IPv6 . unNodeHostIPv6Address <$> genNodeHostIPv6Address
                ]
          <*> (fromIntegral <$> Gen.int (Range.linear 1000 9000))

genRootConfig :: Gen RootConfig
genRootConfig = do
  RootConfig
    <$> Gen.list (Range.linear 0 6) genRelayAddress
    <*> Gen.element [DoAdvertisePeer, DoNotAdvertisePeer]

genLocalRootPeersGroup :: Gen LocalRootPeersGroup
genLocalRootPeersGroup = do
    ra <- genRootConfig
    val <- Gen.int (Range.linear 0 (length (rootAccessPoints ra)))
    return (LocalRootPeersGroup ra val)

genLocalRootPeersGroups :: Gen LocalRootPeersGroups
genLocalRootPeersGroups =
  LocalRootPeersGroups
    <$> Gen.list (Range.linear 0 6) genLocalRootPeersGroup

genPublicRootPeers :: Gen PublicRootPeers
genPublicRootPeers =
  PublicRootPeers
    <$> genRootConfig

genUseLedger :: Gen UseLedger
genUseLedger = do
    slot <- Gen.integral (Range.linear (-1) 10) :: Gen Integer
    if slot >= 0 then return $ UseLedger $ UseLedgerAfter $ SlotNo $ fromIntegral slot
                 else return $ UseLedger   DontUseLedger
