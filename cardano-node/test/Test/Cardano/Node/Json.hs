{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Cardano.Node.Json
  ( tests
  ) where

import           Cardano.Node.Configuration.TopologyP2P (NetworkTopology (..), NodeSetup (..))
import           Ouroboros.Network.PeerSelection.LedgerPeers.Type (AfterSlot (..),
                   UseLedgerPeers (..))

import           Data.Aeson (decode, encode, fromJSON, toJSON)
import           Data.Maybe (isJust)

import           Test.Cardano.Node.Gen

import           Hedgehog (Property, discover)
import qualified Hedgehog


prop_roundtrip_NodeIPv4Address_JSON :: Property
prop_roundtrip_NodeIPv4Address_JSON =
  Hedgehog.property $ do
    na <- Hedgehog.forAll genNodeIPv4Address
    Hedgehog.tripping na toJSON fromJSON
    Hedgehog.tripping na encode decode

prop_roundtrip_NodeIPv6Address_JSON :: Property
prop_roundtrip_NodeIPv6Address_JSON =
  Hedgehog.property $ do
    na <- Hedgehog.forAll genNodeIPv6Address
    Hedgehog.tripping na toJSON fromJSON
    Hedgehog.tripping na encode decode

prop_roundtrip_NodeIPAddress_JSON :: Property
prop_roundtrip_NodeIPAddress_JSON =
  Hedgehog.property $ do
    na <- Hedgehog.forAll genNodeIPAddress
    Hedgehog.tripping na toJSON fromJSON
    Hedgehog.tripping na encode decode

prop_roundtrip_NodeHostAddress_JSON :: Property
prop_roundtrip_NodeHostAddress_JSON =
  Hedgehog.property $ do
    nha <- Hedgehog.forAll genNodeHostIPAddress
    Hedgehog.tripping nha toJSON fromJSON
    Hedgehog.tripping nha encode decode

prop_roundtrip_NodeSetup_JSON :: Property
prop_roundtrip_NodeSetup_JSON =
  Hedgehog.property $ do
    ns <- Hedgehog.forAll genNodeSetup
    Hedgehog.tripping ns toJSON (adjustUseLedger . fromJSON)
    Hedgehog.tripping ns encode (adjustUseLedger . decode)
  where
    -- 'useLedger' will be parsed as 'Always' instead of 'After 0'
    adjustUseLedger :: Functor f => f NodeSetup -> f NodeSetup
    adjustUseLedger = fmap $ \x -> x { useLedger = fixUseLedger $ useLedger x }
    fixUseLedger (UseLedgerPeers Always) = UseLedgerPeers (After 0)
    fixUseLedger ulp                     = ulp

prop_roundtrip_NetworkTopology_JSON :: Property
prop_roundtrip_NetworkTopology_JSON =
  Hedgehog.property $ do
    ntop <- Hedgehog.forAll genNetworkTopology
    Hedgehog.tripping ntop toJSON fromJSON
    Hedgehog.tripping ntop encode decode

-- | Verify that we can parse valid encoding of p2p topology files.
--
prop_decode_NetworkTopology_JSON :: Property
prop_decode_NetworkTopology_JSON =
  Hedgehog.property $ do
    enc <- Hedgehog.forAll genNetworkTopologyEncoding
    let tp :: Maybe NetworkTopology
        tp = decode enc
    Hedgehog.assert $ isJust tp


-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  Hedgehog.checkParallel $$discover
