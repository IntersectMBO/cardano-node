{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Api.Typed.JSON
  ( tests
  ) where

import           Cardano.Api
import           Cardano.Prelude

import           Data.Aeson

import           Hedgehog (Gen, Property, discover, forAll, tripping)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog.Group (fromGroup)

import           Test.Cardano.Api.Typed.Gen


{- HLINT ignore "Use camelCase" -}

prop_roundtrip_praos_nonce_JSON :: Property
prop_roundtrip_praos_nonce_JSON = H.property $ do
  pNonce <- forAll $ Gen.just genMaybePraosNonce
  tripping pNonce encode eitherDecode

prop_roundtrip_protocol_parameters_JSON_Shelley :: Property
prop_roundtrip_protocol_parameters_JSON_Shelley =
  rountrip_JSON_Era genProtocolParameters ShelleyBasedEraShelley

prop_roundtrip_protocol_parameters_JSON_Allegra :: Property
prop_roundtrip_protocol_parameters_JSON_Allegra =
  rountrip_JSON_Era genProtocolParameters ShelleyBasedEraAllegra

prop_roundtrip_protocol_parameters_JSON_Mary :: Property
prop_roundtrip_protocol_parameters_JSON_Mary =
  rountrip_JSON_Era genProtocolParameters ShelleyBasedEraMary

-- -----------------------------------------------------------------------------

roundtrip_CBOR
  :: (SerialiseAsCBOR a, Eq a, Show a)
  => AsType a -> Gen a -> Property
roundtrip_CBOR typeProxy gen =
  H.property $ do
    val <- H.forAll gen
    H.tripping val serialiseToCBOR (deserialiseFromCBOR typeProxy)

rountrip_JSON_Era
  :: (FromJSON a, ToJSON a, Eq a, Show a)
  => (ShelleyBasedEra era -> Gen a) -> ShelleyBasedEra era -> Property
rountrip_JSON_Era gen sbe = H.property $ do
  pp <- H.forAll $ gen sbe
  tripping pp encode eitherDecode

-- -----------------------------------------------------------------------------

tests :: TestTree
tests = fromGroup $$discover
