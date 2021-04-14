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

prop_roundtrip_protocol_parameters_JSON :: Property
prop_roundtrip_protocol_parameters_JSON = H.property $ do
  pp <- forAll $ genProtocolParameters ShelleyBasedEraMary -- TODO: Generate all eras
  tripping pp encode eitherDecode


-- -----------------------------------------------------------------------------

roundtrip_CBOR
  :: (SerialiseAsCBOR a, Eq a, Show a)
  => AsType a -> Gen a -> Property
roundtrip_CBOR typeProxy gen =
  H.property $ do
    val <- H.forAll gen
    H.tripping val serialiseToCBOR (deserialiseFromCBOR typeProxy)



-- -----------------------------------------------------------------------------

tests :: TestTree
tests = fromGroup $$discover
