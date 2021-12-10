{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Api.Typed.JSON
  ( tests
  ) where

import           Cardano.Api

import           Data.Aeson (eitherDecode, encode)
import           Hedgehog (Property, forAll, tripping)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import           Test.Cardano.Api.Typed.Orphans ()
import           Test.Gen.Cardano.Api.Typed (genMaybePraosNonce, genProtocolParameters)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testPropertyNamed)

{- HLINT ignore "Use camelCase" -}

prop_roundtrip_praos_nonce_JSON :: Property
prop_roundtrip_praos_nonce_JSON = H.property $ do
  pNonce <- forAll $ Gen.just genMaybePraosNonce
  tripping pNonce encode eitherDecode

prop_roundtrip_protocol_parameters_JSON :: Property
prop_roundtrip_protocol_parameters_JSON = H.property $ do
  AnyCardanoEra era <- forAll $ Gen.element [minBound .. maxBound]
  pp <- forAll (genProtocolParameters era)
  tripping pp encode eitherDecode

-- -----------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Test.Cardano.Api.Typed.JSON"
  [ testPropertyNamed "roundtrip praos nonce JSON"         "roundtrip praos nonce JSON"         prop_roundtrip_praos_nonce_JSON
  , testPropertyNamed "roundtrip protocol parameters JSON" "roundtrip protocol parameters JSON" prop_roundtrip_protocol_parameters_JSON
  ]
