{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Api.Typed.JSON
  ( tests
  ) where

import           Cardano.Prelude

import           Data.Aeson (eitherDecode, encode)
import           Hedgehog (Property, forAll, tripping)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.TH (testGroupGenerator)

import           Cardano.Api

import           Gen.Cardano.Api.Typed

import           Test.Cardano.Api.Typed.Orphans ()

{- HLINT ignore "Use camelCase" -}

prop_roundtrip_praos_nonce_JSON :: Property
prop_roundtrip_praos_nonce_JSON = H.property $ do
  pNonce <- forAll $ Gen.just genMaybePraosNonce
  tripping pNonce encode eitherDecode

test_roundtrip_protocol_parameters_JSON :: [TestTree]
test_roundtrip_protocol_parameters_JSON =
  [ testProperty (show era) $
    H.property $ do
      pp <- forAll $ genProtocolParameters era
      tripping pp encode eitherDecode
  | AnyCardanoEra era <- [minBound..]
  ]

-- -----------------------------------------------------------------------------

tests :: TestTree
tests = $testGroupGenerator
