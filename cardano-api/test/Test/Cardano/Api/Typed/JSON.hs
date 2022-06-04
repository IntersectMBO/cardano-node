{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-deprecations #-} -- TODO Fix deprecations
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Api.Typed.JSON
  ( tests
  ) where

import           Cardano.Prelude

import           Data.Aeson (eitherDecode, encode)
import           Hedgehog (Property, forAll, tripping)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.TH (testGroupGenerator)

import           Gen.Cardano.Api.Typed (genMaybePraosNonce, genProtocolParameters)

import           Test.Cardano.Api.Typed.Orphans ()

{- HLINT ignore "Use camelCase" -}

prop_roundtrip_praos_nonce_JSON :: Property
prop_roundtrip_praos_nonce_JSON = H.property $ do
  pNonce <- forAll $ Gen.just genMaybePraosNonce
  tripping pNonce encode eitherDecode

prop_roundtrip_protocol_parameters_JSON :: Property
prop_roundtrip_protocol_parameters_JSON = H.property $ do
  pp <- forAll genProtocolParameters
  tripping pp encode eitherDecode

-- -----------------------------------------------------------------------------

tests :: TestTree
tests = $testGroupGenerator
