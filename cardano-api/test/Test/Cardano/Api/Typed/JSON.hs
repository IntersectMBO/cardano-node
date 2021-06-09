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
import           Data.Aeson
import           Gen.Cardano.Api.Typed
import           Gen.Tasty.Hedgehog.Group (fromGroup)
import           Hedgehog (Property, discover, forAll, tripping)
import           Test.Cardano.Api.Typed.Orphans ()
import           Test.Tasty (TestTree)

import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen

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
tests = fromGroup $$discover
