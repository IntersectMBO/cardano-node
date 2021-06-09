{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Api.Json
  ( tests
  ) where

import           Cardano.Api.Orphans ()
import           Cardano.Prelude (($))
import           Data.Aeson (eitherDecode, encode)
import           Gen.Cardano.Api (genAlonzoGenesis)
import           Gen.Tasty.Hedgehog.Group (fromGroup)
import           Hedgehog (Property, discover, forAll, tripping)
import           Test.Tasty (TestTree)

import qualified Hedgehog as H

{- HLINT ignore "Use camelCase" -}

prop_roundtrip_alonzo_genesis :: Property
prop_roundtrip_alonzo_genesis = H.property $ do
  genesis <- forAll genAlonzoGenesis
  tripping genesis encode eitherDecode

tests :: TestTree
tests = fromGroup $$discover
