{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-deprecations #-} -- TODO Fix deprecations

module Test.Cardano.Api.Typed.Ord
  ( tests
  ) where

import           Prelude

import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import           Test.Cardano.Api.Metadata (genTxMetadataValue)
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.TH (testGroupGenerator)

import           Cardano.Api
import           Cardano.Api.Shelley
import           Gen.Cardano.Api.Typed

{- HLINT ignore "Use camelCase" -}


ord_distributive :: (Show a, Ord a, Ord b)
                      => H.Gen a -> (a -> b) -> Property
ord_distributive gen to =
    H.property $ do
      x <- H.forAll gen
      y <- H.forAll gen
      compare x y === compare (to x) (to y)


prop_ord_distributive_TxId :: Property
prop_ord_distributive_TxId =
    ord_distributive genTxId toShelleyTxId

prop_ord_distributive_TxIn :: Property
prop_ord_distributive_TxIn =
    ord_distributive genTxIn toShelleyTxIn

prop_ord_distributive_Address :: Property
prop_ord_distributive_Address =
    ord_distributive genAddressShelley (toShelleyAddr . toAddressInAnyEra)
  where
    toAddressInAnyEra :: Address ShelleyAddr -> AddressInEra ShelleyEra
    toAddressInAnyEra = anyAddressInShelleyBasedEra . toAddressAny

prop_ord_distributive_StakeAddress :: Property
prop_ord_distributive_StakeAddress =
    ord_distributive genStakeAddress toShelleyStakeAddr

prop_ord_distributive_TxMetadata :: Property
prop_ord_distributive_TxMetadata =
    ord_distributive genTxMetadataValue toShelleyMetadatum

prop_ord_distributive_ScriptData :: Property
prop_ord_distributive_ScriptData =
    ord_distributive genScriptData toPlutusData

-- -----------------------------------------------------------------------------

tests :: TestTree
tests = $testGroupGenerator
