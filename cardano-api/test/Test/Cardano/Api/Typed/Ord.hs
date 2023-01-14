module Test.Cardano.Api.Typed.Ord
  ( tests
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley
import           Gen.Cardano.Api.Typed
import           Hedgehog (Property, (===))
import           Prelude
import           Test.Cardano.Api.Metadata (genTxMetadataValue)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testPropertyNamed)

import qualified Hedgehog as H

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
tests = testGroup "Test.Cardano.Api.Typed.Ord"
  [ testPropertyNamed "ord distributive TxId"         "ord distributive TxId"           prop_ord_distributive_TxId
  , testPropertyNamed "ord distributive TxIn"         "ord distributive TxIn"           prop_ord_distributive_TxIn
  , testPropertyNamed "ord distributive Address"      "ord distributive Address"        prop_ord_distributive_Address
  , testPropertyNamed "ord distributive StakeAddress" "ord distributive StakeAddress"   prop_ord_distributive_StakeAddress
  , testPropertyNamed "ord distributive TxMetadata"   "ord distributive TxMetadata"     prop_ord_distributive_TxMetadata
  , testPropertyNamed "ord distributive ScriptData"   "ord distributive ScriptData"     prop_ord_distributive_ScriptData
  ]
