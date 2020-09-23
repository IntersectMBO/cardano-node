{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Api.Typed.Json
  ( tests
  ) where

import           Cardano.Prelude
import           Hedgehog (Property, discover)
import           Test.Cardano.Api.Typed.Gen
import           Test.Cardano.Api.Typed.Orphans ()
import           Test.Cardano.Prelude

import qualified Hedgehog as H

{- HLINT ignore "Use camelCase" -}

prop_roundtrip_Address_Byron_Json :: Property
prop_roundtrip_Address_Byron_Json =
  eachOf 1000 genAddressByron roundTripsAesonShow

prop_roundtrip_Address_Shelley_Json :: Property
prop_roundtrip_Address_Shelley_Json =
  eachOf 1000 genAddressShelley roundTripsAesonShow

prop_roundtrip_TxId_Json :: Property
prop_roundtrip_TxId_Json = eachOf 1000 genTxId roundTripsAesonShow

prop_roundtrip_TxIx_Json :: Property
prop_roundtrip_TxIx_Json = eachOf 1000 genTxIndex roundTripsAesonShow

prop_roundtrip_TxIn_Json :: Property
prop_roundtrip_TxIn_Json = eachOf 1000 genTxIn roundTripsAesonShow

prop_roundtrip_TxOut_Byron_Json :: Property
prop_roundtrip_TxOut_Byron_Json =
  eachOf 1000 genByronTxOut roundTripsAesonShow

prop_roundtrip_TxOut_Shelley_Json :: Property
prop_roundtrip_TxOut_Shelley_Json =
  eachOf 1000 genShelleyTxOut roundTripsAesonShow

------------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkParallel $$discover
