{-# LANGUAGE TemplateHaskell #-}

module Test.PlutusExample.ScriptData where

import           Cardano.Prelude

import           Cardano.Api

import qualified Data.Aeson as Aeson

import           Hedgehog (Property, checkParallel, discover, forAll, property, tripping, (===))
import           Hedgehog.Internal.Property (failWith)

import           Cardano.PlutusExample.ScriptContextChecker

import           Test.PlutusExample.Gen

prop_ScriptData_MyCustomRedeemer :: Property
prop_ScriptData_MyCustomRedeemer =
  property $ do
    myCusRedeem <- forAll genMyCustomRedeemer
    tripping myCusRedeem
             customRedeemerToScriptData
             customRedeemerFromScriptData

prop_ScriptData_MyCustomRedeemer_JSON :: Property
prop_ScriptData_MyCustomRedeemer_JSON =
  property $ do
    myCusRedeem <- forAll genMyCustomRedeemer
    let sData = Aeson.encode $ scriptDataToJson ScriptDataJsonDetailedSchema $ customRedeemerToScriptData myCusRedeem
    case Aeson.eitherDecode sData of
      Left err -> failWith Nothing $ "Failed to decode script data (eitherDecode): " ++ err
      Right sDataJsonValue ->
        case scriptDataFromJson ScriptDataJsonDetailedSchema sDataJsonValue of
          Left err ->
            failWith Nothing $ "Failed to decode script data (scriptDataFromJson): " ++ displayError err
          Right sDataDecoded ->
            case customRedeemerFromScriptData sDataDecoded of
              Left err ->
                failWith Nothing $ "Failed to decode custom redeemer(customRedeemerFromScriptData): " ++ err
              Right cusRedDecoded -> do
                 myCusRedeem === cusRedDecoded
                 sData === Aeson.encode
                             (scriptDataToJson ScriptDataJsonDetailedSchema
                               $ customRedeemerToScriptData cusRedDecoded)


-- -----------------------------------------------------------------------------

tests :: IO Bool
tests = checkParallel $$discover

