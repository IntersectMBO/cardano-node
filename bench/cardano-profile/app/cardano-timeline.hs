{-# LANGUAGE OverloadedStrings #-}

{-- Create a delta timeline with each parameters and cost changes that happened
    in an epoch using "epoch_param" and "cost_model" db-sync tables.
--}
--------------------------------------------------------------------------------

module Main where

--------------------------------------------------------------------------------

import Prelude
import Data.Foldable (toList)
import qualified Data.List as List
import Data.String (fromString)
-- Package: aeson.
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.Aeson.KeyMap as KeyMap
-- Package: text.
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.Text.Lazy.IO as TIO
-- Package: self.
import qualified Paths_cardano_profile as Paths

--------------------------------------------------------------------------------

-- Same as used in data/genesis/epoch-timeline.json
defConfig :: Pretty.Config
defConfig = Pretty.defConfig {Pretty.confIndent = Pretty.Spaces 2}

-- Just outputs the timeline (JSON list of object).
main :: IO ()
main = do
  -- Two `FilePath`s that are part of the project.
  -- These are "epoch_param" and "cost_model" db-sync tables exported as JSON.
  epochParamFilePath <- Paths.getDataFileName "data/db-sync/epoch_param.json"
  costModelFilePath  <- Paths.getDataFileName "data/db-sync/cost_model.json"
  -- Decode both directly as a list of Aeson's KeyMap (JSON object internals).
  epochParamsAns <- Aeson.eitherDecodeFileStrict epochParamFilePath
  costModelsAns <- Aeson.eitherDecodeFileStrict costModelFilePath
  let epochParams = case (epochParamsAns :: Either String [KeyMap.KeyMap Aeson.Value]) of
                      (Left e) -> error e
                      (Right ans) -> ans
  let costModels = case (costModelsAns :: Either String [KeyMap.KeyMap Aeson.Value]) of
                     (Left e) -> error e
                     (Right ans) -> ans
  -- Construct a list of "epoch_param" rows (as they come from db-sync database)
  -- that introduce noteworthy changes compared to the last epoch with changes.
  let epochParamsChanges = List.foldl'
        (\acc ep ->
          case acc of
            -- First epoch, nothing to compare!
            [] -> [ep]
            -- Epoch n+1.
            _ ->  let ep' = last acc -- The last epoch "accumulated".
                      -- The difference ignoring the fields that always change.
                      minusEp = minus
                                  ["block_id", "epoch_no", "id", "nonce"]
                                  ep
                                  ep'
                  in if KeyMap.null minusEp
                     then acc -- No differences, nothing to do.
                     else acc ++ [ep] -- The full epoch for later treatment.
        )
        []
        epochParams
  -- Pretty print as a timeline (forward changes).
  TIO.putStrLn $ TE.decodeUtf8 $ Pretty.encodePretty' defConfig $ snd $ List.mapAccumL
    (\acc ep ->
      case acc of
        [] -> ( [ep], ep ) -- First epoch unchanged!
        (ep':_) -> let minusEp = minus ["id", "block_id", "nonce"] ep ep'
                   in ([ep] -- Pass this epoch unchanged to calculate next diff.
                      , case (getCosts costModels ep', getCosts costModels minusEp) of
                          -- Actual epoch introduces no cost model changes.
                          (_         , Nothing    ) -> minusEp
                          -- Last epoch had no cost model.
                          (Nothing   , Just costs ) -> minusEp <> addCostsNames costs
                          -- Found, at least by IDs, two different cost model.
                          -- Append the difference.
                          (Just costs', Just costs) ->
                            minusEp
                            <>
                            KeyMap.unionWithKey
                              (\_ v' v ->
                                -- Key is "PlutusV1", "PlutusV2" and "PlutusV3".
                                -- So if an object only append the differences.
                                -- If array, do nothing!
                                case (v', v) of
                                  (Aeson.Object kv', Aeson.Object kv) -> Aeson.Object $ minus [] kv kv'
                                  _ -> v
                              )
                              (addCostsNames costs')
                              (addCostsNames costs)
                      )
    )
    []
    epochParamsChanges

-- Aeson.KeyMap helpers.
-- Not using proper data types and Aeson instances because all parameters names
-- (or cost model names if not using an array of numbers) are tricky to
-- maintain. We use maps and folds instead!
-- Future improvement with lenses ???
--------------------------------------------------------------------------------

-- Returns the first `KeyMap` without the keys in the exclude list and without
-- the keys that have the same value as the second `KeyMap`.
-- Works only at the top level.
minus :: [KeyMap.Key]
      -> KeyMap.KeyMap Aeson.Value
      -> KeyMap.KeyMap Aeson.Value
      -> KeyMap.KeyMap Aeson.Value
minus exclude km1 km2 =
  KeyMap.filterWithKey
    (\k v -> not $
         elem k exclude
      || (Just v == KeyMap.lookup k km2)
    )
    km1

-- Get the "costs" object of "cost model" referenced by "epoch_param".
getCosts :: [KeyMap.KeyMap Aeson.Value]
         -> KeyMap.KeyMap Aeson.Value
         -> Maybe (KeyMap.KeyMap Aeson.Value)
getCosts costModels epochParam =
  -- If this "epoch_param" row has an ID that references a "cost_model" row.
  if    KeyMap.member "cost_model_id" epochParam
     && KeyMap.lookup "cost_model_id" epochParam /= Just Aeson.Null
  then
    let
        -- The "cost_model_id" for the epoch.
        mCostModelId = KeyMap.lookup "cost_model_id" epochParam -- `Maybe`.
        -- A cost model with that ID must exist (db-sync schema)!
        costModel =
          let matchingCostModels = filter
                (\cm -> KeyMap.lookup "id" cm == mCostModelId) -- Both `Maybe`s.
                costModels
          in case matchingCostModels of
               [] -> error $ "No cost model with \"id\" = " ++ show mCostModelId
               [cm] -> cm
               _ -> error $ "Multiple cost models with \"id\" = " ++ show mCostModelId
          -- The "costs" property with a JSON Object must exist (db-sync schema)!
    in  case KeyMap.lookup "costs" costModel of
          (Just (Aeson.Object c)) -> Just c
          _ -> error "Cost model \"costs\" property not a JSON Object"
  else Nothing

addCostsNames :: KeyMap.KeyMap Aeson.Value -> KeyMap.KeyMap Aeson.Value
addCostsNames =
  KeyMap.mapWithKey
    (\k v ->
      case (k,v) of
        ("PlutusV1", Aeson.Array v1s) ->
          Aeson.object $ zip (map fromString plutusV1) (toList v1s)
        ("PlutusV2", Aeson.Array v2s) ->
          Aeson.object $ zip (map fromString plutusV2) (toList v2s)
        {-- TODO: Is this OK ?
        ("PlutusV3", Aeson.Array v3s) ->
          Aeson.object $ zip (map fromString _plutusV3) (toList v3s)
        --}
        _ -> v
    )

-- The cost models cost names, the order is important for Plutus V3.
--------------------------------------------------------------------------------

plutusV1 :: [String]
plutusV1 =
  [
    "addInteger-cpu-arguments-intercept"
  , "addInteger-cpu-arguments-slope"
  , "addInteger-memory-arguments-intercept"
  , "addInteger-memory-arguments-slope"
  , "appendByteString-cpu-arguments-intercept"
  , "appendByteString-cpu-arguments-slope"
  , "appendByteString-memory-arguments-intercept"
  , "appendByteString-memory-arguments-slope"
  , "appendString-cpu-arguments-intercept"
  , "appendString-cpu-arguments-slope"
  , "appendString-memory-arguments-intercept"
  , "appendString-memory-arguments-slope"
  , "bData-cpu-arguments"
  , "bData-memory-arguments"
  , "blake2b-cpu-arguments-intercept"
  , "blake2b-cpu-arguments-slope"
  , "blake2b-memory-arguments"
  , "cekApplyCost-exBudgetCPU"
  , "cekApplyCost-exBudgetMemory"
  , "cekBuiltinCost-exBudgetCPU"
  , "cekBuiltinCost-exBudgetMemory"
  , "cekConstCost-exBudgetCPU"
  , "cekConstCost-exBudgetMemory"
  , "cekDelayCost-exBudgetCPU"
  , "cekDelayCost-exBudgetMemory"
  , "cekForceCost-exBudgetCPU"
  , "cekForceCost-exBudgetMemory"
  , "cekLamCost-exBudgetCPU"
  , "cekLamCost-exBudgetMemory"
  , "cekStartupCost-exBudgetCPU"
  , "cekStartupCost-exBudgetMemory"
  , "cekVarCost-exBudgetCPU"
  , "cekVarCost-exBudgetMemory"
  , "chooseData-cpu-arguments"
  , "chooseData-memory-arguments"
  , "chooseList-cpu-arguments"
  , "chooseList-memory-arguments"
  , "chooseUnit-cpu-arguments"
  , "chooseUnit-memory-arguments"
  , "consByteString-cpu-arguments-intercept"
  , "consByteString-cpu-arguments-slope"
  , "consByteString-memory-arguments-intercept"
  , "consByteString-memory-arguments-slope"
  , "constrData-cpu-arguments"
  , "constrData-memory-arguments"
  , "decodeUtf8-cpu-arguments-intercept"
  , "decodeUtf8-cpu-arguments-slope"
  , "decodeUtf8-memory-arguments-intercept"
  , "decodeUtf8-memory-arguments-slope"
  , "divideInteger-cpu-arguments-constant"
  , "divideInteger-cpu-arguments-model-arguments-intercept"
  , "divideInteger-cpu-arguments-model-arguments-slope"
  , "divideInteger-memory-arguments-intercept"
  , "divideInteger-memory-arguments-minimum"
  , "divideInteger-memory-arguments-slope"
  , "encodeUtf8-cpu-arguments-intercept"
  , "encodeUtf8-cpu-arguments-slope"
  , "encodeUtf8-memory-arguments-intercept"
  , "encodeUtf8-memory-arguments-slope"
  , "equalsByteString-cpu-arguments-constant"
  , "equalsByteString-cpu-arguments-intercept"
  , "equalsByteString-cpu-arguments-slope"
  , "equalsByteString-memory-arguments"
  , "equalsData-cpu-arguments-intercept"
  , "equalsData-cpu-arguments-slope"
  , "equalsData-memory-arguments"
  , "equalsInteger-cpu-arguments-intercept"
  , "equalsInteger-cpu-arguments-slope"
  , "equalsInteger-memory-arguments"
  , "equalsString-cpu-arguments-constant"
  , "equalsString-cpu-arguments-intercept"
  , "equalsString-cpu-arguments-slope"
  , "equalsString-memory-arguments"
  , "fstPair-cpu-arguments"
  , "fstPair-memory-arguments"
  , "headList-cpu-arguments"
  , "headList-memory-arguments"
  , "iData-cpu-arguments"
  , "iData-memory-arguments"
  , "ifThenElse-cpu-arguments"
  , "ifThenElse-memory-arguments"
  , "indexByteString-cpu-arguments"
  , "indexByteString-memory-arguments"
  , "lengthOfByteString-cpu-arguments"
  , "lengthOfByteString-memory-arguments"
  , "lessThanByteString-cpu-arguments-intercept"
  , "lessThanByteString-cpu-arguments-slope"
  , "lessThanByteString-memory-arguments"
  , "lessThanEqualsByteString-cpu-arguments-intercept"
  , "lessThanEqualsByteString-cpu-arguments-slope"
  , "lessThanEqualsByteString-memory-arguments"
  , "lessThanEqualsInteger-cpu-arguments-intercept"
  , "lessThanEqualsInteger-cpu-arguments-slope"
  , "lessThanEqualsInteger-memory-arguments"
  , "lessThanInteger-cpu-arguments-intercept"
  , "lessThanInteger-cpu-arguments-slope"
  , "lessThanInteger-memory-arguments"
  , "listData-cpu-arguments"
  , "listData-memory-arguments"
  , "mapData-cpu-arguments"
  , "mapData-memory-arguments"
  , "mkCons-cpu-arguments"
  , "mkCons-memory-arguments"
  , "mkNilData-cpu-arguments"
  , "mkNilData-memory-arguments"
  , "mkNilPairData-cpu-arguments"
  , "mkNilPairData-memory-arguments"
  , "mkPairData-cpu-arguments"
  , "mkPairData-memory-arguments"
  , "modInteger-cpu-arguments-constant"
  , "modInteger-cpu-arguments-model-arguments-intercept"
  , "modInteger-cpu-arguments-model-arguments-slope"
  , "modInteger-memory-arguments-intercept"
  , "modInteger-memory-arguments-minimum"
  , "modInteger-memory-arguments-slope"
  , "multiplyInteger-cpu-arguments-intercept"
  , "multiplyInteger-cpu-arguments-slope"
  , "multiplyInteger-memory-arguments-intercept"
  , "multiplyInteger-memory-arguments-slope"
  , "nullList-cpu-arguments"
  , "nullList-memory-arguments"
  , "quotientInteger-cpu-arguments-constant"
  , "quotientInteger-cpu-arguments-model-arguments-intercept"
  , "quotientInteger-cpu-arguments-model-arguments-slope"
  , "quotientInteger-memory-arguments-intercept"
  , "quotientInteger-memory-arguments-minimum"
  , "quotientInteger-memory-arguments-slope"
  , "remainderInteger-cpu-arguments-constant"
  , "remainderInteger-cpu-arguments-model-arguments-intercept"
  , "remainderInteger-cpu-arguments-model-arguments-slope"
  , "remainderInteger-memory-arguments-intercept"
  , "remainderInteger-memory-arguments-minimum"
  , "remainderInteger-memory-arguments-slope"
  , "sha2_256-cpu-arguments-intercept"
  , "sha2_256-cpu-arguments-slope"
  , "sha2_256-memory-arguments"
  , "sha3_256-cpu-arguments-intercept"
  , "sha3_256-cpu-arguments-slope"
  , "sha3_256-memory-arguments"
  , "sliceByteString-cpu-arguments-intercept"
  , "sliceByteString-cpu-arguments-slope"
  , "sliceByteString-memory-arguments-intercept"
  , "sliceByteString-memory-arguments-slope"
  , "sndPair-cpu-arguments"
  , "sndPair-memory-arguments"
  , "subtractInteger-cpu-arguments-intercept"
  , "subtractInteger-cpu-arguments-slope"
  , "subtractInteger-memory-arguments-intercept"
  , "subtractInteger-memory-arguments-slope"
  , "tailList-cpu-arguments"
  , "tailList-memory-arguments"
  , "trace-cpu-arguments"
  , "trace-memory-arguments"
  , "unBData-cpu-arguments"
  , "unBData-memory-arguments"
  , "unConstrData-cpu-arguments"
  , "unConstrData-memory-arguments"
  , "unIData-cpu-arguments"
  , "unIData-memory-arguments"
  , "unListData-cpu-arguments"
  , "unListData-memory-arguments"
  , "unMapData-cpu-arguments"
  , "unMapData-memory-arguments"
  , "verifySignature-cpu-arguments-intercept"
  , "verifySignature-cpu-arguments-slope"
  , "verifySignature-memory-arguments"
  ]
  ++
  repeat (error "PlutusV1 cost with no name")

plutusV2 :: [String]
plutusV2 =
  [
    "addInteger-cpu-arguments-intercept"
  , "addInteger-cpu-arguments-slope"
  , "addInteger-memory-arguments-intercept"
  , "addInteger-memory-arguments-slope"
  , "appendByteString-cpu-arguments-intercept"
  , "appendByteString-cpu-arguments-slope"
  , "appendByteString-memory-arguments-intercept"
  , "appendByteString-memory-arguments-slope"
  , "appendString-cpu-arguments-intercept"
  , "appendString-cpu-arguments-slope"
  , "appendString-memory-arguments-intercept"
  , "appendString-memory-arguments-slope"
  , "bData-cpu-arguments"
  , "bData-memory-arguments"
  , "blake2b_256-cpu-arguments-intercept"
  , "blake2b_256-cpu-arguments-slope"
  , "blake2b_256-memory-arguments"
  , "cekApplyCost-exBudgetCPU"
  , "cekApplyCost-exBudgetMemory"
  , "cekBuiltinCost-exBudgetCPU"
  , "cekBuiltinCost-exBudgetMemory"
  , "cekConstCost-exBudgetCPU"
  , "cekConstCost-exBudgetMemory"
  , "cekDelayCost-exBudgetCPU"
  , "cekDelayCost-exBudgetMemory"
  , "cekForceCost-exBudgetCPU"
  , "cekForceCost-exBudgetMemory"
  , "cekLamCost-exBudgetCPU"
  , "cekLamCost-exBudgetMemory"
  , "cekStartupCost-exBudgetCPU"
  , "cekStartupCost-exBudgetMemory"
  , "cekVarCost-exBudgetCPU"
  , "cekVarCost-exBudgetMemory"
  , "chooseData-cpu-arguments"
  , "chooseData-memory-arguments"
  , "chooseList-cpu-arguments"
  , "chooseList-memory-arguments"
  , "chooseUnit-cpu-arguments"
  , "chooseUnit-memory-arguments"
  , "consByteString-cpu-arguments-intercept"
  , "consByteString-cpu-arguments-slope"
  , "consByteString-memory-arguments-intercept"
  , "consByteString-memory-arguments-slope"
  , "constrData-cpu-arguments"
  , "constrData-memory-arguments"
  , "decodeUtf8-cpu-arguments-intercept"
  , "decodeUtf8-cpu-arguments-slope"
  , "decodeUtf8-memory-arguments-intercept"
  , "decodeUtf8-memory-arguments-slope"
  , "divideInteger-cpu-arguments-constant"
  , "divideInteger-cpu-arguments-model-arguments-intercept"
  , "divideInteger-cpu-arguments-model-arguments-slope"
  , "divideInteger-memory-arguments-intercept"
  , "divideInteger-memory-arguments-minimum"
  , "divideInteger-memory-arguments-slope"
  , "encodeUtf8-cpu-arguments-intercept"
  , "encodeUtf8-cpu-arguments-slope"
  , "encodeUtf8-memory-arguments-intercept"
  , "encodeUtf8-memory-arguments-slope"
  , "equalsByteString-cpu-arguments-constant"
  , "equalsByteString-cpu-arguments-intercept"
  , "equalsByteString-cpu-arguments-slope"
  , "equalsByteString-memory-arguments"
  , "equalsData-cpu-arguments-intercept"
  , "equalsData-cpu-arguments-slope"
  , "equalsData-memory-arguments"
  , "equalsInteger-cpu-arguments-intercept"
  , "equalsInteger-cpu-arguments-slope"
  , "equalsInteger-memory-arguments"
  , "equalsString-cpu-arguments-constant"
  , "equalsString-cpu-arguments-intercept"
  , "equalsString-cpu-arguments-slope"
  , "equalsString-memory-arguments"
  , "fstPair-cpu-arguments"
  , "fstPair-memory-arguments"
  , "headList-cpu-arguments"
  , "headList-memory-arguments"
  , "iData-cpu-arguments"
  , "iData-memory-arguments"
  , "ifThenElse-cpu-arguments"
  , "ifThenElse-memory-arguments"
  , "indexByteString-cpu-arguments"
  , "indexByteString-memory-arguments"
  , "lengthOfByteString-cpu-arguments"
  , "lengthOfByteString-memory-arguments"
  , "lessThanByteString-cpu-arguments-intercept"
  , "lessThanByteString-cpu-arguments-slope"
  , "lessThanByteString-memory-arguments"
  , "lessThanEqualsByteString-cpu-arguments-intercept"
  , "lessThanEqualsByteString-cpu-arguments-slope"
  , "lessThanEqualsByteString-memory-arguments"
  , "lessThanEqualsInteger-cpu-arguments-intercept"
  , "lessThanEqualsInteger-cpu-arguments-slope"
  , "lessThanEqualsInteger-memory-arguments"
  , "lessThanInteger-cpu-arguments-intercept"
  , "lessThanInteger-cpu-arguments-slope"
  , "lessThanInteger-memory-arguments"
  , "listData-cpu-arguments"
  , "listData-memory-arguments"
  , "mapData-cpu-arguments"
  , "mapData-memory-arguments"
  , "mkCons-cpu-arguments"
  , "mkCons-memory-arguments"
  , "mkNilData-cpu-arguments"
  , "mkNilData-memory-arguments"
  , "mkNilPairData-cpu-arguments"
  , "mkNilPairData-memory-arguments"
  , "mkPairData-cpu-arguments"
  , "mkPairData-memory-arguments"
  , "modInteger-cpu-arguments-constant"
  , "modInteger-cpu-arguments-model-arguments-intercept"
  , "modInteger-cpu-arguments-model-arguments-slope"
  , "modInteger-memory-arguments-intercept"
  , "modInteger-memory-arguments-minimum"
  , "modInteger-memory-arguments-slope"
  , "multiplyInteger-cpu-arguments-intercept"
  , "multiplyInteger-cpu-arguments-slope"
  , "multiplyInteger-memory-arguments-intercept"
  , "multiplyInteger-memory-arguments-slope"
  , "nullList-cpu-arguments"
  , "nullList-memory-arguments"
  , "quotientInteger-cpu-arguments-constant"
  , "quotientInteger-cpu-arguments-model-arguments-intercept"
  , "quotientInteger-cpu-arguments-model-arguments-slope"
  , "quotientInteger-memory-arguments-intercept"
  , "quotientInteger-memory-arguments-minimum"
  , "quotientInteger-memory-arguments-slope"
  , "remainderInteger-cpu-arguments-constant"
  , "remainderInteger-cpu-arguments-model-arguments-intercept"
  , "remainderInteger-cpu-arguments-model-arguments-slope"
  , "remainderInteger-memory-arguments-intercept"
  , "remainderInteger-memory-arguments-minimum"
  , "remainderInteger-memory-arguments-slope"
  , "serialiseData-cpu-arguments-intercept"
  , "serialiseData-cpu-arguments-slope"
  , "serialiseData-memory-arguments-intercept"
  , "serialiseData-memory-arguments-slope"
  , "sha2_256-cpu-arguments-intercept"
  , "sha2_256-cpu-arguments-slope"
  , "sha2_256-memory-arguments"
  , "sha3_256-cpu-arguments-intercept"
  , "sha3_256-cpu-arguments-slope"
  , "sha3_256-memory-arguments"
  , "sliceByteString-cpu-arguments-intercept"
  , "sliceByteString-cpu-arguments-slope"
  , "sliceByteString-memory-arguments-intercept"
  , "sliceByteString-memory-arguments-slope"
  , "sndPair-cpu-arguments"
  , "sndPair-memory-arguments"
  , "subtractInteger-cpu-arguments-intercept"
  , "subtractInteger-cpu-arguments-slope"
  , "subtractInteger-memory-arguments-intercept"
  , "subtractInteger-memory-arguments-slope"
  , "tailList-cpu-arguments"
  , "tailList-memory-arguments"
  , "trace-cpu-arguments"
  , "trace-memory-arguments"
  , "unBData-cpu-arguments"
  , "unBData-memory-arguments"
  , "unConstrData-cpu-arguments"
  , "unConstrData-memory-arguments"
  , "unIData-cpu-arguments"
  , "unIData-memory-arguments"
  , "unListData-cpu-arguments"
  , "unListData-memory-arguments"
  , "unMapData-cpu-arguments"
  , "unMapData-memory-arguments"
  , "verifyEcdsaSecp256k1Signature-cpu-arguments"
  , "verifyEcdsaSecp256k1Signature-memory-arguments"
  , "verifyEd25519Signature-cpu-arguments-intercept"
  , "verifyEd25519Signature-cpu-arguments-slope"
  , "verifyEd25519Signature-memory-arguments"
  , "verifySchnorrSecp256k1Signature-cpu-arguments-intercept"
  , "verifySchnorrSecp256k1Signature-cpu-arguments-slope"
  , "verifySchnorrSecp256k1Signature-memory-arguments"
  ]
  ++
  repeat (error "PlutusV2 cost with no name")

-- https://github.com/IntersectMBO/plutus/blob/3400c4ca44f86b1c3ee135ceb6353b659109172c/plutus-ledger-api/CostModel/Params/CostModelParams/costModelParamNames.txt.golden#L242
_plutusV3 :: [String]
_plutusV3 =
  [
    "addInteger-cpu-arguments-intercept"
  , "addInteger-cpu-arguments-slope"
  , "addInteger-memory-arguments-intercept"
  , "addInteger-memory-arguments-slope"
  , "appendByteString-cpu-arguments-intercept"
  , "appendByteString-cpu-arguments-slope"
  , "appendByteString-memory-arguments-intercept"
  , "appendByteString-memory-arguments-slope"
  , "appendString-cpu-arguments-intercept"
  , "appendString-cpu-arguments-slope"
  , "appendString-memory-arguments-intercept"
  , "appendString-memory-arguments-slope"
  , "bData-cpu-arguments"
  , "bData-memory-arguments"
  , "blake2b_256-cpu-arguments-intercept"
  , "blake2b_256-cpu-arguments-slope"
  , "blake2b_256-memory-arguments"
  , "cekApplyCost-exBudgetCPU"
  , "cekApplyCost-exBudgetMemory"
  , "cekBuiltinCost-exBudgetCPU"
  , "cekBuiltinCost-exBudgetMemory"
  , "cekConstCost-exBudgetCPU"
  , "cekConstCost-exBudgetMemory"
  , "cekDelayCost-exBudgetCPU"
  , "cekDelayCost-exBudgetMemory"
  , "cekForceCost-exBudgetCPU"
  , "cekForceCost-exBudgetMemory"
  , "cekLamCost-exBudgetCPU"
  , "cekLamCost-exBudgetMemory"
  , "cekStartupCost-exBudgetCPU"
  , "cekStartupCost-exBudgetMemory"
  , "cekVarCost-exBudgetCPU"
  , "cekVarCost-exBudgetMemory"
  , "chooseData-cpu-arguments"
  , "chooseData-memory-arguments"
  , "chooseList-cpu-arguments"
  , "chooseList-memory-arguments"
  , "chooseUnit-cpu-arguments"
  , "chooseUnit-memory-arguments"
  , "consByteString-cpu-arguments-intercept"
  , "consByteString-cpu-arguments-slope"
  , "consByteString-memory-arguments-intercept"
  , "consByteString-memory-arguments-slope"
  , "constrData-cpu-arguments"
  , "constrData-memory-arguments"
  , "decodeUtf8-cpu-arguments-intercept"
  , "decodeUtf8-cpu-arguments-slope"
  , "decodeUtf8-memory-arguments-intercept"
  , "decodeUtf8-memory-arguments-slope"
  , "divideInteger-cpu-arguments-constant"
  , "divideInteger-cpu-arguments-model-arguments-c00"
  , "divideInteger-cpu-arguments-model-arguments-c01"
  , "divideInteger-cpu-arguments-model-arguments-c02"
  , "divideInteger-cpu-arguments-model-arguments-c10"
  , "divideInteger-cpu-arguments-model-arguments-c11"
  , "divideInteger-cpu-arguments-model-arguments-c20"
  , "divideInteger-cpu-arguments-model-arguments-minimum"
  , "divideInteger-memory-arguments-intercept"
  , "divideInteger-memory-arguments-minimum"
  , "divideInteger-memory-arguments-slope"
  , "encodeUtf8-cpu-arguments-intercept"
  , "encodeUtf8-cpu-arguments-slope"
  , "encodeUtf8-memory-arguments-intercept"
  , "encodeUtf8-memory-arguments-slope"
  , "equalsByteString-cpu-arguments-constant"
  , "equalsByteString-cpu-arguments-intercept"
  , "equalsByteString-cpu-arguments-slope"
  , "equalsByteString-memory-arguments"
  , "equalsData-cpu-arguments-intercept"
  , "equalsData-cpu-arguments-slope"
  , "equalsData-memory-arguments"
  , "equalsInteger-cpu-arguments-intercept"
  , "equalsInteger-cpu-arguments-slope"
  , "equalsInteger-memory-arguments"
  , "equalsString-cpu-arguments-constant"
  , "equalsString-cpu-arguments-intercept"
  , "equalsString-cpu-arguments-slope"
  , "equalsString-memory-arguments"
  , "fstPair-cpu-arguments"
  , "fstPair-memory-arguments"
  , "headList-cpu-arguments"
  , "headList-memory-arguments"
  , "iData-cpu-arguments"
  , "iData-memory-arguments"
  , "ifThenElse-cpu-arguments"
  , "ifThenElse-memory-arguments"
  , "indexByteString-cpu-arguments"
  , "indexByteString-memory-arguments"
  , "lengthOfByteString-cpu-arguments"
  , "lengthOfByteString-memory-arguments"
  , "lessThanByteString-cpu-arguments-intercept"
  , "lessThanByteString-cpu-arguments-slope"
  , "lessThanByteString-memory-arguments"
  , "lessThanEqualsByteString-cpu-arguments-intercept"
  , "lessThanEqualsByteString-cpu-arguments-slope"
  , "lessThanEqualsByteString-memory-arguments"
  , "lessThanEqualsInteger-cpu-arguments-intercept"
  , "lessThanEqualsInteger-cpu-arguments-slope"
  , "lessThanEqualsInteger-memory-arguments"
  , "lessThanInteger-cpu-arguments-intercept"
  , "lessThanInteger-cpu-arguments-slope"
  , "lessThanInteger-memory-arguments"
  , "listData-cpu-arguments"
  , "listData-memory-arguments"
  , "mapData-cpu-arguments"
  , "mapData-memory-arguments"
  , "mkCons-cpu-arguments"
  , "mkCons-memory-arguments"
  , "mkNilData-cpu-arguments"
  , "mkNilData-memory-arguments"
  , "mkNilPairData-cpu-arguments"
  , "mkNilPairData-memory-arguments"
  , "mkPairData-cpu-arguments"
  , "mkPairData-memory-arguments"
  , "modInteger-cpu-arguments-constant"
  , "modInteger-cpu-arguments-model-arguments-c00"
  , "modInteger-cpu-arguments-model-arguments-c01"
  , "modInteger-cpu-arguments-model-arguments-c02"
  , "modInteger-cpu-arguments-model-arguments-c10"
  , "modInteger-cpu-arguments-model-arguments-c11"
  , "modInteger-cpu-arguments-model-arguments-c20"
  , "modInteger-cpu-arguments-model-arguments-minimum"
  , "modInteger-memory-arguments-intercept"
  , "modInteger-memory-arguments-slope"
  , "multiplyInteger-cpu-arguments-intercept"
  , "multiplyInteger-cpu-arguments-slope"
  , "multiplyInteger-memory-arguments-intercept"
  , "multiplyInteger-memory-arguments-slope"
  , "nullList-cpu-arguments"
  , "nullList-memory-arguments"
  , "quotientInteger-cpu-arguments-constant"
  , "quotientInteger-cpu-arguments-model-arguments-c00"
  , "quotientInteger-cpu-arguments-model-arguments-c01"
  , "quotientInteger-cpu-arguments-model-arguments-c02"
  , "quotientInteger-cpu-arguments-model-arguments-c10"
  , "quotientInteger-cpu-arguments-model-arguments-c11"
  , "quotientInteger-cpu-arguments-model-arguments-c20"
  , "quotientInteger-cpu-arguments-model-arguments-minimum"
  , "quotientInteger-memory-arguments-intercept"
  , "quotientInteger-memory-arguments-minimum"
  , "quotientInteger-memory-arguments-slope"
  , "remainderInteger-cpu-arguments-constant"
  , "remainderInteger-cpu-arguments-model-arguments-c00"
  , "remainderInteger-cpu-arguments-model-arguments-c01"
  , "remainderInteger-cpu-arguments-model-arguments-c02"
  , "remainderInteger-cpu-arguments-model-arguments-c10"
  , "remainderInteger-cpu-arguments-model-arguments-c11"
  , "remainderInteger-cpu-arguments-model-arguments-c20"
  , "remainderInteger-cpu-arguments-model-arguments-minimum"
  , "remainderInteger-memory-arguments-intercept"
  , "remainderInteger-memory-arguments-slope"
  , "serialiseData-cpu-arguments-intercept"
  , "serialiseData-cpu-arguments-slope"
  , "serialiseData-memory-arguments-intercept"
  , "serialiseData-memory-arguments-slope"
  , "sha2_256-cpu-arguments-intercept"
  , "sha2_256-cpu-arguments-slope"
  , "sha2_256-memory-arguments"
  , "sha3_256-cpu-arguments-intercept"
  , "sha3_256-cpu-arguments-slope"
  , "sha3_256-memory-arguments"
  , "sliceByteString-cpu-arguments-intercept"
  , "sliceByteString-cpu-arguments-slope"
  , "sliceByteString-memory-arguments-intercept"
  , "sliceByteString-memory-arguments-slope"
  , "sndPair-cpu-arguments"
  , "sndPair-memory-arguments"
  , "subtractInteger-cpu-arguments-intercept"
  , "subtractInteger-cpu-arguments-slope"
  , "subtractInteger-memory-arguments-intercept"
  , "subtractInteger-memory-arguments-slope"
  , "tailList-cpu-arguments"
  , "tailList-memory-arguments"
  , "trace-cpu-arguments"
  , "trace-memory-arguments"
  , "unBData-cpu-arguments"
  , "unBData-memory-arguments"
  , "unConstrData-cpu-arguments"
  , "unConstrData-memory-arguments"
  , "unIData-cpu-arguments"
  , "unIData-memory-arguments"
  , "unListData-cpu-arguments"
  , "unListData-memory-arguments"
  , "unMapData-cpu-arguments"
  , "unMapData-memory-arguments"
  , "verifyEcdsaSecp256k1Signature-cpu-arguments"
  , "verifyEcdsaSecp256k1Signature-memory-arguments"
  , "verifyEd25519Signature-cpu-arguments-intercept"
  , "verifyEd25519Signature-cpu-arguments-slope"
  , "verifyEd25519Signature-memory-arguments"
  , "verifySchnorrSecp256k1Signature-cpu-arguments-intercept"
  , "verifySchnorrSecp256k1Signature-cpu-arguments-slope"
  , "verifySchnorrSecp256k1Signature-memory-arguments"
  , "cekConstrCost-exBudgetCPU"
  , "cekConstrCost-exBudgetMemory"
  , "cekCaseCost-exBudgetCPU"
  , "cekCaseCost-exBudgetMemory"
  , "bls12_381_G1_add-cpu-arguments"
  , "bls12_381_G1_add-memory-arguments"
  , "bls12_381_G1_compress-cpu-arguments"
  , "bls12_381_G1_compress-memory-arguments"
  , "bls12_381_G1_equal-cpu-arguments"
  , "bls12_381_G1_equal-memory-arguments"
  , "bls12_381_G1_hashToGroup-cpu-arguments-intercept"
  , "bls12_381_G1_hashToGroup-cpu-arguments-slope"
  , "bls12_381_G1_hashToGroup-memory-arguments"
  , "bls12_381_G1_neg-cpu-arguments"
  , "bls12_381_G1_neg-memory-arguments"
  , "bls12_381_G1_scalarMul-cpu-arguments-intercept"
  , "bls12_381_G1_scalarMul-cpu-arguments-slope"
  , "bls12_381_G1_scalarMul-memory-arguments"
  , "bls12_381_G1_uncompress-cpu-arguments"
  , "bls12_381_G1_uncompress-memory-arguments"
  , "bls12_381_G2_add-cpu-arguments"
  , "bls12_381_G2_add-memory-arguments"
  , "bls12_381_G2_compress-cpu-arguments"
  , "bls12_381_G2_compress-memory-arguments"
  , "bls12_381_G2_equal-cpu-arguments"
  , "bls12_381_G2_equal-memory-arguments"
  , "bls12_381_G2_hashToGroup-cpu-arguments-intercept"
  , "bls12_381_G2_hashToGroup-cpu-arguments-slope"
  , "bls12_381_G2_hashToGroup-memory-arguments"
  , "bls12_381_G2_neg-cpu-arguments"
  , "bls12_381_G2_neg-memory-arguments"
  , "bls12_381_G2_scalarMul-cpu-arguments-intercept"
  , "bls12_381_G2_scalarMul-cpu-arguments-slope"
  , "bls12_381_G2_scalarMul-memory-arguments"
  , "bls12_381_G2_uncompress-cpu-arguments"
  , "bls12_381_G2_uncompress-memory-arguments"
  , "bls12_381_finalVerify-cpu-arguments"
  , "bls12_381_finalVerify-memory-arguments"
  , "bls12_381_millerLoop-cpu-arguments"
  , "bls12_381_millerLoop-memory-arguments"
  , "bls12_381_mulMlResult-cpu-arguments"
  , "bls12_381_mulMlResult-memory-arguments"
  , "keccak_256-cpu-arguments-intercept"
  , "keccak_256-cpu-arguments-slope"
  , "keccak_256-memory-arguments"
  , "blake2b_224-cpu-arguments-intercept"
  , "blake2b_224-cpu-arguments-slope"
  , "blake2b_224-memory-arguments"
  , "integerToByteString-cpu-arguments-c0"
  , "integerToByteString-cpu-arguments-c1"
  , "integerToByteString-cpu-arguments-c2"
  , "integerToByteString-memory-arguments-intercept"
  , "integerToByteString-memory-arguments-slope"
  , "byteStringToInteger-cpu-arguments-c0"
  , "byteStringToInteger-cpu-arguments-c1"
  , "byteStringToInteger-cpu-arguments-c2"
  , "byteStringToInteger-memory-arguments-intercept"
  , "byteStringToInteger-memory-arguments-slope"
  , "andByteString-cpu-arguments-intercept"
  , "andByteString-cpu-arguments-slope1"
  , "andByteString-cpu-arguments-slope2"
  , "andByteString-memory-arguments-intercept"
  , "andByteString-memory-arguments-slope"
  , "orByteString-cpu-arguments-intercept"
  , "orByteString-cpu-arguments-slope1"
  , "orByteString-cpu-arguments-slope2"
  , "orByteString-memory-arguments-intercept"
  , "orByteString-memory-arguments-slope"
  , "xorByteString-cpu-arguments-intercept"
  , "xorByteString-cpu-arguments-slope1"
  , "xorByteString-cpu-arguments-slope2"
  , "xorByteString-memory-arguments-intercept"
  , "xorByteString-memory-arguments-slope"
  , "complementByteString-cpu-arguments-intercept"
  , "complementByteString-cpu-arguments-slope"
  , "complementByteString-memory-arguments-intercept"
  , "complementByteString-memory-arguments-slope"
  , "readBit-cpu-arguments"
  , "readBit-memory-arguments"
  , "writeBits-cpu-arguments-intercept"
  , "writeBits-cpu-arguments-slope"
  , "writeBits-memory-arguments-intercept"
  , "writeBits-memory-arguments-slope"
  , "replicateByte-cpu-arguments-intercept"
  , "replicateByte-cpu-arguments-slope"
  , "replicateByte-memory-arguments-intercept"
  , "replicateByte-memory-arguments-slope"
  , "shiftByteString-cpu-arguments-intercept"
  , "shiftByteString-cpu-arguments-slope"
  , "shiftByteString-memory-arguments-intercept"
  , "shiftByteString-memory-arguments-slope"
  , "rotateByteString-cpu-arguments-intercept"
  , "rotateByteString-cpu-arguments-slope"
  , "rotateByteString-memory-arguments-intercept"
  , "rotateByteString-memory-arguments-slope"
  , "countSetBits-cpu-arguments-intercept"
  , "countSetBits-cpu-arguments-slope"
  , "countSetBits-memory-arguments"
  , "findFirstSetBit-cpu-arguments-intercept"
  , "findFirstSetBit-cpu-arguments-slope"
  , "findFirstSetBit-memory-arguments"
  , "ripemd_160-cpu-arguments-intercept"
  , "ripemd_160-cpu-arguments-slope"
  , "ripemd_160-memory-arguments"
  ]
  ++
  zipWith (++) (repeat "PlutusV3_Unknown_") (map show ([1,2..]::[Int]))
