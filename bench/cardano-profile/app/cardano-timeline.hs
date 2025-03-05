{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude

import Data.Foldable (toList)
import Data.List (foldl', mapAccumL)
import Data.String (fromString)

-- Package: aeson.
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.Aeson.KeyMap as KeyMap
-- Package: text.
import qualified Data.Text.Lazy.Encoding as Encoding
import qualified Data.Text.Lazy.IO as TIO
-- Package: self.
import qualified Paths_cardano_profile as Paths

-- Returns the first `KeyMap` without the keys in the exclude list and without
-- the keys that have the same value as the second `KeyMap`.
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

-- Look for the cost model by ID and append the "costs" field properties.
appendCostModel :: [KeyMap.KeyMap Aeson.Value]
                -> KeyMap.KeyMap Aeson.Value
                -> KeyMap.KeyMap Aeson.Value
appendCostModel costModels epochParam =
  if KeyMap.member "cost_model_id" epochParam && KeyMap.lookup "cost_model_id" epochParam /= Just Aeson.Null
  then
    let
        -- The "cost_model_id" for the epoch.
        costModelId = KeyMap.lookup "cost_model_id" epochParam
        -- A cost model with that ID must exist (db-sync schema)!
        costModel = head $ filter
                             (\cm -> KeyMap.lookup "id" cm == costModelId)
                             costModels
        -- The "costs" property with a JSON Object must exist (db-sync schema)!
        costModelCosts = case KeyMap.lookup "costs" costModel of
                          (Just (Aeson.Object c)) -> c
                          _ -> error (show costModel)
        -- `zip` the JSON array of numbers with the names known by us!
        costModelNamed = KeyMap.mapWithKey
          (\k v ->
            case (k,v) of
              ("PlutusV1", Aeson.Array v1s) -> Aeson.object $ zip (map fromString plutusV1) (toList v1s)
              ("PlutusV2", Aeson.Array v2s) -> Aeson.object $ zip (map fromString plutusV2) (toList v2s)
              -- TODO: The names of the v3 cost models ???
              _ -> v
          )
          costModelCosts
    in     epochParam
        <> minus ["id", "hash"] costModelNamed KeyMap.empty
  else epochParam

main :: IO ()
main = do
  -- Two `FilePath`s.
  epochParamFilePath <- Paths.getDataFileName "data/db-sync/epoch_param.json"
  costModelFilePath  <- Paths.getDataFileName "data/db-sync/cost_model.json"
  -- Decode both as just a list of JSON objects (KeyMap).
  epochParamsAns <- (Aeson.eitherDecodeFileStrict epochParamFilePath :: IO (Either String [KeyMap.KeyMap Aeson.Value]))
  let epochParams = case epochParamsAns of
                      (Left e) -> error e
                      (Right ans) -> ans
  costModelsAns <- (Aeson.eitherDecodeFileStrict costModelFilePath :: IO (Either String [KeyMap.KeyMap Aeson.Value]))
  let costModels = case costModelsAns of
                     (Left e) -> error e
                     (Right ans) -> ans
  -- Construct a list of "epoch_param" rows (as the come from db-sync database)
  -- that introduce noteworthy changes compared to the last epoch.
  let epochParamsChanges = foldl'
        (\acc ep ->
          case acc of
            -- First epoch!
            [] -> [ep]
            -- The others.
            _ ->  let ep' = last acc -- The last epoch "accumulated".
                      -- The difference without the fields that always change.
                      minusEp = minus
                                  ["block_id", "epoch_no", "id", "nonce"]
                                  ep
                                  ep'
                  in if KeyMap.null minusEp
                     then acc -- No differences, nothing to do.
                     else acc ++ [ep]
        )
        []
        epochParams
  -- Pretty print as a timeline (forward changes).
  TIO.putStrLn $ Encoding.decodeUtf8 $ Pretty.encodePretty' Pretty.defConfig $ snd $ mapAccumL
    (\acc ep ->
      case acc of
        [] -> ( [ep], ep )
        (ep':_) -> let minusEp = minus ["id", "block_id", "nonce"] ep ep'
                   in ([ep]
                      , appendCostModel costModels minusEp
                      )
    )
    []
    epochParamsChanges

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
  repeat "Unknown"

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
  repeat "Unknown"

{--

data EpochParam = EpochParam {
    ep_id :: Integer
  , epoch_no :: Integer
  , min_fee_a :: Integer
  , min_fee_b :: Integer
  , max_block_size :: Integer
  , max_tx_size :: Integer
  , max_bh_size :: Integer
  , key_deposit :: Integer
  , pool_deposit :: Integer
  , max_epoch :: Integer
  , optimal_pool_count :: Integer
  , influence :: Integer
  , monetary_expand_rate :: Integer
  , treasury_growth_rate :: Integer
  , decentralisation :: Integer
  , protocol_major :: Integer
  , protocol_minor :: Integer
  , min_utxo_value :: Integer
  , min_pool_cost :: Integer
  , nonce :: Integer
  , cost_model_id :: Integer
  , price_mem :: Integer
  , price_step :: Integer
  , max_tx_ex_mem :: Integer
  , max_tx_ex_steps :: Integer
  , max_block_ex_mem :: Integer
  , max_block_ex_steps :: Integer
  , max_val_size :: Integer
  , collateral_percent :: Integer
  , max_collateral_inputs :: Integer
  , block_id :: Integer
  , extra_entropy :: Integer
  , coins_per_utxo_size :: Integer
  , pvt_motion_no_confidence :: Integer
  , pvt_committee_normal :: Integer
  , pvt_committee_no_confidence :: Integer
  , pvt_hard_fork_initiation :: Integer
  , dvt_motion_no_confidence :: Integer
  , dvt_committee_normal :: Integer
  , dvt_committee_no_confidence :: Integer
  , dvt_update_to_constitution :: Integer
  , dvt_hard_fork_initiation :: Integer
  , dvt_p_p_network_group :: Integer
  , dvt_p_p_economic_group :: Integer
  , dvt_p_p_technical_group :: Integer
  , dvt_p_p_gov_group :: Integer
  , dvt_treasury_withdrawal :: Integer
  , committee_min_size :: Integer
  , committee_max_term_length :: Integer
  , gov_action_lifetime :: Integer
  , gov_action_deposit :: Integer
  , drep_deposit :: Integer
  , drep_activity :: Integer
  , pvtpp_security_group :: Integer
  , min_fee_ref_script_cost_per_byte :: Integer
} deriving (Show, Generic)

instance PSQL.FromRow EpochParam where
  fromRow = EpochParam
              <$> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field
              <*> PSQL.field

data CostModel = CostModel {
    cm_id :: Integer
  , costs :: [Integer]
  , hash :: Text.Text
} deriving (Show, Generic)

instance PSQL.FromRow CostModel where
  fromRow = CostModel
              <$> PSQL.field
              <*> PSQL.field
              <*> PSQL.field

--}
