{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.Genesis
  ( Epoch (..)
  , EpochParams (..), CostModel (..)
  , epochTimeline
  , plutusV1CostNames, plutusV2CostNames, plutusV3CostNames
  ) where

--------------------------------------------------------------------------------

import           Prelude
import           GHC.Generics
-- Package: aeson.
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
-- Package: containers.
import qualified Data.Map as Map
-- Package: self.
import qualified Paths_cardano_profile as Paths

--------------------------------------------------------------------------------

-- The per-epoch accumulator. The same JSON shape is used by two file sets:
--
--   * `data/genesis/epoch-timeline.json`: one entry per epoch, supplying the
--     protocol-parameters and cost-models defaults that hold from that epoch
--     onwards. Folded across all entries <= the profile's `pparamsEpoch` to
--     produce a baseline `Epoch`.
--
--   * `data/genesis/overlays/*.json`: optional tweaks. A profile opts in by
--     listing an overlay's name in its `pparamsOverlays` field;
--     `Profile.shelleyAlonzoConway` parses each one as an `Epoch` and merges it
--     on top of the baseline via `<>`. This is how profiles pull in things like
--     `voting`, `v9-preview`, `blocksize64k`, etc.
--
-- Two sub-objects: `epoch_params` (Shelley/Alonzo/Conway protocol params) and
-- `cost_model` (Plutus V1/V2/V3 named-key cost models). All values are plain
-- `Maybe (KeyMap Value)` and merge via the same `union` helper.
data Epoch = Epoch
  { epoch_params :: EpochParams
  , cost_model   :: CostModel
  }
  deriving (Eq, Show, Generic)

data EpochParams = EpochParams
  { byron    :: Maybe (KeyMap.KeyMap Aeson.Value)
  , shelley  :: Maybe (KeyMap.KeyMap Aeson.Value)
  , alonzo   :: Maybe (KeyMap.KeyMap Aeson.Value)
  , conway   :: Maybe (KeyMap.KeyMap Aeson.Value)
  , dijkstra :: Maybe (KeyMap.KeyMap Aeson.Value)
  }
  deriving (Eq, Show, Generic)

data CostModel = CostModel
  { plutusV1 :: Maybe (KeyMap.KeyMap Aeson.Value)
  , plutusV2 :: Maybe (KeyMap.KeyMap Aeson.Value)
  , plutusV3 :: Maybe (KeyMap.KeyMap Aeson.Value)
  }
  deriving (Eq, Show, Generic)

instance Semigroup EpochParams where
  ep1 <> ep2 = EpochParams
    { byron    = byron    ep1 `union` byron    ep2
    , shelley  = shelley  ep1 `union` shelley  ep2
    , alonzo   = alonzo   ep1 `union` alonzo   ep2
    , conway   = conway   ep1 `union` conway   ep2
    , dijkstra = dijkstra ep1 `union` dijkstra ep2
    }

instance Monoid EpochParams where
  mempty = EpochParams Nothing Nothing Nothing Nothing Nothing
  mappend = (<>)

instance Semigroup CostModel where
  cm1 <> cm2 = CostModel
    { plutusV1 = plutusV1 cm1 `union` plutusV1 cm2
    , plutusV2 = plutusV2 cm1 `union` plutusV2 cm2
    , plutusV3 = plutusV3 cm1 `union` plutusV3 cm2
    }

instance Monoid CostModel where
  mempty = CostModel Nothing Nothing Nothing
  mappend = (<>)

instance Semigroup Epoch where
  e1 <> e2 = Epoch
    { epoch_params = epoch_params e1 <> epoch_params e2
    , cost_model   = cost_model   e1 <> cost_model   e2
    }

instance Monoid Epoch where
  mempty = Epoch mempty mempty
  mappend = (<>)

-- Both fields are optional; missing means `mempty`. Lets us parse:
--   * timeline entries: `{ epoch, description, epoch_params, cost_model }`
--     where `epoch` and `description` are silently ignored.
--   * overlay files:    `{ epoch_params: ..., cost_model: ... }` (one or the
--     other may be absent).
instance Aeson.FromJSON Epoch where
  parseJSON = Aeson.withObject "Epoch" $ \o -> Epoch
    <$> o Aeson..:? "epoch_params" Aeson..!= mempty
    <*> o Aeson..:? "cost_model"   Aeson..!= mempty

instance Aeson.FromJSON EpochParams
instance Aeson.FromJSON CostModel

instance Aeson.ToJSON Epoch
instance Aeson.ToJSON EpochParams
instance Aeson.ToJSON CostModel

--------------------------------------------------------------------------------

-- Collects all properties from epochs lower or equal than the desired one.
epochTimeline :: Integer -> IO Epoch
epochTimeline upToEpochNumber = do
  fp <- Paths.getDataFileName "data/genesis/epoch-timeline.json"
  -- Get the epochs sorted by number.
  -- The key is a string, so we need to use the "epoch" numeric property.
  eitherValue <-
    (  Aeson.eitherDecodeFileStrict fp
    :: IO (Either String (Map.Map Integer Epoch))
    )
  return $ case eitherValue of
    -- If a proper JSON object, collect!
    (Right epochs) -> Map.foldlWithKey'
      (\acc key epochEntry ->
        if key <= upToEpochNumber
        then acc <> epochEntry
        else acc
      )
      mempty
      epochs
    (Left e) -> error $ "\"data/genesis/epoch-timeline.json\": " ++ e

union :: Maybe (KeyMap.KeyMap Aeson.Value)
      -> Maybe (KeyMap.KeyMap Aeson.Value)
      -> Maybe (KeyMap.KeyMap Aeson.Value)
union Nothing Nothing = Nothing
union Nothing (Just a) = Just a
union (Just a) Nothing = Just a
union (Just a) (Just b) = Just $ KeyMap.unionWithKey unionWithKey a b

-- Right-biased merge of both JSON objects at all depths.
unionWithKey :: KeyMap.Key -> Aeson.Value -> Aeson.Value -> Aeson.Value
-- Empty right object wipes the left: a deep merge with `{}` is otherwise a
-- no-op (the right side adds no keys, the left's content is preserved), so
-- there'd be no way for an overlay to express "I want this collection empty"
-- (e.g. `conway.committee.members: {}` to disable the committee).
-- Treat `{}` on the right as a wholesale replacement.
unionWithKey _ (Aeson.Object _) (Aeson.Object b)
  | KeyMap.null b  = Aeson.Object b
-- Tagged-union (Aeson default sum-type) handling: if both sides are objects and
-- both carry a `tag` key whose values differ, the right side is a different
-- constructor. A deep merge would carry stale payload keys from the left
-- constructor into the right one (e.g. merging Nonce {contents = hex} with
-- NeutralNonce {} would yield NeutralNonce {contents = hex}). Replace the whole
-- object instead.
unionWithKey _ (Aeson.Object a) (Aeson.Object b)
  | tagsDiffer a b = Aeson.Object b
  | otherwise      = Aeson.Object $ KeyMap.unionWithKey unionWithKey a b
-- If not an object prefer the right value.
unionWithKey _ _ b = b

tagsDiffer :: KeyMap.KeyMap Aeson.Value -> KeyMap.KeyMap Aeson.Value -> Bool
tagsDiffer a b = case (KeyMap.lookup "tag" a, KeyMap.lookup "tag" b) of
  (Just av, Just bv) -> av /= bv
  _                  -> False

-- Canonical Plutus cost-parameter names, in canonical order. They are the
-- bridge between the named-object and positional-array forms of a cost
-- model. Two consumers, both order-dependent:
--
--   * `app/cardano-timeline.hs::addCostsNames`: zips each db-sync positional
--     cost array with the matching list to recover `{name: value}` JSON
--     objects.
--   * `Profile.genesisCostModels`: walks each list with `take n` to emit the
--     named OBJECT for V1 at `alonzo.costModels.PlutusV1` (mainnet shape) and
--     positional ARRAYS for V3 at `conway.plutusV3CostModel` and for any
--     V1/V2/V3 entries that go to `alonzo.extraConfig.costModels`.
--
-- Reordering silently mis-prices every cost in that version, so don't. New
-- costs added upstream go at the END of the relevant list.
--
-- Each list's tail is `repeat (error "PlutusVN cost with no name")`, so a read
-- past the known names crashes loudly instead of silently aliasing onto the
-- wrong cost parameter, signalling that the list must be extended with the new
-- upstream names.
--------------------------------------------------------------------------------

-- The 6 names at positions 14..16 and 163..165 below differ from those derived
-- from Plutus's `PlutusLedgerApi.V1.ParamName` enum:
--   https://github.com/IntersectMBO/plutus/blob/0c3d72dc04740e2df9b20a39e40e7301cd43e1db/plutus-ledger-api/src/PlutusLedgerApi/V1/ParamName.hs#L17
--
-- cardano-ledger rewrites those 6 Plutus-derived names to the V1 cost-model
-- JSON actually uses before parsing it. See `plutusV1ParamNames`
-- (lines 184..201) in `Cardano.Ledger.Plutus.CostModels`:
--   https://github.com/IntersectMBO/cardano-ledger/blob/1ab6922d1dcd074951c09f5aa3204c124a6931c5/libs/cardano-ledger-core/src/Cardano/Ledger/Plutus/CostModels.hs#L184-L201
-- The 6 rewrites are exactly:
--   blake2b_256-cpu-arguments-intercept            -> blake2b-cpu-arguments-intercept
--   blake2b_256-cpu-arguments-slope                -> blake2b-cpu-arguments-slope
--   blake2b_256-memory-arguments                   -> blake2b-memory-arguments
--   verifyEd25519Signature-cpu-arguments-intercept -> verifySignature-cpu-arguments-intercept
--   verifyEd25519Signature-cpu-arguments-slope     -> verifySignature-cpu-arguments-slope
--   verifyEd25519Signature-memory-arguments        -> verifySignature-memory-arguments
--
-- Here `plutusV1CostNames` uses what the genesis file actually parses as valid.
--
-- See also (history, not needed to understand the above):
--   * Plutus PR #4599 (2022-05-09) renamed Blake2b -> Blake2b_256:
--     https://github.com/IntersectMBO/plutus/pull/4599
--   * Plutus PR #4595 (2022-05-10) renamed VerifySignature -> VerifyEd25519Signature:
--     https://github.com/IntersectMBO/plutus/pull/4595
--   * Plutus PR #5932 (2024-04-29) — same name-conflict pattern: V1
--     keeps legacy spellings, V3/Conway onward uses the new ones:
--     https://github.com/IntersectMBO/plutus/pull/5932
plutusV1CostNames :: [String]
plutusV1CostNames =
  [
  -- Positions 0..165 (166 entries): Alonzo HF (Alonzo era, mainnet epoch 290).
  -- Original V1 init set.
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
  -- Difference with `PlutusLedgerApi.V1.ParamName`:
  , "blake2b-cpu-arguments-intercept"
  , "blake2b-cpu-arguments-slope"
  , "blake2b-memory-arguments"
  --------------------------------------------------
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
  -- Difference with `PlutusLedgerApi.V1.ParamName`:
  , "verifySignature-cpu-arguments-intercept"
  , "verifySignature-cpu-arguments-slope"
  , "verifySignature-memory-arguments"
  --------------------------------------------------
  -- Positions 166..331 (166 entries): van Rossem (enacted on epoch 638).
  -- See governance action:
  -- gov_action1eqhnsdyf3exhp5mqt7sdjtl7xy69wqg8tvg854psns2jt72cra3qqrcnr8r
  -- (https://gov.tools/governance_actions/c82f3834898e4d70d3605fa0d92ffe31345701075b107a54309c1525f9581f62).
  , "serialiseData-cpu-arguments-intercept"
  , "serialiseData-cpu-arguments-slope"
  , "serialiseData-memory-arguments-intercept"
  , "serialiseData-memory-arguments-slope"
  , "verifyEcdsaSecp256k1Signature-cpu-arguments"
  , "verifyEcdsaSecp256k1Signature-memory-arguments"
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
  , "expModInteger-cpu-arguments-coefficient00"
  , "expModInteger-cpu-arguments-coefficient11"
  , "expModInteger-cpu-arguments-coefficient12"
  , "expModInteger-memory-arguments-intercept"
  , "expModInteger-memory-arguments-slope"
  , "dropList-cpu-arguments-intercept"
  , "dropList-cpu-arguments-slope"
  , "dropList-memory-arguments"
  , "lengthOfArray-cpu-arguments"
  , "lengthOfArray-memory-arguments"
  , "listToArray-cpu-arguments-intercept"
  , "listToArray-cpu-arguments-slope"
  , "listToArray-memory-arguments-intercept"
  , "listToArray-memory-arguments-slope"
  , "indexArray-cpu-arguments"
  , "indexArray-memory-arguments"
  , "bls12_381_G1_multiScalarMul-cpu-arguments-intercept"
  , "bls12_381_G1_multiScalarMul-cpu-arguments-slope"
  , "bls12_381_G1_multiScalarMul-memory-arguments"
  , "bls12_381_G2_multiScalarMul-cpu-arguments-intercept"
  , "bls12_381_G2_multiScalarMul-cpu-arguments-slope"
  , "bls12_381_G2_multiScalarMul-memory-arguments"
  , "insertCoin-cpu-arguments-intercept"
  , "insertCoin-cpu-arguments-slope"
  , "insertCoin-memory-arguments-intercept"
  , "insertCoin-memory-arguments-slope"
  , "lookupCoin-cpu-arguments-intercept"
  , "lookupCoin-cpu-arguments-slope"
  , "lookupCoin-memory-arguments"
  , "unionValue-cpu-arguments-c00"
  , "unionValue-cpu-arguments-c10"
  , "unionValue-cpu-arguments-c01"
  , "unionValue-cpu-arguments-c11"
  , "unionValue-memory-arguments-intercept"
  , "unionValue-memory-arguments-slope"
  , "valueContains-cpu-arguments-constant"
  , "valueContains-cpu-arguments-model-arguments-intercept"
  , "valueContains-cpu-arguments-model-arguments-slope1"
  , "valueContains-cpu-arguments-model-arguments-slope2"
  , "valueContains-memory-arguments"
  , "valueData-cpu-arguments-intercept"
  , "valueData-cpu-arguments-slope"
  , "valueData-memory-arguments-intercept"
  , "valueData-memory-arguments-slope"
  , "unValueData-cpu-arguments-c0"
  , "unValueData-cpu-arguments-c1"
  , "unValueData-cpu-arguments-c2"
  , "unValueData-memory-arguments-intercept"
  , "unValueData-memory-arguments-slope"
  , "scaleValue-cpu-arguments-intercept"
  , "scaleValue-cpu-arguments-slope"
  , "scaleValue-memory-arguments-intercept"
  , "scaleValue-memory-arguments-slope"
  ]
  ++
  repeat (error "PlutusV1 cost with no name")

-- Source: Plutus's `PlutusLedgerApi.V2.ParamName` enum,
--   https://github.com/IntersectMBO/plutus/blob/b6e724e9577419e0cddfaf861ce2d77ea7526d09/plutus-ledger-api/src/PlutusLedgerApi/V2/ParamName.hs
plutusV2CostNames :: [String]
plutusV2CostNames =
  [
  -- Positions 0..174 (175 entries): Vasil HF (Babbage era, mainnet epoch 366).
  -- Original V2 init set.
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
  -- Positions 175..331 (157 entries): van Rossem (enacted on epoch 638).
  -- See governance action:
  -- gov_action1eqhnsdyf3exhp5mqt7sdjtl7xy69wqg8tvg854psns2jt72cra3qqrcnr8r
  -- (https://gov.tools/governance_actions/c82f3834898e4d70d3605fa0d92ffe31345701075b107a54309c1525f9581f62).
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
  , "expModInteger-cpu-arguments-coefficient00"
  , "expModInteger-cpu-arguments-coefficient11"
  , "expModInteger-cpu-arguments-coefficient12"
  , "expModInteger-memory-arguments-intercept"
  , "expModInteger-memory-arguments-slope"
  , "dropList-cpu-arguments-intercept"
  , "dropList-cpu-arguments-slope"
  , "dropList-memory-arguments"
  , "lengthOfArray-cpu-arguments"
  , "lengthOfArray-memory-arguments"
  , "listToArray-cpu-arguments-intercept"
  , "listToArray-cpu-arguments-slope"
  , "listToArray-memory-arguments-intercept"
  , "listToArray-memory-arguments-slope"
  , "indexArray-cpu-arguments"
  , "indexArray-memory-arguments"
  , "bls12_381_G1_multiScalarMul-cpu-arguments-intercept"
  , "bls12_381_G1_multiScalarMul-cpu-arguments-slope"
  , "bls12_381_G1_multiScalarMul-memory-arguments"
  , "bls12_381_G2_multiScalarMul-cpu-arguments-intercept"
  , "bls12_381_G2_multiScalarMul-cpu-arguments-slope"
  , "bls12_381_G2_multiScalarMul-memory-arguments"
  , "insertCoin-cpu-arguments-intercept"
  , "insertCoin-cpu-arguments-slope"
  , "insertCoin-memory-arguments-intercept"
  , "insertCoin-memory-arguments-slope"
  , "lookupCoin-cpu-arguments-intercept"
  , "lookupCoin-cpu-arguments-slope"
  , "lookupCoin-memory-arguments"
  , "unionValue-cpu-arguments-c00"
  , "unionValue-cpu-arguments-c10"
  , "unionValue-cpu-arguments-c01"
  , "unionValue-cpu-arguments-c11"
  , "unionValue-memory-arguments-intercept"
  , "unionValue-memory-arguments-slope"
  , "valueContains-cpu-arguments-constant"
  , "valueContains-cpu-arguments-model-arguments-intercept"
  , "valueContains-cpu-arguments-model-arguments-slope1"
  , "valueContains-cpu-arguments-model-arguments-slope2"
  , "valueContains-memory-arguments"
  , "valueData-cpu-arguments-intercept"
  , "valueData-cpu-arguments-slope"
  , "valueData-memory-arguments-intercept"
  , "valueData-memory-arguments-slope"
  , "unValueData-cpu-arguments-c0"
  , "unValueData-cpu-arguments-c1"
  , "unValueData-cpu-arguments-c2"
  , "unValueData-memory-arguments-intercept"
  , "unValueData-memory-arguments-slope"
  , "scaleValue-cpu-arguments-intercept"
  , "scaleValue-cpu-arguments-slope"
  , "scaleValue-memory-arguments-intercept"
  , "scaleValue-memory-arguments-slope"
  ]
  ++
  repeat (error "PlutusV2 cost with no name")

-- Source: Plutus's `PlutusLedgerApi.V3.ParamName` enum,
--   https://github.com/IntersectMBO/plutus/blob/b6e724e9577419e0cddfaf861ce2d77ea7526d09/plutus-ledger-api/src/PlutusLedgerApi/V3/ParamName.hs
plutusV3CostNames :: [String]
plutusV3CostNames =
  [
  -- Positions 0..250 (251 entries): Chang HF (Conway era, mainnet epoch 507).
  -- Original V3 init set.
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
  -- Positions 251..296 (46 entries):
  --   post-Chang on-chain parameter update at mainnet epoch 526
  --   (bitwise / byte-string ops, ripemd_160).
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
  -- Positions 297..349 (53 entries): van Rossem (enacted on epoch 638).
  -- See governance action:
  -- gov_action1eqhnsdyf3exhp5mqt7sdjtl7xy69wqg8tvg854psns2jt72cra3qqrcnr8r
  -- (https://gov.tools/governance_actions/c82f3834898e4d70d3605fa0d92ffe31345701075b107a54309c1525f9581f62).
  , "expModInteger-cpu-arguments-coefficient00"
  , "expModInteger-cpu-arguments-coefficient11"
  , "expModInteger-cpu-arguments-coefficient12"
  , "expModInteger-memory-arguments-intercept"
  , "expModInteger-memory-arguments-slope"
  , "dropList-cpu-arguments-intercept"
  , "dropList-cpu-arguments-slope"
  , "dropList-memory-arguments"
  , "lengthOfArray-cpu-arguments"
  , "lengthOfArray-memory-arguments"
  , "listToArray-cpu-arguments-intercept"
  , "listToArray-cpu-arguments-slope"
  , "listToArray-memory-arguments-intercept"
  , "listToArray-memory-arguments-slope"
  , "indexArray-cpu-arguments"
  , "indexArray-memory-arguments"
  , "bls12_381_G1_multiScalarMul-cpu-arguments-intercept"
  , "bls12_381_G1_multiScalarMul-cpu-arguments-slope"
  , "bls12_381_G1_multiScalarMul-memory-arguments"
  , "bls12_381_G2_multiScalarMul-cpu-arguments-intercept"
  , "bls12_381_G2_multiScalarMul-cpu-arguments-slope"
  , "bls12_381_G2_multiScalarMul-memory-arguments"
  , "insertCoin-cpu-arguments-intercept"
  , "insertCoin-cpu-arguments-slope"
  , "insertCoin-memory-arguments-intercept"
  , "insertCoin-memory-arguments-slope"
  , "lookupCoin-cpu-arguments-intercept"
  , "lookupCoin-cpu-arguments-slope"
  , "lookupCoin-memory-arguments"
  , "unionValue-cpu-arguments-c00"
  , "unionValue-cpu-arguments-c10"
  , "unionValue-cpu-arguments-c01"
  , "unionValue-cpu-arguments-c11"
  , "unionValue-memory-arguments-intercept"
  , "unionValue-memory-arguments-slope"
  , "valueContains-cpu-arguments-constant"
  , "valueContains-cpu-arguments-model-arguments-intercept"
  , "valueContains-cpu-arguments-model-arguments-slope1"
  , "valueContains-cpu-arguments-model-arguments-slope2"
  , "valueContains-memory-arguments"
  , "valueData-cpu-arguments-intercept"
  , "valueData-cpu-arguments-slope"
  , "valueData-memory-arguments-intercept"
  , "valueData-memory-arguments-slope"
  , "unValueData-cpu-arguments-c0"
  , "unValueData-cpu-arguments-c1"
  , "unValueData-cpu-arguments-c2"
  , "unValueData-memory-arguments-intercept"
  , "unValueData-memory-arguments-slope"
  , "scaleValue-cpu-arguments-intercept"
  , "scaleValue-cpu-arguments-slope"
  , "scaleValue-memory-arguments-intercept"
  , "scaleValue-memory-arguments-slope"
  ]
  ++
  repeat (error "PlutusV3 cost with no name")
