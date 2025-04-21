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
import qualified Cardano.Benchmarking.Profile.Genesis as Genesis
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
          Aeson.object $ zip (map fromString Genesis.plutusV1CostNames) (toList v1s)
        ("PlutusV2", Aeson.Array v2s) ->
          Aeson.object $ zip (map fromString Genesis.plutusV2CostNames) (toList v2s)
        ("PlutusV3", Aeson.Array v3s) ->
          Aeson.object $ zip (map fromString Genesis.plutusV3CostNames) (toList v3s)
        _ -> v
    )
