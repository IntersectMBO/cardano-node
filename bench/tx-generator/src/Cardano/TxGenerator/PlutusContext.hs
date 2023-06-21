{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

{-
Module      : Cardano.TxGenerator.PlutusContext
Description : Handle Plutus budgeting and script loading.
-}
module  Cardano.TxGenerator.PlutusContext
        ( PlutusAutoLimitingFactor(..)
        , PlutusBudgetFittingStrategy(..)
        , PlutusBudgetSummary(..)
        , plutusAutoBudgetMaxOut
        , plutusAutoScaleBlockfit
        , plutusBudgetSummary
        , readScriptData
        , scriptDataModifyNumber
        )
        where

import           GHC.Generics (Generic)
import           GHC.Natural (Natural)

import           Control.Monad.Trans.Except.Extra
import           Data.Aeson as Aeson
import           Data.List (maximumBy, minimumBy)
import           Data.Ord (comparing)

import           Cardano.Api
import           Cardano.Api.Shelley (ProtocolParameters (..))

import           Cardano.TxGenerator.Setup.Plutus (preExecutePlutusScript)
import           Cardano.TxGenerator.Types


-- | This collects information describing the budget. It's only
-- directly referenced in "Cardano.Benchmarking.Script.Env" to supply
-- accessors as the state in an `Cardano.Benchmarking.Script.ActionM`
-- (RWST) monad and "Cardano.Benchmarking.Script.Core" where the only
-- field touched is `projectedTxSize`.
data PlutusBudgetSummary =
  PlutusBudgetSummary
  { budgetPerBlock          :: !ExecutionUnits
  , budgetPerTx             :: !ExecutionUnits
  , budgetPerTxInput        :: !ExecutionUnits
  , budgetStrategy          :: !PlutusBudgetFittingStrategy
  , budgetTarget            :: !ExecutionUnits
  , scriptId                :: !FilePath
  , scriptArgDatum          :: !ScriptData
  , scriptArgRedeemer       :: !ScriptData
  , loopCounter             :: !Int
  , loopLimitingFactors     :: ![PlutusAutoLimitingFactor]
  , budgetUsedPerTxInput    :: !ExecutionUnits
  , projectedBudgetUnusedPerBlock :: !ExecutionUnits
  , projectedBudgetUnusedPerTx    :: !ExecutionUnits
  , projectedTxPerBlock     :: !Int
  , projectedLoopsPerBlock  :: !Int
  , projectedTxSize         :: !(Maybe Int)
  , strategyMessage         :: !(Maybe String)
  }
  deriving (Generic, Show, ToJSON)

-- | Nothing references this type's name or uses its constructors
-- outside this module.
data PlutusAutoLimitingFactor
  = ExceededMemoryLimit
  | ExceededStepLimit
  deriving (Generic, Eq, Show, ToJSON)

-- | This type specifies the end to which a script's loop counter is calibrated
data PlutusBudgetFittingStrategy
  = TargetTxExpenditure                     -- ^ calibrate for maximum expenditure of per-tx-budget
  | TargetBlockExpenditure (Maybe Double)   -- ^ calibrate for maximum expenditure of per-block-budget, with a scaling factor of [1.0, 2.0]
  | TargetTxsPerBlock Int                   -- ^ calibrate for stable tx count per block
  deriving (Generic, Eq, Show, ToJSON)

instance ToJSON ScriptData where
  toJSON = scriptDataToJson ScriptDataJsonDetailedSchema . unsafeHashableScriptData


-- | Load serialized ScriptData, filling in an empty value if no
-- .json file is given
readScriptData :: FilePath -> IO (Either TxGenError HashableScriptData)
readScriptData ""
  = pure $ Right $ unsafeHashableScriptData $ ScriptDataNumber 0             -- TODO: make sure this is an adequate empty value
readScriptData jsonFilePath
  = runExceptT $ do
    sData :: Aeson.Value <-
      firstExceptT TxGenError . hoistEither =<<
        handleIOExceptT (TxGenError . show) (eitherDecodeFileStrict' jsonFilePath)
    firstExceptT ApiError . hoistEither $
       scriptDataFromJson ScriptDataJsonDetailedSchema sData

-- | Can find the optimal scaling factor for block expenditure, by aiming at highest
-- loop count per block iff TargetBlockExpenditure Nothing is given;
-- will calibrate loop for any fully specified fitting strategy otherwise
plutusAutoScaleBlockfit ::
     ProtocolParameters
  -> FilePath
  -> ScriptInAnyLang
  -> PlutusAutoBudget
  -> PlutusBudgetFittingStrategy
  -> Int
  -> Either TxGenError (PlutusBudgetSummary, PlutusAutoBudget, ExecutionUnits)
plutusAutoScaleBlockfit pparams fp script pab strategy txInputs
  = do
    summaries <- mapM go scalingStrats
    let
      maxLoops  = maximumBy (comparing (projectedLoopsPerBlock . fst3)) summaries
      minSteps  = minimumBy (comparing (executionSteps . projectedBudgetUnusedPerBlock . fst3)) summaries
      msg = case scalingStrats of
        [TargetTxExpenditure] -> "maxing out loops for tx budget was indicated"
        [TargetTxsPerBlock t] -> "a fixed " ++ show t ++ " txs per block was specified"
        _
          | budgetStrategy (fst3 maxLoops) == budgetStrategy (fst3 minSteps) ->
            "maximizes loops per block AND minimizes unused execution steps per block"
          | otherwise ->
            "maximizes loops per block BUT DOES NOT minimize unused execution steps per block"
    pure $ setMessage msg maxLoops
  where
    fst3 (x, _, _)  = x
    setMessage msg (summ, b, c) = (summ {strategyMessage = Just msg}, b, c)

    scalingStrats
      = case strategy of
        TargetBlockExpenditure Nothing  -> TargetBlockExpenditure . Just <$> [1.0, 1.25 .. 2.0]
        s                               -> [s]

    go strat = do
      result@(pab', _, _) <- plutusAutoBudgetMaxOut pparams script pab strat txInputs
      preRun <- preExecutePlutusScript pparams script (autoBudgetDatum pab') (autoBudgetRedeemer pab')
      pure  ( plutusBudgetSummary pparams fp strat result preRun txInputs
            , pab'
            , preRun
            )

-- | Use a binary search to find a loop counter that maxes out the available script execution units.
--   plutusAutoBudgetMaxOut makes two assumptions about the loop / PlutusAuto script:
--   1. The redeemer passed in is a valid one, and encodes i.a. the loop's
--      termination value when counting down.
--   2. In the redeemer's argument structure, this value is the first numerical value
--      that's encountered during traversal.
plutusAutoBudgetMaxOut ::
     ProtocolParameters
  -> ScriptInAnyLang
  -> PlutusAutoBudget
  -> PlutusBudgetFittingStrategy
  -> Int
  -> Either TxGenError (PlutusAutoBudget, Int, [PlutusAutoLimitingFactor])
plutusAutoBudgetMaxOut _ _ _ (TargetBlockExpenditure Nothing) _
  = Left $ TxGenError "plutusAutoBudgetMaxOut : a scaling factor is required for TargetBlockExpenditure"
plutusAutoBudgetMaxOut
  protocolParams@ProtocolParameters
    { protocolParamMaxBlockExUnits  = Just budgetPerBlock
    , protocolParamMaxTxExUnits     = Just budgetPerTx
    }
  script
  pab@PlutusAutoBudget{..}
  target
  txInputs
  = do
    (n, limitFactors) <- binarySearch isInLimits 0 searchUpperBound
    let pab' = pab {autoBudgetUnits = targetBudget, autoBudgetRedeemer = unsafeHashableScriptData $ toLoopArgument n}
    pure (pab', fromIntegral n, limitFactors)
  where
    -- The highest loop counter that is tried - this is about 10 times the current mainnet limit.
    searchUpperBound  = 20000

    targetTxPerBlock :: Double -> Int
    targetTxPerBlock  s =
      let mTx :: Double = fromIntegral (executionMemory budgetPerTx)
      in ceiling $ s * max
        (fromIntegral (executionSteps budgetPerBlock) / fromIntegral (executionSteps budgetPerTx))
        (fromIntegral (executionMemory budgetPerBlock) / mTx)

    targetBudget = case target of
      TargetTxExpenditure             -> calc budgetPerTx div txInputs
      TargetTxsPerBlock t             -> calc budgetPerBlock div (t * txInputs) `bmin` budgetPerTx
      TargetBlockExpenditure (Just s) -> calc budgetPerBlock div (targetTxPerBlock s * txInputs)
      TargetBlockExpenditure Nothing  -> error "plutusAutoBudgetMaxOut : TargetBlockExpenditure Nothing should be unreachable. This is an implementation error in tx-generator."

    toLoopArgument n = scriptDataModifyNumber (+ n) $ getScriptData autoBudgetRedeemer

    -- the execution is considered within limits when there's no limiting factor, i.e. the list is empty
    isInLimits :: Integer -> Either TxGenError [PlutusAutoLimitingFactor]
    isInLimits n = do
      used <- preExecutePlutusScript protocolParams script autoBudgetDatum (unsafeHashableScriptData $ toLoopArgument n)
      pure $   [ExceededStepLimit   | executionSteps used > executionSteps targetBudget]
            ++ [ExceededMemoryLimit | executionMemory used > executionMemory targetBudget]

plutusAutoBudgetMaxOut _ _ _ _ _
  = error "plutusAutoBudgetMaxOut : call to function in pre-Alonzo era. This is an implementation error in tx-generator."

-- | Only used in `plutusAutoScaleBlockfit`, this assembles a
-- `PlutusBudgetSummary` from information about the how to budget.
-- Some of the function arguments share names with the record fields
-- mass imported with the @Constr{..}@ notation, setting the field
-- of the final result to that argument.
plutusBudgetSummary ::
     ProtocolParameters
  -> FilePath
  -> PlutusBudgetFittingStrategy
  -> (PlutusAutoBudget, Int, [PlutusAutoLimitingFactor])
  -> ExecutionUnits
  -> Int
  -> PlutusBudgetSummary
plutusBudgetSummary
  ProtocolParameters
    { protocolParamMaxBlockExUnits  = Just budgetPerBlock
    , protocolParamMaxTxExUnits     = Just budgetPerTx
    }
  scriptId
  budgetStrategy
  (PlutusAutoBudget{..}, loopCounter, loopLimitingFactors)
  budgetUsedPerTxInput
  txInputs
  = PlutusBudgetSummary{..}
  where
    projectedTxSize         = Nothing           -- we defer this value until after splitting phase
    strategyMessage         = Nothing
    scriptArgDatum          = autoBudgetDatum
    scriptArgRedeemer       = getScriptData autoBudgetRedeemer
    budgetPerTxInput        = calc budgetPerTx div txInputs
    budgetTarget            = autoBudgetUnits
    projectedTxPerBlock     = fromIntegral $ min
                                (executionSteps budgetPerBlock  `div` executionSteps usedPerTx)
                                (executionMemory budgetPerBlock `div` executionMemory usedPerTx)
    projectedLoopsPerBlock  = loopCounter * txInputs * projectedTxPerBlock
    projectedBudgetUnusedPerTx    = budgetPerTx `minus` usedPerTx
    projectedBudgetUnusedPerBlock = budgetPerBlock `minus` calc usedPerTx (*) projectedTxPerBlock
    usedPerTx                = calc budgetUsedPerTxInput (*) txInputs

plutusBudgetSummary _ _ _ _ _ _
  = error "plutusBudgetSummary : call to function in pre-Alonzo era. This is an implementation error in tx-generator."


-- | Modifies the first ScriptDataNumber encountered during traversal
-- to the value provided
scriptDataModifyNumber :: (Integer -> Integer) -> ScriptData -> ScriptData
scriptDataModifyNumber f
  = go
  where
    go = \case
      ScriptDataNumber i -> ScriptDataNumber (f i)
      ScriptDataConstructor int list -> ScriptDataConstructor int (goList list)
      ScriptDataList list -> ScriptDataList (goList list)
      ScriptDataMap m ->
        let {(ks, vs) = unzip m; vs' = goList vs}
        in ScriptDataMap (zip ks vs')
      other -> other
    goList [] = []
    goList (x:xs) =
      let x' = go x in if x' == x then x : goList xs else x' : xs

binarySearch :: (Integral n, Show n) => (n -> Either TxGenError [a]) -> n -> n -> Either TxGenError (n, [a])
binarySearch f a_ b_ = do
  l <- withinLimits <$> f a_
  h <- withinLimits <$> f b_
  if l && not h
    then go [] a_ b_
    else Left $ TxGenError $ "binarySearch: bad initial bounds: " ++ show (a_,l,b_,h)
  where
    withinLimits = null
    go limitingFactors a b
      | a + 1 == b = Right (a, limitingFactors)
      | otherwise = do
          let m = (a + b) `div` 2
          limitingFactors' <- f m
          if withinLimits limitingFactors'
            then go limitingFactors m b
            else go limitingFactors' a m


minus :: ExecutionUnits -> ExecutionUnits -> ExecutionUnits
minus (ExecutionUnits a b) (ExecutionUnits a' b')
  = ExecutionUnits (a - a') (b - b')

calc :: ExecutionUnits -> (Natural -> Natural -> Natural) -> Int -> ExecutionUnits
calc (ExecutionUnits a b) op (fromIntegral -> n)
  = ExecutionUnits (a `op` n) (b `op` n)

bmin :: ExecutionUnits -> ExecutionUnits -> ExecutionUnits
bmin (ExecutionUnits a b) (ExecutionUnits a' b')
  = ExecutionUnits (min a a') (min b b')
