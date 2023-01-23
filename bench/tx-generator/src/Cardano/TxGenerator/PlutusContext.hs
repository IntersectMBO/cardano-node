{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module  Cardano.TxGenerator.PlutusContext
        ( PlutusAutoLimitingFactor(..)
        , PlutusBudgetFittingStrategy(..)
        , PlutusBudgetSummary(..)
        , plutusAutoBudgetMaxOut
        , plutusBudgetSummary
        , readScriptData
        , scriptDataModifyNumber
        )
        where

import           GHC.Generics (Generic)
import           GHC.Natural (Natural)

import           Control.Monad.Trans.Except.Extra
import           Data.Aeson as Aeson

import           Cardano.Api
import           Cardano.Api.Shelley (ProtocolParameters (..))

import           Cardano.TxGenerator.Setup.Plutus (preExecutePlutusScript)
import           Cardano.TxGenerator.Types


data PlutusBudgetSummary =
  PlutusBudgetSummary
  { budgetPerBlock          :: !ExecutionUnits
  , budgetPerTx             :: !ExecutionUnits
  , budgetPerTxInput        :: !ExecutionUnits
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
  }
  deriving (Generic, Show, ToJSON)

data PlutusAutoLimitingFactor
  = ExceededMemoryLimit
  | ExceededStepLimit
  deriving (Generic, Eq, Show, ToJSON)

data PlutusBudgetFittingStrategy
  = TargetTxExpenditure
  | TargetBlockExpenditure Double

instance ToJSON ScriptData where
  toJSON = scriptDataToJson ScriptDataJsonDetailedSchema


-- | load serialized ScriptData, filling in an empty value if no .json file is given
readScriptData :: FilePath -> IO (Either TxGenError ScriptData)
readScriptData ""
  = pure $ Right $ ScriptDataNumber 0             -- TODO: make sure this is an adequate empty value
readScriptData jsonFilePath
  = runExceptT $ do
    sData :: Aeson.Value <-
      firstExceptT TxGenError . hoistEither =<<
        handleIOExceptT (TxGenError . show) (eitherDecodeFileStrict' jsonFilePath)
    firstExceptT ApiError . hoistEither $
       scriptDataFromJson ScriptDataJsonDetailedSchema sData

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
    let pab' = pab {autoBudgetUnits = targetBudget, autoBudgetRedeemer = toLoopArgument n}
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
      TargetTxExpenditure       -> calc budgetPerTx div txInputs                            -- optimize for maximum expenditure of per-tx-budget
      TargetBlockExpenditure s  -> calc budgetPerBlock div (targetTxPerBlock s * txInputs)  -- optimize for maximum expenditure of per-block-budget

    toLoopArgument n = scriptDataModifyNumber (+ n) autoBudgetRedeemer

    -- the execution is considered within limits when there's no limiting factor, i.e. the list is empty
    isInLimits :: Integer -> Either TxGenError [PlutusAutoLimitingFactor]
    isInLimits n = do
      used <- preExecutePlutusScript protocolParams script autoBudgetDatum (toLoopArgument n)
      pure $   [ExceededStepLimit   | executionSteps used > executionSteps targetBudget]
            ++ [ExceededMemoryLimit | executionMemory used > executionMemory targetBudget]

plutusAutoBudgetMaxOut _ _ _ _ _
  = error "plutusAutoBudgetMaxOut : call to function in pre-Alonzo era. This is an implementation error in tx-generator."

plutusBudgetSummary ::
     ProtocolParameters
  -> FilePath
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
  (PlutusAutoBudget{..}, loopCounter, loopLimitingFactors)
  budgetUsedPerTxInput
  txInputs
  = PlutusBudgetSummary{..}
  where
    projectedTxSize         = Nothing           -- we defer this value until after splitting phase
    scriptArgDatum          = autoBudgetDatum
    scriptArgRedeemer       = autoBudgetRedeemer
    budgetPerTxInput        = calc budgetPerTx div txInputs
    budgetTarget            = autoBudgetUnits
    projectedTxPerBlock     = fromIntegral $ min
                                (executionSteps budgetPerBlock  `div` executionSteps usedPerTx)
                                (executionMemory budgetPerBlock `div` executionMemory usedPerTx)
    projectedLoopsPerBlock  = loopCounter * txInputs * projectedTxPerBlock
    projectedBudgetUnusedPerTx    = budgetPerTx `minus` usedPerTx
    projectedBudgetUnusedPerBlock = budgetPerBlock `minus` calc usedPerTx (*) projectedTxPerBlock
    usedPerTx                = calc budgetUsedPerTxInput (*) txInputs

plutusBudgetSummary _ _ _ _ _
  = error "plutusBudgetSummary : call to function in pre-Alonzo era. This is an implementation error in tx-generator."


-- modifies the first ScriptDataNumber encountered during traversal to the value provided
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
