{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module  Cardano.TxGenerator.PlutusContext
        ( plutusAutoBudgetMaxOut
        , readRedeemer
        , scriptDataModifyNumber
        )
        where

import           Control.Monad.Trans.Except.Extra
import           Data.Aeson as Aeson

import           Cardano.Api
import           Cardano.Api.Shelley (ProtocolParameters)

import           Cardano.TxGenerator.Setup.Plutus (preExecutePlutusScript)
import           Cardano.TxGenerator.Types


-- | load a redeemer for the script
readRedeemer :: FilePath -> IO (Either TxGenError ScriptRedeemer)
readRedeemer redeemerPath
  = runExceptT $ do
    sData :: Aeson.Value <-
      firstExceptT TxGenError . hoistEither =<<
        handleIOExceptT (TxGenError . show) (eitherDecodeFileStrict' redeemerPath)
    firstExceptT ApiError . hoistEither $
       scriptDataFromJson ScriptDataJsonDetailedSchema sData

-- | Use a binary search to find a loop counter that maxes out the available script execution units.
--   plutusAutoBudgetMaxOut makes two assumptions about the loop / PlutusAuto script:
--   1. The redeemer passed in is a valid one, and encodes i.a. the loop's
--      termination value when counting down.
--   2. In the redeemer's argument structure, this value is the first numerical value
--      that's encountered during traversal.
plutusAutoBudgetMaxOut :: ProtocolParameters -> ScriptInAnyLang -> PlutusAutoBudget -> Either TxGenError PlutusAutoBudget
plutusAutoBudgetMaxOut protocolParams script pab@PlutusAutoBudget{..}
  = do
    redeemer' <- toLoopArgument <$> binarySearch isInLimits 0 searchUpperBound
    pure $ pab {autoBudgetRedeemer = redeemer'}
  where
    -- The highest loop counter that is tried - this is about 10 times the current mainnet limit.
    searchUpperBound = 20000

    toLoopArgument n = scriptDataModifyNumber (+ n) autoBudgetRedeemer

    isInLimits :: Integer -> Either TxGenError Bool
    isInLimits n = do
      used <- preExecutePlutusScript protocolParams script autoBudgetDatum (toLoopArgument n)
      pure $ executionSteps used <= executionSteps autoBudgetUnits && executionMemory used <= executionMemory autoBudgetUnits

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

binarySearch :: (Integral n, Show n) => (n -> Either TxGenError Bool) -> n -> n -> Either TxGenError n
binarySearch f a_ b_ = do
  l <- f a_
  h <- f b_
  if l && not h
    then go a_ b_
    else Left $ TxGenError $ "binarySearch: bad initial bounds: " ++ show (a_,l,b_,h)
  where
    go a b
      | a + 1 == b = Right a
      | otherwise = do
          let m = (a + b) `div` 2
          test <- f m
          if test then go m b else go a m
