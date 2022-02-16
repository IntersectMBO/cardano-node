{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module Cardano.Benchmarking.Compiler
where

import           Prelude

import           Control.Applicative (liftA2)
import           Control.Monad
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.RWS.CPS

import           Data.Dependent.Sum ( (==>) )
import           Data.DList (DList)
import qualified Data.DList as DL

import           Cardano.Api
import           Cardano.Benchmarking.Types
import           Cardano.Benchmarking.NixOptions
import           Cardano.Benchmarking.Script.Setters
import           Cardano.Benchmarking.Script.Store (Name(..))
import           Cardano.Benchmarking.Script.Types

data CompileError where
  SomeCompilerError :: CompileError
  deriving (Show)
type Compiler a = RWST NixServiceOptions (DList Action) () (Except CompileError) a

compileOptions :: NixServiceOptions -> Either CompileError [Action]
compileOptions = runCompiler compileToScript

runCompiler :: Compiler () -> NixServiceOptions -> Either CompileError [Action]
runCompiler c o = case runExcept $ runRWST c o () of
  Left err -> Left err
  Right ((), _ , l) -> Right $ DL.toList l

testCompiler :: Compiler a -> NixServiceOptions -> Either CompileError (a, (), [Action])
testCompiler c o = case runExcept $ runRWST c o () of
  Left err -> Left err
  Right (a, s , l) -> Right (a, s, DL.toList l)

compileToScript :: Compiler ()
compileToScript = do
  initConstants
  emit . StartProtocol =<< askNixOption _nix_nodeConfigFile
  importGenesisFunds
  initCollaterals
  splittingPhase
  benchmarkingPhase

initConstants :: Compiler ()
initConstants = do
  setN TNumberOfInputsPerTx  _nix_inputs_per_tx
  setN TNumberOfOutputsPerTx _nix_outputs_per_tx
  setN TNumberOfTxs          _nix_tx_count
  setN TTxAdditionalSize     _nix_add_tx_size
  setN TMinValuePerUTxO      _nix_min_utxo_value
  setN TFee                  _nix_tx_fee
  setN TEra                  _nix_era
  setN TTargets              _nix_targetNodes
  setN TLocalSocket          _nix_localNodeSocketPath
  setConst  TTTL             1000000
  where
    setConst :: Tag v -> v -> Compiler ()
    setConst key val = emit $ Set $ key ==> val 

    setN :: Tag v -> (NixServiceOptions -> v) -> Compiler ()
    setN key s = askNixOption s >>= setConst key

importGenesisFunds :: Compiler ()
importGenesisFunds = do
  cmd1 (ReadSigningKey $ KeyName "pass-partout") _nix_sigKey
  emit $ ImportGenesisFund LocalSocket (KeyName "pass-partout") (KeyName "pass-partout")
  delay

initCollaterals :: Compiler ()
initCollaterals = do
  isAnyPlutusMode >>= \case
    False -> return ()
    True -> do
      tx_fee <- askNixOption _nix_tx_fee
      safeCollateral <- _safeCollateral <$> evilFeeMagic
      emit $ CreateChange LocalSocket (PayToAddr $ KeyName "pass-partout") (safeCollateral + tx_fee) 1
      emit $ CreateChange LocalSocket (PayToCollateral $ KeyName "pass-partout") safeCollateral 1

splittingPhase :: Compiler ()
splittingPhase = do
  (NumberOfTxs tx_count) <- askNixOption _nix_tx_count
  (NumberOfInputsPerTx inputs_per_tx) <- askNixOption _nix_inputs_per_tx
  minValuePerInput <- _minValuePerInput <$> evilFeeMagic
  plutus <- isAnyPlutusMode
  if plutus then createChangeRecursivePlutus minValuePerInput (tx_count * inputs_per_tx)
            else createChangeRecursive       minValuePerInput (tx_count * inputs_per_tx)
 where
  createChangeRecursive :: Lovelace -> Int -> Compiler ()
  createChangeRecursive value count = do
    when (count > 30) $ do
      tx_fee <- askNixOption _nix_tx_fee
      createChangeRecursive (value * 30 + tx_fee) (count `div` 30 + 1)
    createChange value count

  createChangeRecursivePlutus :: Lovelace -> Int -> Compiler ()
  createChangeRecursivePlutus value count = do
    when (count > 30) $ do
      tx_fee <- askNixOption _nix_tx_fee
      createChangeRecursive (value * 30 + tx_fee) (count `div` 30 + 1)
    createChangePlutus value count
  
  createChange :: Lovelace -> Int -> Compiler ()
  createChange value count = do
     emit $ CreateChange LocalSocket (PayToAddr $ KeyName "pass-partout") value count
     delay

  createChangePlutus :: Lovelace -> Int -> Compiler ()
  createChangePlutus value count = do
     autoMode <- isPlutusAutoMode
     plutusTarget <- if autoMode
       then PayToScript <$> askNixOption _nix_plutusLoopScript <*> pure (ScriptDataNumber 0)
       else PayToScript <$> askNixOption _nix_plutusScript     <*> (ScriptDataNumber <$> askNixOption _nix_plutusData)
     emit $ CreateChange LocalSocket plutusTarget value count
     delay

benchmarkingPhase :: Compiler ()
benchmarkingPhase = do
  debugMode <- askNixOption _nix_debugMode
  plutusMode <- askNixOption _nix_plutusMode
  plutusAutoMode <- askNixOption _nix_plutusAutoMode
  tx_count <- askNixOption _nix_tx_count
  tps <- askNixOption _nix_tps
  let target = if debugMode then LocalSocket else NodeToNode
  spendMode <- case (plutusAutoMode, plutusMode) of
    ( True,    _ ) -> SpendAutoScript <$> askNixOption  _nix_plutusLoopScript
    (False, True ) -> do
      executionUnits <- ExecutionUnits <$> askNixOption _nix_executionMemory <*> askNixOption _nix_executionSteps
      scriptBudget <- if debugMode
        then return $ CheckScriptBudget executionUnits
        else return $ StaticScriptBudget executionUnits
      SpendScript <$> askNixOption _nix_plutusScript
                  <*> pure scriptBudget
                  <*> (ScriptDataNumber <$> askNixOption _nix_plutusData)
                  <*> (ScriptDataNumber <$> askNixOption _nix_plutusRedeemer)
    (False,False) ->  return SpendOutput
  emit $ RunBenchmark target spendMode (ThreadName "tx-submit-benchmark") tx_count tps
  unless debugMode $ do
    emit $ WaitBenchmark $ ThreadName "tx-submit-benchmark"

data Fees = Fees {
    _safeCollateral :: Lovelace
  , _minValuePerInput :: Lovelace
  }
  
evilFeeMagic :: Compiler Fees
evilFeeMagic = do
  (Quantity tx_fee) <- lovelaceToQuantity <$> askNixOption _nix_tx_fee
  plutusMode <- askNixOption _nix_plutusMode  
  (NumberOfInputsPerTx inputs_per_tx) <- askNixOption _nix_inputs_per_tx
  (NumberOfOutputsPerTx outputs_per_tx) <- askNixOption _nix_outputs_per_tx  
  (Quantity min_utxo_value)  <- lovelaceToQuantity <$> askNixOption _nix_min_utxo_value
  let
    scriptFees = 5000000;
    collateralPercentage = 200;

    totalFee = if plutusMode
               then tx_fee + scriptFees * fromIntegral inputs_per_tx
               else tx_fee;
    safeCollateral = max ((scriptFees + tx_fee) * collateralPercentage `div` 100) min_utxo_value;
    minTotalValue = min_utxo_value * fromIntegral outputs_per_tx + totalFee;
    minValuePerInput = minTotalValue `div` fromIntegral inputs_per_tx + 1;
  return $ Fees {
      _safeCollateral = fromIntegral safeCollateral
    , _minValuePerInput = fromIntegral minValuePerInput
    }

emit :: Action -> Compiler ()
emit = tell . DL.singleton

cmd1 :: (v -> Action) -> (NixServiceOptions -> v) -> Compiler ()
cmd1 cmd arg = emit . cmd =<< askNixOption arg
  
askNixOption :: (NixServiceOptions -> v) -> Compiler v
askNixOption = asks

delay :: Compiler ()
delay = cmd1 Delay _nix_init_cooldown

isPlutusMode :: Compiler Bool
isPlutusMode = askNixOption _nix_plutusMode

isPlutusAutoMode :: Compiler Bool
isPlutusAutoMode = askNixOption _nix_plutusAutoMode

isAnyPlutusMode :: Compiler Bool
isAnyPlutusMode = liftA2 (||) isPlutusMode isPlutusAutoMode
