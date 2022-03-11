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
import           Cardano.Benchmarking.Script.Store (Name(..), WalletName)
import           Cardano.Benchmarking.Script.Types

data CompileError where
  SomeCompilerError :: CompileError
  deriving (Show)
type Compiler a = RWST NixServiceOptions (DList Action) Int (Except CompileError) a

type SrcWallet = WalletName
type DstWallet = WalletName

compileOptions :: NixServiceOptions -> Either CompileError [Action]
compileOptions opts = runCompiler opts compileToScript

runCompiler ::NixServiceOptions -> Compiler () -> Either CompileError [Action]
runCompiler o c = case runExcept $ runRWST c o 0 of
  Left err -> Left err
  Right ((), _ , l) -> Right $ DL.toList l

testCompiler :: NixServiceOptions -> Compiler a -> Either CompileError (a, Int, [Action])
testCompiler o c = case runExcept $ runRWST c o 0 of
  Left err -> Left err
  Right (a, s , l) -> Right (a, s, DL.toList l)

compileToScript :: Compiler ()
compileToScript = do
  initConstants
  emit . StartProtocol =<< askNixOption _nix_nodeConfigFile
  genesisWallet <- newWallet "genesis_wallet"
  importGenesisFunds genesisWallet
  splitWallet <- splittingPhase genesisWallet
  addCollaterals genesisWallet splitWallet
  benchmarkingPhase splitWallet

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

importGenesisFunds :: DstWallet -> Compiler ()
importGenesisFunds wallet = do
  cmd1 (ReadSigningKey $ KeyName "pass-partout") _nix_sigKey
  emit $ ImportGenesisFund wallet LocalSocket (KeyName "pass-partout") (KeyName "pass-partout")
  delay

addCollaterals :: SrcWallet -> DstWallet -> Compiler ()
addCollaterals src dest = do
  isAnyPlutusMode >>= \case
    False -> return ()
    True -> do
      tx_fee <- askNixOption _nix_tx_fee
      safeCollateral <- _safeCollateral <$> evilFeeMagic
      emit $ CreateChange src src LocalSocket (PayToAddr $ KeyName "pass-partout") (safeCollateral + tx_fee) 1
      emit $ CreateChange src dest LocalSocket (PayToCollateral $ KeyName "pass-partout") safeCollateral 1

splittingPhase :: SrcWallet -> Compiler DstWallet
splittingPhase srcWallet = do
  (NumberOfTxs tx_count) <- askNixOption _nix_tx_count
  (NumberOfInputsPerTx inputs_per_tx) <- askNixOption _nix_inputs_per_tx
  tx_fee <- askNixOption _nix_tx_fee
  minValuePerInput <- _minValuePerInput <$> evilFeeMagic
  splitSteps <- splitSequenceWalletNames srcWallet srcWallet $ unfoldSplitSequence tx_fee minValuePerInput (tx_count * inputs_per_tx)
  forM_ (init splitSteps) createChange
  plutus <- isAnyPlutusMode
  (if plutus then createChangePlutus else createChange) $ last splitSteps
 where
  createChange :: SplitStep -> Compiler DstWallet
  createChange (src, dst, value, count) = do
     emit $ CreateChange src dst LocalSocket (PayToAddr $ KeyName "pass-partout") value count
     delay
     return dst

  createChangePlutus :: SplitStep -> Compiler DstWallet
  createChangePlutus (src, dst, value, count) = do
     autoMode <- isPlutusAutoMode
     plutusTarget <- if autoMode
       then PayToScript <$> askNixOption _nix_plutusLoopScript <*> pure (ScriptDataNumber 0)
       else PayToScript <$> askNixOption _nix_plutusScript     <*> (ScriptDataNumber <$> askNixOption _nix_plutusData)
     emit $ CreateChange src dst LocalSocket plutusTarget value count
     delay
     return dst

-- Generate src and dst wallet names for a splitSequence.
-- testCompiler (error "opts") $ splitSequenceWalletNames (WalletName "w1") (WalletName "w2") (unfoldSplitSequence 1 1000 10000)
type SplitStep = (SrcWallet, DstWallet, Lovelace, Int)

splitSequenceWalletNames :: SrcWallet -> DstWallet -> [(Lovelace, Int)] -> Compiler [ SplitStep ]
splitSequenceWalletNames _src _dst [] = return []
splitSequenceWalletNames src dst [ (val,count) ] = return [( src, dst, val, count)]
splitSequenceWalletNames src dst ((val, count):rest) = do
  nextDst <- newWallet "change_wallet"
  l <- splitSequenceWalletNames dst nextDst rest
  return $ ( src, dst, val, count) : l

-- Return a list of splitSteps.
unfoldSplitSequence :: Lovelace -> Lovelace -> Int -> [(Lovelace, Int)]
unfoldSplitSequence fee value count
  = if count < maxOutputs
    then [
           -- Add an extra transaction that just contains the desired output and possible fees.
           (value * fromIntegral count + fee, 1)
         , (value, count )
         ]
    else unfoldSplitSequence fee (value * fromIntegral maxOutputs + fee) (count `div` maxOutputs + 1) ++ [ (value, count) ]
  where
    -- maximal number of outputs in a TX.
    -- todo: this must be in sync with Scipt/Core.hs
    maxOutputs = 30
    
benchmarkingPhase :: WalletName -> Compiler ()
benchmarkingPhase wallet = do
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
  emit $ RunBenchmark wallet target spendMode (ThreadName "tx-submit-benchmark") tx_count tps
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

newIdentifier :: String -> Compiler String
newIdentifier prefix = do
  n <- get
  put $ succ n
  return $ prefix ++ "_" ++ show n

newWallet :: String -> Compiler WalletName
newWallet n = do
  name <- WalletName <$> newIdentifier n
  emit $ InitWallet name
  return name

  
