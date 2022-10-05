{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module Cardano.Benchmarking.Compiler
where

import           Prelude

import           Control.Applicative (liftA2)
import           Control.Monad
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.RWS.CPS
import           Data.ByteString as BS (ByteString)
import           Data.ByteString.Base16 as Base16
import           Data.DList (DList)
import qualified Data.DList as DL
import           Data.Dependent.Sum ((==>))
import           Data.Text (Text)
import qualified Data.Text as Text

import           Cardano.Api
import           Cardano.Benchmarking.Script.Setters
import           Cardano.Benchmarking.Script.Store (KeyName, Name (..), WalletName)
import           Cardano.Benchmarking.Script.Types
import           Cardano.TxGenerator.Setup.NixService
import           Cardano.TxGenerator.Types (TxGenTxParams (..))

data CompileError where
  SomeCompilerError :: String -> CompileError
  deriving (Show)
type Compiler a = RWST NixServiceOptions (DList Action) Int (Except CompileError) a

throwCompileError :: CompileError -> Compiler ()
throwCompileError = lift . throwE

maxOutputsPerTx :: Int
maxOutputsPerTx = 30

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
  maybe
    (throwCompileError $ SomeCompilerError "nodeConfigFile not set in Nix options")
    emit
    (StartProtocol <$> askNixOption getNodeConfigFile <*> askNixOption _nix_cardanoTracerSocket)
  genesisWallet <- importGenesisFunds
  collateralWallet <- addCollaterals genesisWallet
  splitWallet <- splittingPhase genesisWallet
  void $ benchmarkingPhase splitWallet collateralWallet

initConstants :: Compiler ()
initConstants = do
  setN TLocalSocket          _nix_localNodeSocketPath
  setConst  TTTL             1000000
  emit $ DefineSigningKey keyNameTxGenFunds keyTxGenFunds
  emit $ DefineSigningKey keyNameCollaterals keyCollaterals
  emit $ DefineSigningKey keyNameSplitPhase keySplitPhase
  emit $ DefineSigningKey keyNameBenchmarkInputs keyBenchmarkInputs
  emit $ DefineSigningKey keyNameBenchmarkDone keyBenchmarkDone
  where
    setConst :: Tag v -> v -> Compiler ()
    setConst key val = emit $ Set $ key ==> val

    setN :: Tag v -> (NixServiceOptions -> v) -> Compiler ()
    setN key s = askNixOption s >>= setConst key

importGenesisFunds :: Compiler WalletName
importGenesisFunds = do
  logMsg "Importing Genesis Fund."
  wallet <- newWallet "genesis_wallet"
  era <- askNixOption _nix_era
  txParams <- askNixOption txGenTxParams
  cmd1 (ReadSigningKey keyNameGenesisInputFund) _nix_sigKey
  emit $ Submit era LocalSocket txParams $ SecureGenesis wallet keyNameGenesisInputFund keyNameTxGenFunds
  delay
  logMsg "Importing Genesis Fund. Done."
  return wallet

addCollaterals :: SrcWallet -> Compiler (Maybe WalletName)
addCollaterals src = do
  era <- askNixOption _nix_era
  txParams <- askNixOption txGenTxParams
  isAnyPlutusMode >>= \case
    False -> return Nothing
    True -> do
      logMsg "Create collaterals."
      safeCollateral <- _safeCollateral <$> evilFeeMagic
      collateralWallet <- newWallet "collateral_wallet"
      let generator = Split src
                        (PayToAddr keyNameCollaterals collateralWallet)
                        (PayToAddr keyNameTxGenFunds src)
                        [ safeCollateral ]
      emit $ Submit era LocalSocket txParams generator
      logMsg "Create collaterals. Done."
      return $ Just collateralWallet

splittingPhase :: SrcWallet -> Compiler DstWallet
splittingPhase srcWallet = do
  tx_count <- askNixOption _nix_tx_count
  inputs_per_tx <- askNixOption _nix_inputs_per_tx
  era <- askNixOption _nix_era
  txParams <- askNixOption txGenTxParams
  minValuePerInput <- _minValuePerInput <$> evilFeeMagic
  finalDest <- newWallet "final_split_wallet"
  splitSteps <- splitSequenceWalletNames srcWallet finalDest $
    unfoldSplitSequence (txParamFee txParams) minValuePerInput (tx_count * inputs_per_tx)
  isPlutus <- isAnyPlutusMode
  forM_ (init splitSteps) $ createChange txParams False False era
  createChange txParams True isPlutus era $ last splitSteps
  return finalDest
 where
  createChange :: TxGenTxParams -> Bool -> Bool -> AnyCardanoEra -> (SrcWallet, DstWallet, Split) -> Compiler ()
  createChange txParams isLastStep isPlutus era (src, dst, split) = do
    logMsg $ Text.pack $ "Splitting step: " ++ show split
    let valuePayMode = PayToAddr (if isLastStep then keyNameSplitPhase else keyNameBenchmarkInputs) dst
    payMode <- if isPlutus then plutusPayMode dst else return valuePayMode
    let generator = case split of
          SplitWithChange lovelace count -> Split src payMode (PayToAddr keyNameTxGenFunds src) $ replicate count lovelace
          FullSplits txCount -> Take txCount $ Cycle $ SplitN src payMode maxOutputsPerTx
    emit $ Submit era LocalSocket txParams generator
    delay
    logMsg "Splitting step: Done"

  plutusPayMode :: DstWallet -> Compiler PayMode
  plutusPayMode dst = do
    autoMode <- isPlutusAutoMode
    scriptSpec <- if autoMode
      then ScriptSpec <$> askNixOption _nix_plutusLoopScript <*> pure AutoScript
      else do
        executionUnits <- ExecutionUnits <$> askNixOption _nix_executionMemory <*> askNixOption _nix_executionSteps
        debugMode <- askNixOption _nix_debugMode
        budget <- (if debugMode then CheckScriptBudget else StaticScriptBudget)
                    <$> (ScriptDataNumber <$> askNixOption _nix_plutusData)
                    <*> (ScriptDataNumber <$> askNixOption _nix_plutusRedeemer)
                    <*> pure executionUnits
        ScriptSpec <$> askNixOption _nix_plutusScript <*> pure budget
    return $ PayToScript scriptSpec dst

-- Generate src and dst wallet names for a splitSequence.
-- testCompiler (error "opts") $ splitSequenceWalletNames (WalletName "w1") (WalletName "w2") (unfoldSplitSequence 1 1000 10000)

data Split
  = SplitWithChange Lovelace Int
  | FullSplits Int
  deriving Show

splitSequenceWalletNames :: SrcWallet -> DstWallet -> [Split] -> Compiler [ (SrcWallet, DstWallet, Split) ]
splitSequenceWalletNames _src _dst [] = return []
splitSequenceWalletNames src dst [ split ] = return [( src, dst, split )]
splitSequenceWalletNames src dst (split: rest) = do
  tempWallet <- newWallet "change_wallet"
  l <- splitSequenceWalletNames tempWallet dst rest
  return $ ( src, tempWallet, split) : l

unfoldSplitSequence :: Lovelace -> Lovelace -> Int -> [ Split ]
unfoldSplitSequence fee value outputs
  = if outputs < maxOutputsPerTx
      then [ SplitWithChange value outputs ]
      else
        let txs = outputs `divCeiling` maxOutputsPerTx
        in unfoldSplitSequence fee (value * fromIntegral maxOutputsPerTx + fee) txs ++ [ FullSplits txs ]
  where
    -- maximal number of outputs in a TX.
    -- todo: this must be in sync with Scipt/Core.hs
    divCeiling a b = case divMod a b of
     (x, 0) -> x
     (x, _rest) -> x+1

benchmarkingPhase :: WalletName -> Maybe WalletName -> Compiler WalletName
benchmarkingPhase wallet collateralWallet = do
  debugMode <- askNixOption _nix_debugMode
  targetNodes <- askNixOption _nix_targetNodes
  tps <- askNixOption _nix_tps
  era <- askNixOption _nix_era
  txCount <- askNixOption _nix_tx_count
  inputs <- askNixOption _nix_inputs_per_tx
  outputs <- askNixOption _nix_outputs_per_tx
  txParams <- askNixOption txGenTxParams
  doneWallet <- newWallet "done_wallet"
  let
    payMode = PayToAddr keyNameBenchmarkDone doneWallet
    submitMode = if debugMode
        then LocalSocket
        else Benchmark targetNodes (ThreadName "tx-submit-benchmark") tps txCount
    generator = Take txCount $ Cycle $ NtoM wallet payMode inputs outputs (Just $ txParamAddTxSize txParams) collateralWallet
  emit $ Submit era submitMode txParams generator
  unless debugMode $ do
    emit $ WaitBenchmark $ ThreadName "tx-submit-benchmark"
  return doneWallet

data Fees = Fees {
    _safeCollateral :: Lovelace
  , _minValuePerInput :: Lovelace
  }

evilFeeMagic :: Compiler Fees
evilFeeMagic = do
  (Quantity tx_fee) <- lovelaceToQuantity <$> askNixOption _nix_tx_fee
  plutusMode <- askNixOption _nix_plutusMode
  inputs_per_tx <- askNixOption _nix_inputs_per_tx
  outputs_per_tx <- askNixOption _nix_outputs_per_tx
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

logMsg :: Text -> Compiler ()
logMsg = emit . LogMsg

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

parseKey :: BS.ByteString -> TextEnvelope
parseKey x = case Base16.decode x of
  Left err -> error $ "parsing of key failed : " ++ show err
  Right addr ->  TextEnvelope
    { teType = TextEnvelopeType "PaymentSigningKeyShelley_ed25519"
    , teDescription = "Payment Signing Key"
    , teRawCBOR = addr
    }


keyNameGenesisInputFund :: KeyName
keyNameGenesisInputFund = KeyName "GenesisInputFund"

keyNameTxGenFunds :: KeyName
keyNameTxGenFunds = KeyName "TxGenFunds"

{-|
The key that is used for the very first transaction, i.e. the secure Genesis transaction.
addr_test1vzd3muund27y5nw83vymqj3a83pcuzkkejej6s75e5lfjcc85nc3p is the actual address (in Testnet 42).
It is also used as change addresse in the first splitting-step.
-}
keyTxGenFunds :: TextEnvelope
keyTxGenFunds = parseKey "5820617f846fc8b0e753bd51790de5f5a916de500175c6f5a0e27dde9da7879e1d35"

keyNameSplitPhase :: KeyName
keyNameSplitPhase = KeyName "SplitPhase"

{-|
UTxOs that are generated in intermediate splitting steps use:
addr_test1vz45dtkyzk6s3245qw8hmaddaatcx8td3pvmntl8ty7q99c22eahm
-}

keySplitPhase :: TextEnvelope
keySplitPhase = parseKey "5820cf0083c2a5d4c90ab255bc8e68f407d52eebd9408de60a0b9e4c468f9714f076"

{-|
UTxOs of the final splitting steps, i.e. the inputs of the benchmarking phase, use:
addr_test1vzj7zv9msmdasvy5nc9jhnn2gqvrvu33v5rlg332zdfrkugklxkau
(Plutus script addresses are ofc different.)
-}
keyNameBenchmarkInputs :: KeyName
keyNameBenchmarkInputs = KeyName "BenchmarkInputs"

keyBenchmarkInputs :: TextEnvelope
keyBenchmarkInputs = parseKey "58205b7f272602661d4ad3d9a4081f25fdcdcdf64fdc4892107de50e50937b77ea42"

keyNameBenchmarkDone :: KeyName
keyNameBenchmarkDone = KeyName "BenchmarkingDone"

{-|
The output of the actual benchmarking transactions use:
addr_test1vz4qz2ayucp7xvnthrx93uhha7e04gvxttpnuq4e6mx2n5gzfw23z
Query the progress of the benchmarking phase:
`cardano-node query utxo --testnet-magic 42 --address addr_test1vz4qz2ayucp7xvnthrx93uhha7e04gvxttpnuq4e6mx2n5gzfw23z`
-}

keyBenchmarkDone :: TextEnvelope
keyBenchmarkDone = parseKey "582016ca4f13fa17557e56a7d0dd3397d747db8e1e22fdb5b9df638abdb680650d50"

keyNameCollaterals :: KeyName
keyNameCollaterals = KeyName "Collaterals"

{-|
Collateral inputs for Plutus transactions:
addr_test1vpckd9muw3l4f8ne4uzumy28p0k84rvx48q46kssjkta5ng4v6sfs
-}
keyCollaterals :: TextEnvelope
keyCollaterals = parseKey "58204babdb63537ccdac393ea23d042af3b7c3587d7dc88ed3b66c959f198ad358fa"
