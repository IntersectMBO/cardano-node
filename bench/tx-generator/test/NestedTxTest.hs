{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations -Wno-missed-specialisations #-}
module NestedTxTest
  ( nestedTxTests
  ) where

import           Prelude

import           Cardano.Api (AsType (AsPaymentKey), CardanoEra (DijkstraEra), DijkstraEra,
                   ExecutionUnits, HashableScriptData, InAnyCardanoEra (..),
                   LedgerProtocolParameters (..), NetworkId (..), NetworkMagic (..), PaymentKey,
                   ScriptData (ScriptDataNumber), ScriptInAnyLang, SigningKey, TxId, TxIn (..),
                   TxIx (..), TxMetadata (..), TxMetadataInEra (..), TxMetadataValue (..),
                   fromShelleyTxId, generateSigningKey, getScriptData, serialiseToRawBytes,
                   shelleyBasedEra, toShelleyAddr, toShelleyTxIn, unsafeHashableScriptData)
import           Cardano.Api.Experimental qualified as Exp
import           Cardano.Api.Experimental.Tx qualified as Exp
import           Cardano.Api.Ledger qualified as L
import           Cardano.Benchmarking.Script.Aeson (readProtocolParametersFile)
import           Cardano.Ledger.Api qualified as LA
import           Cardano.Ledger.Dijkstra.TxBody (DijkstraEraTxBody (subTransactionsTxBodyL))
import           Cardano.TxGenerator.Fund (Fund (..), FundInEra (..), getFundCoin, getFundTxIn)
import           Cardano.TxGenerator.PlutusContext (readScriptData, scriptDataModifyNumber)
import           Cardano.TxGenerator.ProtocolParameters (ProtocolParameters,
                   convertToLedgerProtocolParameters)
import           Cardano.TxGenerator.Setup.Plutus (mkPlutusSpendingWitness, preExecutePlutusScript,
                   readPlutusScript, toAnyPlutusScript)
import           Cardano.TxGenerator.Tx (genNestedTx, genTx, nestedTx)
import           Cardano.TxGenerator.Utils (inputsToOutputsWithFee, keyAddress, mkTxIn)
import           Cardano.TxGenerator.UTxO (keyOutput, mkReferenceScriptTxOut, mkTxOutToAddress,
                   outputFund, outputFunds, outputTxOut, scriptOutput)

import           Data.Foldable (toList)
import           Data.Map.Strict qualified as Map
import           Data.Set qualified as Set
import           Data.Text qualified as Text
import           Lens.Micro ((^.))
import           System.FilePath ((</>))
import           Test.Tasty
import           Test.Tasty.HUnit


nestedTxTests :: TestTree
nestedTxTests = testGroup "nested transactions (Dijkstra)"
  [ -- genNestedTx builds with the experimental API, genTx with the Compatible
    -- one; without sub-transactions they must build the same transaction.
    testGroup "without sub-transactions, the same transaction as genTx"
      [ testCase "key inputs" $ do
          key <- generateSigningKey AsPaymentKey
          sameAsGenTx pparams ([], []) noMetadata (mkFunds key 0 2) (map (txOut key) [9_500_000, 9_500_000])
      , testCase "key inputs, metadata" $ do
          key <- generateSigningKey AsPaymentKey
          sameAsGenTx pparams ([], []) someMetadata (mkFunds key 0 2) (map (txOut key) [9_500_000, 9_500_000])
      , testCase "key inputs, metadata, a reference script output" $ do
          key <- generateSigningKey AsPaymentKey
          v11 <- ledgerPParams "protocol-parameters-v11.json"
          script <- loadScript "LoopV3"
          refOut <- either (assertFailure . show) pure $ mkReferenceScriptTxOut v11 (keyAddress @DijkstraEra network key) script
          sameAsGenTx v11 ([], []) someMetadata (mkFunds key 0 3) (map (txOut key) [9_000_000, 9_000_000] ++ [refOut])
      , testCase "LoopV3 by reference" $ plutusCase "protocol-parameters-v11.json" "LoopV3" True
      , testCase "LoopV3 in the witnesses" $ plutusCase "protocol-parameters-v11.json" "LoopV3" False
      , testCase "LoopV4 by reference" $ plutusCase "protocol-parameters-v12.json" "LoopV4" True
      , testCase "LoopV4 in the witnesses" $ plutusCase "protocol-parameters-v12.json" "LoopV4" False
      ]

  , testCase "sub-transactions are included, with distinct ids, and the batch balances" $ do
      key <- generateSigningKey AsPaymentKey
      let topFunds = mkFunds key 0 2
          subTxFunds = [ mkFunds key (10 * i) 2 | i <- [1 .. 3] ]
          subTx :: [Fund] -> ([Fund], [Exp.TxOut LA.DijkstraEra])
          subTx funds = (funds, map (txOut key) [10_000_000, 10_000_000])
      (tx, _, subTxIds) <- either (assertFailure . show) pure $
        genNestedTx pparams ([], []) fee mempty topFunds (map (txOut key) [9_500_000, 9_500_000])
          (map subTx subTxFunds)
      let ledgerTx = ledgerTxOf tx
          includedIds = map (fromShelleyTxId . LA.txIdTx) $ toList $ ledgerTx ^. LA.bodyTxL . subTransactionsTxBodyL
          utxo :: L.UTxO LA.DijkstraEra
          utxo = L.UTxO $ Map.fromList
            [ (toShelleyTxIn $ getFundTxIn f, L.mkBasicTxOut (addr key) (L.inject $ getFundCoin f))
            | f <- topFunds ++ concat subTxFunds ]
      length includedIds @?= 3
      includedIds @?= subTxIds
      Set.size (Set.fromList subTxIds) @?= 3
      LA.evalBalanceTxBody pparams (const Nothing) (const False) utxo (ledgerTx ^. LA.bodyTxL) @?= mempty

  , testCase "the outputs of a sub-transaction are created under its own id" $ do
      key <- generateSigningKey AsPaymentKey
      let outputs fee' n funds = map (keyOutput @DijkstraEra network key) $ inputsToOutputsWithFee fee' n $ map getFundCoin funds
          topFunds = mkFunds key 0 2
          subTxFunds = [ mkFunds key (10 * i) 2 | i <- [1, 2] ]
      (tx, created) <- either (assertFailure . show) pure $
        nestedTx outputTxOut (genNestedTx pparams ([], []) fee mempty)
          (topFunds, outputs fee 2 topFunds) [ (funds, outputs 0 3 funds) | funds <- subTxFunds ]
      let createdTxIns = map getFundTxIn $ concatMap (uncurry outputFunds) created
          ledgerTx = ledgerTxOf tx
          topId = fromShelleyTxId $ LA.txIdTx ledgerTx
          subIds = map (fromShelleyTxId . LA.txIdTx) $ toList $ ledgerTx ^. LA.bodyTxL . subTransactionsTxBodyL
      length subIds @?= 2
      createdTxIns @?= txIns topId 2 ++ concatMap (`txIns` 3) subIds
  ]
 where
  txIns :: TxId -> Int -> [TxIn]
  txIns txId n = [ TxIn txId (TxIx $ fromIntegral i) | i <- [0 .. n - 1] ]

-- | 'genNestedTx' without sub-transactions builds the same bytes as 'genTx'.
sameAsGenTx :: L.PParams LA.DijkstraEra -> ([TxIn], [Fund]) -> TxMetadata -> [Fund] -> [Exp.TxOut LA.DijkstraEra] -> Assertion
sameAsGenTx ledgerParameters collateral metadata funds outputs = do
  plain <- either (assertFailure . show) (pure . fst) $
    genTx ledgerParameters collateral fee (metadataInEra metadata) funds outputs
  nested <- either (assertFailure . show) (pure . fst3) $
    genNestedTx ledgerParameters collateral fee metadata funds outputs []
  serialiseToRawBytes nested @?= serialiseToRawBytes plain
 where
  fst3 :: (a, b, c) -> a
  fst3 (a, _, _) = a
  metadataInEra :: TxMetadata -> TxMetadataInEra DijkstraEra
  metadataInEra m
    | m == mempty = TxMetadataNone
    | otherwise   = TxMetadataInEra (shelleyBasedEra @DijkstraEra) m

-- | Spending an output locked by a loop script, by reference or with the
-- script in the witnesses, with collateral and metadata.
plutusCase :: FilePath -> String -> Bool -> Assertion
plutusCase pparamsFile scriptName byReference = do
  generatorPParams <- readProtocolParametersFile ("data" </> pparamsFile)
  ledgerParameters <- ledgerPParams pparamsFile
  key <- generateSigningKey AsPaymentKey
  script <- loadScript scriptName
  (datum, redeemer, units) <- budget generatorPParams scriptName script
  witness <- spendingWitness script (if byReference then Just refTxIn else Nothing) datum redeemer units
  lockedOutput <- either (assertFailure . show) pure $ scriptOutput @DijkstraEra network (script, getScriptData datum) witness
  let locked = outputFund lockTxId (TxIx 0) (lockedOutput 20_000_000)
      collateral = outputFund collateralTxId (TxIx 0) (keyOutput @DijkstraEra network key 10_000_000)
  sameAsGenTx ledgerParameters ([getFundTxIn collateral], [collateral]) someMetadata [locked] [txOut key (20_000_000 - fee)]
 where
  refTxIn = mkTxIn "1111111111111111111111111111111111111111111111111111111111111111#0"
  TxIn lockTxId _ = mkTxIn "2222222222222222222222222222222222222222222222222222222222222222#0"
  TxIn collateralTxId _ = mkTxIn "3333333333333333333333333333333333333333333333333333333333333333#0"

spendingWitness :: ScriptInAnyLang -> Maybe TxIn -> HashableScriptData -> HashableScriptData -> ExecutionUnits
  -> IO (Exp.AnyWitness LA.DijkstraEra)
spendingWitness script reference datum redeemer units = do
  anyPlutusScript <- either (assertFailure . show) pure $ toAnyPlutusScript (shelleyBasedEra @DijkstraEra) script
  pure $ mkPlutusSpendingWitness anyPlutusScript reference datum redeemer units

-- | The datum, redeemer and execution units for a loop script, with the loop
-- counter where the generator's automatic budgeting starts it.
budget :: ProtocolParameters -> String -> ScriptInAnyLang -> IO (HashableScriptData, HashableScriptData, ExecutionUnits)
budget generatorPParams scriptName script = do
  redeemerFile <- readScriptData ("data" </> (scriptName ++ ".redeemer.json")) >>= either (assertFailure . show) pure
  let datum = unsafeHashableScriptData $ ScriptDataNumber 0
      redeemer = unsafeHashableScriptData $ scriptDataModifyNumber (const 1_000_000) $ getScriptData redeemerFile
  units <- either (assertFailure . show) pure $ preExecutePlutusScript generatorPParams script (getScriptData datum) redeemer
  return (datum, redeemer, units)

ledgerPParams :: FilePath -> IO (L.PParams LA.DijkstraEra)
ledgerPParams file = do
  generatorPParams <- readProtocolParametersFile ("data" </> file)
  either (assertFailure . show) (pure . unLedgerProtocolParameters) $
    convertToLedgerProtocolParameters (shelleyBasedEra @DijkstraEra) generatorPParams

loadScript :: String -> IO ScriptInAnyLang
loadScript name =
  readPlutusScript (Right $ "scripts-fallback" </> (name ++ ".plutus")) >>= either (assertFailure . show) (pure . fst)

ledgerTxOf :: Exp.SignedTx DijkstraEra -> L.Tx L.TopTx LA.DijkstraEra
ledgerTxOf (Exp.SignedTx ledgerTx) = ledgerTx

network :: NetworkId
network = Testnet (NetworkMagic 42)

fee :: L.Coin
fee = 1_000_000

noMetadata :: TxMetadata
noMetadata = mempty

someMetadata :: TxMetadata
someMetadata = TxMetadata $ Map.fromList [ (0, TxMetaBytes "tx-generator"), (1, TxMetaNumber 42) ]

pparams :: L.PParams LA.DijkstraEra
pparams = LA.emptyPParams

addr :: SigningKey PaymentKey -> L.Addr
addr key = toShelleyAddr (keyAddress @DijkstraEra network key)

txOut :: SigningKey PaymentKey -> L.Coin -> Exp.TxOut LA.DijkstraEra
txOut key = mkTxOutToAddress (keyAddress @DijkstraEra network key)

-- | @n@ funds of 10 ada each, spendable with @key@, at distinct inputs from @first@ on.
mkFunds :: SigningKey PaymentKey -> Int -> Int -> [Fund]
mkFunds key first n =
  [ Fund $ InAnyCardanoEra DijkstraEra FundInEra
      { _fundTxIn       = mkTxIn $ "900fc5da77a0747da53f7675cbb7d149d46779346dea2f879ab811ccc72a2162#" <> Text.pack (show i)
      , _fundWitness    = Exp.AnyKeyWitnessPlaceholder
      , _fundVal        = 10_000_000
      , _fundSigningKey = Just key
      }
  | i <- [first .. first + n - 1]
  ]
