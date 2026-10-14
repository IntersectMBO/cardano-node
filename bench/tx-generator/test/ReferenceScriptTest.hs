{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations -Wno-missed-specialisations #-}
module ReferenceScriptTest
  ( referenceScriptTests
  ) where

import           Prelude

import           Cardano.Api (AnyCardanoEra (..), AsType (AsPaymentKey), CardanoEra (ConwayEra), ConwayEra,
                   DijkstraEra,
                   ExecutionUnits (..), HashableScriptData, InAnyCardanoEra (..),
                   LedgerProtocolParameters (..), NetworkId (..), NetworkMagic (..), PaymentKey,
                   ScriptData (ScriptDataNumber), ScriptHash, ScriptInAnyLang (..), ShelleyLedgerEra,
                   SigningKey, Tx (ShelleyTx), TxIn (..), TxIx (..), generateSigningKey, getScriptData,
                   hashScript, shelleyBasedEra, toShelleyScriptHash, toShelleyTxIn,
                   unsafeHashableScriptData)
import           Cardano.Api.Experimental qualified as Exp
import           Cardano.Api.Experimental.Tx qualified as Exp
import           Cardano.Api.Ledger qualified as L
import           Cardano.Benchmarking.Script.Aeson (readProtocolParametersFile, testJSONRoundTrip)
import           Cardano.Benchmarking.Script.Types (Action (..), Generator (..), PayMode (..),
                   ScriptBudget (..), ScriptSetPayment (..), ScriptSetSpec (..), ScriptSource (..),
                   SubmitMode (..))
import           Cardano.Benchmarking.Wallet (buildAndStore, initWallet, newCyclingDestination, payTo,
                   walletPreview)
import           Cardano.Ledger.Api qualified as LA
import           Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext)
import           Cardano.Ledger.Alonzo.UTxO (AlonzoEraUTxO, AlonzoScriptsNeeded)
import           Cardano.Ledger.BaseTypes (unboundRational)
import           Cardano.Ledger.BaseTypes.NonZero (unNonZero)
import           Cardano.Ledger.Conway.PParams (ppMinFeeRefScriptCostPerByteL)
import           Cardano.Ledger.Credential qualified as LC
import           Cardano.Ledger.Dijkstra.PParams (ppRefScriptCostMultiplierL, ppRefScriptCostStrideL)
import           Cardano.Ledger.State (EraUTxO (ScriptsNeeded, getMinFeeTxUtxo))
import           Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import           Cardano.Slotting.Slot (EpochSize (..))
import           Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import           Cardano.TxGenerator.Fund (Fund (..), FundInEra (..), getFundCoin, getFundTxIn)
import           Cardano.TxGenerator.PlutusContext (readScriptData, scriptDataModifyNumber)
import           Cardano.TxGenerator.ProtocolParameters (ProtocolParameters (..),
                   convertToLedgerProtocolParameters, fromLedgerPParams)
import           Cardano.TxGenerator.ScriptVariants
import           Cardano.TxGenerator.Setup.Plutus (preExecutePlutusScript, readPlutusScript)
import           Cardano.TxGenerator.Tx (estimateTxFee, genTx)
import           Cardano.TxGenerator.Types (TxGenPlutusType (..), TxGenTxParams (..))
import           Cardano.TxGenerator.Utils (inputsToOutputsWithFee, keyAddress, mkTxIn,
                   plutusSpendingWitness)
import           Cardano.TxGenerator.UTxO (Output (..), keyOutput, mkReferenceScriptTxOut,
                   mkTxOutToAddress, outputFund, scriptOutput)

import           Control.Monad (forM, forM_)
import           Control.Monad.Except (runExceptT)
import           Data.Foldable (toList)
import           Data.Kind (Type)
import           Data.Map.Strict qualified as Map
import           Data.Set qualified as Set
import           Data.Text qualified as Text
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Maybe (fromMaybe)
import           Lens.Micro ((^.))
import           System.FilePath ((</>))
import           Test.Tasty
import           Test.Tasty.HUnit


referenceScriptTests :: TestTree
referenceScriptTests = testGroup "reference scripts"
  [ testCase "consecutive groups of outputs are locked at one variant each, cycling across transactions" $ do
      key <- generateSigningKey AsPaymentKey
      variants <- loopV3Variants 3
      wallet <- initWallet
      destination <- newCyclingDestination wallet 2
        [ scriptOutput @ConwayEra network (variant, ScriptDataNumber 0) Exp.AnyKeyWitnessPlaceholder
        | variant <- variants ]
      txOutputHashes <- fmap concat $ forM [0 .. 2 :: Int] $ \i -> do
        let inFunds = keyFunds key (10 * i) 1
        -- Planning outputs again, as a preview does, pays nothing yet.
        _ <- payTo destination $ inputsToOutputsWithFee fee 4 $ map getFundCoin inFunds
        txOutputs <- payTo destination $ inputsToOutputsWithFee fee 4 $ map getFundCoin inFunds
        tx <- runExceptT (buildAndStore (genTx emptyConwayPParams ([], []) fee mempty) inFunds txOutputs)
                >>= either (assertFailure . show) pure
        return $ map outputScriptHash $ toList $ ledgerTxOf tx ^. LA.bodyTxL . LA.outputsTxBodyL
      let variantIndex = Map.fromList $ zip (map (Just . toShelleyScriptHash . scriptHashOf) variants) [0 :: Int ..]
      map (`Map.lookup` variantIndex) txOutputHashes @?= map Just [0, 0, 1, 1, 2, 2, 0, 0, 1, 1, 2, 2]
      walletPreview wallet 100 >>= (@?= 12) . length

  , testCase "script set actions survive a JSON round trip" $
      testJSONRoundTrip scriptSetActions @?= Nothing

  , testCase "reference script fees survive the conversion to and from ledger parameters" $ do
      conway <- readProtocolParametersFile ("data" </> "protocol-parameters-v10.json")
      dijkstra <- readProtocolParametersFile ("data" </> "protocol-parameters-v11.json")
      conwayLedger <- either (assertFailure . show) (pure . unLedgerProtocolParameters) $
        convertToLedgerProtocolParameters (shelleyBasedEra @ConwayEra) (withRefScriptFee conway)
      dijkstraLedger <- either (assertFailure . show) (pure . unLedgerProtocolParameters) $
        convertToLedgerProtocolParameters (shelleyBasedEra @DijkstraEra) (withRefScriptTiers $ withRefScriptFee dijkstra)
      unboundRational (conwayLedger ^. ppMinFeeRefScriptCostPerByteL) @?= 15
      unboundRational (dijkstraLedger ^. ppMinFeeRefScriptCostPerByteL) @?= 15
      unNonZero (dijkstraLedger ^. ppRefScriptCostStrideL) @?= 25_600
      unboundRational (dijkstraLedger ^. ppRefScriptCostMultiplierL) @?= 6 / 5
      fromLedgerPParams (shelleyBasedEra @ConwayEra) conwayLedger
        @?= withRefScriptFee (fromLedgerPParams (shelleyBasedEra @ConwayEra) conwayLedger)
      let fromDijkstra = fromLedgerPParams (shelleyBasedEra @DijkstraEra) dijkstraLedger
      (protocolParamMinFeeRefScriptCostPerByte fromDijkstra, protocolParamRefScriptCostStride fromDijkstra,
       protocolParamRefScriptCostMultiplier fromDijkstra) @?= (Just 15, Just 25_600, Just (6 / 5))

  , spendingTests @ConwayEra "Conway" "protocol-parameters-v10.json" withRefScriptFee
  , spendingTests @DijkstraEra "Dijkstra" "protocol-parameters-v11.json" (withRefScriptTiers . withRefScriptFee)
  ]

-- | Actions using the script set constructors, as a generator script would.
scriptSetActions :: [Action]
scriptSetActions =
  [ DefineScriptSet "loops" (ScriptSetSpec (Left "LoopV3") 20 (ScriptSizeFixed 4_096))
  , DefineScriptSet "loops-minimal" (ScriptSetSpec (Right "LoopV3.plutus") 1 ScriptSizeMinimal)
  , Submit (AnyCardanoEra ConwayEra) LocalSocket txParams $
      PublishScripts "genesis" "loops" "ReferenceScripts" (PayToAddr "Funds" "genesis")
  , Submit (AnyCardanoEra ConwayEra) LocalSocket txParams $ SplitN "funds"
      (PayToScriptSet (ScriptSetPayment "loops" (AutoScript "LoopV3.redeemer.json" 2) LimitSaturationLoop ScriptByReference 2) "script_funds")
      30
  , Submit (AnyCardanoEra ConwayEra) LocalSocket txParams $ SplitN "funds"
      (PayToScriptSet (ScriptSetPayment "loops" (AutoScript "LoopV3.redeemer.json" 1) LimitTxPerBlock_8 ScriptInWitness 1) "script_funds")
      30
  ]
 where
  txParams = TxGenTxParams { txParamFee = 3_000_000, txParamAddTxSize = 100, txParamTTL = 1_000_000 }

-- | The script hash an output is locked by, if any.
outputScriptHash :: LA.EraTxOut era => L.TxOut era -> Maybe LA.ScriptHash
outputScriptHash out = case out ^. LA.addrTxOutL of
  LA.Addr _ (LC.ScriptHashObj h) _ -> Just h
  _ -> Nothing

-- | Spending an output locked by a script variant, with the variant in the
-- transaction or by reference, checked against the ledger.
spendingTests :: forall era. Exp.EraCommonConstraints era
  => String -> FilePath -> (ProtocolParameters -> ProtocolParameters) -> TestTree
spendingTests eraName pparamsFile adjust = testGroup eraName
  [ testCase "by reference: a reference input, no script in the witnesses, and the ledger charges for it" $ do
      Spending{..} <- spending @era pparamsFile adjust True
      let ledgerTx = ledgerTxOf spendingTx
          referenceInputs = ledgerTx ^. LA.bodyTxL . LA.referenceInputsTxBodyL
          witnessScripts = ledgerTx ^. LA.witsTxL . LA.scriptTxWitsL
          ledgerMinFee = getMinFeeTxUtxo spendingPParams ledgerTx spendingUTxO
      referenceInputs @?= Set.singleton (toShelleyTxIn spendingReference)
      Map.size witnessScripts @?= 0
      redeemerCount ledgerTx @?= 1
      assertBool ("the ledger does not charge for the reference script: " ++ show ledgerMinFee) $
        ledgerMinFee > estimateTxFee spendingPParams spendingTx 1 0
      assertBool ("the estimate is below the ledger's minimum fee: " ++ show ledgerMinFee) $
        estimateTxFee spendingPParams spendingTx 1 spendingScriptSize >= ledgerMinFee
      scriptsSucceed spendingPParams ledgerTx spendingUTxO spendingUnits
      balanced spendingPParams ledgerTx spendingUTxO

  , testCase "in the witnesses: no reference input, and the script in the witnesses" $ do
      Spending{..} <- spending @era pparamsFile adjust False
      let ledgerTx = ledgerTxOf spendingTx
          referenceInputs = ledgerTx ^. LA.bodyTxL . LA.referenceInputsTxBodyL
          witnessScripts = ledgerTx ^. LA.witsTxL . LA.scriptTxWitsL
          ledgerMinFee = getMinFeeTxUtxo spendingPParams ledgerTx spendingUTxO
      referenceInputs @?= Set.empty
      Map.keys witnessScripts @?= [toShelleyScriptHash spendingScriptHash]
      redeemerCount ledgerTx @?= 1
      assertBool ("the estimate is below the ledger's minimum fee: " ++ show ledgerMinFee) $
        estimateTxFee spendingPParams spendingTx 1 0 >= ledgerMinFee
      scriptsSucceed spendingPParams ledgerTx spendingUTxO spendingUnits
      balanced spendingPParams ledgerTx spendingUTxO
  ]

redeemerCount :: (LA.EraTx era, LA.AlonzoEraTxWits era) => L.Tx l era -> Int
redeemerCount ledgerTx = Map.size $ LA.unRedeemers $ ledgerTx ^. LA.witsTxL . LA.rdmrsTxWitsL

type Spending :: Type -> Type
data Spending era = Spending
  { spendingPParams     :: L.PParams (ShelleyLedgerEra era)
  , spendingTx          :: Tx era
  , spendingUTxO        :: L.UTxO (ShelleyLedgerEra era)
  , spendingReference   :: TxIn
  , spendingScriptHash  :: ScriptHash
  , spendingScriptSize  :: Int
  , spendingUnits       :: ExecutionUnits
  }

-- | A transaction spending an output locked by variant 1 of three, with the
-- variant's reference output and the collateral in the UTxO.
spending :: forall era. Exp.EraCommonConstraints era
  => FilePath -> (ProtocolParameters -> ProtocolParameters) -> Bool -> IO (Spending era)
spending pparamsFile adjust byReference = do
  generatorPParams <- adjust <$> readProtocolParametersFile ("data" </> pparamsFile)
  pparams <- either (assertFailure . show) (pure . unLedgerProtocolParameters) $
    convertToLedgerProtocolParameters (shelleyBasedEra @era) generatorPParams
  key <- generateSigningKey AsPaymentKey
  variant <- (!! 1) <$> loopV3Variants 3
  (datum, redeemer, units) <- budget generatorPParams variant
  refOut <- either (assertFailure . show) pure $ mkReferenceScriptTxOut pparams (keyAddress @era network key) variant
  witness <- either (assertFailure . show) pure $
    plutusSpendingWitness @era variant (if byReference then Just refTxIn else Nothing) datum redeemer units
  let locked = scriptOutput @era network (variant, getScriptData datum) witness 20_000_000
      lockedOut = outputTxOut locked
      lockedFund = outputFund lockTxId (TxIx 0) locked
      collateral = keyOutput @era network key 10_000_000
      collateralOut = outputTxOut collateral
      collateralFund = outputFund collateralTxId (TxIx 0) collateral
      utxo = L.UTxO $ Map.fromList
        [ (toShelleyTxIn refTxIn, unTxOut refOut)
        , (toShelleyTxIn $ getFundTxIn lockedFund, unTxOut lockedOut)
        , (toShelleyTxIn $ getFundTxIn collateralFund, unTxOut collateralOut) ]
  (tx, _) <- either (assertFailure . show) pure $
    genTx pparams ([getFundTxIn collateralFund], [collateralFund]) spendingFee mempty [lockedFund]
      [mkTxOutToAddress (keyAddress @era network key) (20_000_000 - spendingFee)]
  return Spending
    { spendingPParams = pparams
    , spendingTx = tx
    , spendingUTxO = utxo
    , spendingReference = refTxIn
    , spendingScriptHash = scriptHashOf variant
    , spendingScriptSize = fromMaybe 0 $ plutusScriptSize variant
    , spendingUnits = units
    }
 where
  refTxIn = mkTxIn "1111111111111111111111111111111111111111111111111111111111111111#0"
  TxIn lockTxId _ = mkTxIn "2222222222222222222222222222222222222222222222222222222222222222#0"
  TxIn collateralTxId _ = mkTxIn "3333333333333333333333333333333333333333333333333333333333333333#0"
  unTxOut :: Exp.TxOut (ShelleyLedgerEra era) -> L.TxOut (ShelleyLedgerEra era)
  unTxOut (Exp.TxOut out) = out
  spendingFee :: L.Coin
  spendingFee = 2_000_000

-- | The datum, redeemer and execution units for a loop script, with the loop
-- counter where the generator's automatic budgeting starts it.
budget :: ProtocolParameters -> ScriptInAnyLang -> IO (HashableScriptData, HashableScriptData, ExecutionUnits)
budget pparams script = do
  redeemerFile <- readScriptData ("data" </> "LoopV3.redeemer.json") >>= either (assertFailure . show) pure
  let datum = unsafeHashableScriptData $ ScriptDataNumber 0
      redeemer = unsafeHashableScriptData $ scriptDataModifyNumber (const 1_000_000) $ getScriptData redeemerFile
  units <- either (assertFailure . show) pure $ preExecutePlutusScript pparams script (getScriptData datum) redeemer
  return (datum, redeemer, units)

-- | The reference script fee of mainnet; the generator's parameter files do
-- not set it.
withRefScriptFee :: ProtocolParameters -> ProtocolParameters
withRefScriptFee pparams = pparams { protocolParamMinFeeRefScriptCostPerByte = Just 15 }

-- | The reference script fee tiers, which are protocol parameters since Dijkstra,
-- as Conway fixes them.
withRefScriptTiers :: ProtocolParameters -> ProtocolParameters
withRefScriptTiers pparams = pparams
  { protocolParamRefScriptCostStride = Just 25_600
  , protocolParamRefScriptCostMultiplier = Just (6 / 5)
  }

-- | Every script of the transaction runs successfully, within its stated units.
scriptsSucceed :: forall era. ()
  => AlonzoEraUTxO era
  => LA.AlonzoEraTx era
  => EraPlutusContext era
  => ScriptsNeeded era ~ AlonzoScriptsNeeded era
  => L.PParams era -> L.Tx L.TopTx era -> L.UTxO era -> ExecutionUnits -> Assertion
scriptsSucceed pparams ledgerTx utxo (ExecutionUnits steps memory) = do
  let report = LA.evalTxExUnits pparams ledgerTx utxo epochInfo systemStart
  Map.size report @?= 1
  forM_ (Map.elems report) $ \case
    Left failure -> assertFailure $ "script failed: " ++ show failure
    Right (L.ExUnits memoryUsed stepsUsed) ->
      assertBool "the script needs more than its stated units" $
        stepsUsed <= steps && memoryUsed <= memory
 where
  epochInfo :: EpochInfo (Either Text.Text)
  epochInfo = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)
  systemStart :: SystemStart
  systemStart = SystemStart $ posixSecondsToUTCTime 0

-- | The transaction's inputs pay exactly for its outputs and fee.
balanced :: EraUTxO era => L.PParams era -> L.Tx L.TopTx era -> L.UTxO era -> Assertion
balanced pparams ledgerTx utxo =
  LA.evalBalanceTxBody pparams (const Nothing) (const False) utxo (ledgerTx ^. LA.bodyTxL) @?= mempty

loopV3Variants :: Int -> IO [ScriptInAnyLang]
loopV3Variants count = do
  base <- readPlutusScript (Right $ "scripts-fallback" </> "LoopV3.plutus") >>= either (assertFailure . show) (pure . fst)
  either (assertFailure . show) pure $ scriptVariants (ScriptSizeFixed 4_096) count base

scriptHashOf :: ScriptInAnyLang -> ScriptHash
scriptHashOf (ScriptInAnyLang _ script) = hashScript script

ledgerTxOf :: Tx era -> L.Tx L.TopTx (ShelleyLedgerEra era)
ledgerTxOf (ShelleyTx _ ledgerTx) = ledgerTx

network :: NetworkId
network = Testnet (NetworkMagic 42)

fee :: L.Coin
fee = 1_000_000

emptyConwayPParams :: L.PParams LA.ConwayEra
emptyConwayPParams = LA.emptyPParams

-- | @n@ funds of 10 ada each, spendable with @key@, at distinct inputs from @first@ on.
keyFunds :: SigningKey PaymentKey -> Int -> Int -> [Fund]
keyFunds key first n =
  [ Fund $ InAnyCardanoEra ConwayEra FundInEra
      { _fundTxIn       = mkTxIn $ "900fc5da77a0747da53f7675cbb7d149d46779346dea2f879ab811ccc72a2162#" <> Text.pack (show i)
      , _fundWitness    = Exp.AnyKeyWitnessPlaceholder
      , _fundVal        = 10_000_000
      , _fundSigningKey = Just key
      }
  | i <- [first .. first + n - 1]
  ]
