{-# LANGUAGE GADTs #-}

module Test.PlutusExample.Gen where

import           Cardano.Api
import           Cardano.Api.Shelley
import           Prelude

import qualified Data.Map.Strict as Map

import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxInfo as Alonzo
-- import           Cardano.Ledger.BaseTypes (ProtoVer)
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Era as Ledger
import           Cardano.PlutusExample.ScriptContextChecker
import           Gen.Cardano.Api.Typed
import qualified Ledger as Plutus
import qualified Plutus.V1.Ledger.DCert as Plutus
import qualified Cardano.Ledger.Shelley.UTxO as Ledger
import qualified Cardano.Ledger.TxIn as Ledger

import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


genPlutusTxOut :: Gen Plutus.TxOut
genPlutusTxOut = do
  alonzoTxOut <-
    TxOut <$> (shelleyAddressInEra <$> genAddressShelley)
          <*> genTxOutValue AlonzoEra
          <*> genTxOutDatumHash AlonzoEra
  Gen.just . return . Alonzo.txInfoOut
    $ toShelleyTxOut ShelleyBasedEraAlonzo (toCtxUTxOTxOut alonzoTxOut)

genMyCustomRedeemer :: Gen MyCustomRedeemer
genMyCustomRedeemer =
  MyCustomRedeemer
    <$> Gen.list (Range.singleton 1) genPlutusTxOut
    <*> return mempty --TODO: Investigate why genTxInfoIn generates Nothing
    <*> (Alonzo.transValue . toMaryValue <$> genValueForMinting)
    <*> genPOSIXTimeRange
    <*> (Alonzo.transValue . toMaryValue <$> genValueForTxOut)
    <*> genDatumMap
    <*> Gen.list (Range.constant 0 2) genPlutusCert
    <*> Gen.list (Range.constant 0 2) genReqSigners
    <*> return Nothing

genTxInfoIn :: Gen Plutus.TxInInfo
genTxInfoIn = do
  txinput <- genTxIn
  txout <- genTxOut AlonzoEra
  lUTxO <- genLedgerUTxO ShelleyBasedEraAlonzo (txinput, txout)
  let mTxInfoIn = Alonzo.txInfoIn lUTxO (toShelleyTxIn txinput)
  case mTxInfoIn of
    Just txin -> return txin
    Nothing -> error $ "Utxo: " ++ show lUTxO ++ "\n" ++ "Txin: " ++ show txinput

genReqSigners :: Gen Plutus.PubKeyHash
genReqSigners = do
  PaymentKeyHash kh <- genVerificationKeyHash AsPaymentKey
  return $ Alonzo.transKeyHash kh

genLedgerUTxO
  :: (Ledger.Crypto (ShelleyLedgerEra era) ~ StandardCrypto)
  => ShelleyBasedEra era
  -> (TxIn, TxOut CtxTx era)
  -> Gen (Ledger.UTxO (ShelleyLedgerEra era))
genLedgerUTxO sbe (txin, out) = do
  UTxO utxoMap <- genUTxO (shelleyBasedToCardanoEra sbe)
  return . toLedgerUTxO sbe . UTxO $ Map.insert txin (toCtxUTxOTxOut out) utxoMap

genPlutusCert :: Gen Plutus.DCert
genPlutusCert = Alonzo.transDCert . toShelleyCertificate <$> genCertificate

genLedgerTxIn :: Gen (Ledger.TxIn StandardCrypto)
genLedgerTxIn = toShelleyTxIn <$> genTxIn

genPlutusTxId :: Gen Plutus.TxId
genPlutusTxId =
  Alonzo.txInfoId . toShelleyTxId <$> genTxId

genDatumMap :: Gen [(Plutus.DatumHash, Plutus.Datum)]
genDatumMap =
  map Alonzo.transDataPair <$> Gen.list (Range.linear 0 5) genDatumHashTuple

genDatumHashTuple :: Gen (Alonzo.DataHash StandardCrypto, Alonzo.Data ledgerera)
genDatumHashTuple = do
  sData <- genScriptData
  let ScriptDataHash h = hashScriptData sData
  return (h, toAlonzoData sData)

genPOSIXTimeRange :: Gen Plutus.POSIXTimeRange
genPOSIXTimeRange = do
  ptime <- Plutus.POSIXTime <$> Gen.integral (Range.linear 0 10)
  Gen.element [ Plutus.to ptime
              , Plutus.always
              , Plutus.never
              , Plutus.singleton ptime
              ]
