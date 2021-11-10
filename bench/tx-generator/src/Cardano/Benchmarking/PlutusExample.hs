{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.Benchmarking.PlutusExample
where
import           Prelude
import qualified Data.Map as Map

import           Control.Monad.Trans.Except
import qualified Data.ByteString.Char8 as BSC

import           Cardano.CLI.Shelley.Script (readFileScriptInAnyLang)

import           Cardano.Api
import           Cardano.Api.Shelley (ProtocolParameters, PlutusScript(..), fromAlonzoExUnits, protocolParamCostModels, toPlutusData)
import           Cardano.Ledger.Alonzo.TxInfo (exBudgetToExUnits)
import           Cardano.Benchmarking.FundSet
import           Cardano.Benchmarking.Wallet

import qualified Plutus.V1.Ledger.Api as Plutus
import           Plutus.V1.Ledger.Contexts (ScriptContext(..), ScriptPurpose(..), TxInfo(..), TxOutRef(..))

mkUtxoScript ::
     NetworkId
  -> SigningKey PaymentKey
  -> (FilePath, Script PlutusScriptV1, ScriptData)
  -> Validity
  -> ToUTxO AlonzoEra
mkUtxoScript networkId key (scriptFile, script, txOutDatum) validity values
  = ( map mkTxOut values
    , newFunds
    )
 where
  mkTxOut v = TxOut plutusScriptAddr (mkTxOutValueAdaOnly v) (TxOutDatumHash ScriptDataInAlonzoEra $ hashScriptData txOutDatum)

  plutusScriptAddr = makeShelleyAddressInEra
                       networkId
                       (PaymentCredentialByScript $ hashScript script)
                       NoStakeAddress

  newFunds txId = zipWith (mkNewFund txId) [TxIx 0 ..] values

  mkNewFund :: TxId -> TxIx -> Lovelace -> Fund
  mkNewFund txId txIx val = Fund $ InAnyCardanoEra AlonzoEra $ FundInEra {
      _fundTxIn = TxIn txId txIx
    , _fundVal = mkTxOutValueAdaOnly val
    , _fundSigningKey = key
    , _fundValidity = validity
    , _fundVariant = PlutusScriptFund scriptFile txOutDatum
    }

readScript :: FilePath -> IO (Script PlutusScriptV1)
readScript fp = do
  res <- runExceptT $ readFileScriptInAnyLang fp
  case res of
    Left err -> do
      print err
      error $ show err
    Right (ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV1) script) -> return script
    Right _otherScript ->
      error "Wrong script version."

toScriptHash :: String -> Hash ScriptData
toScriptHash str
  = case deserialiseFromRawBytesHex (AsHash AsScriptData) (BSC.pack str) of
    Just x -> x
    Nothing  -> error $ "Invalid datum hash: " ++ show str

genTxPlutusSpend ::
     ProtocolParameters
  -> [Fund]
  -> ScriptWitness WitCtxTxIn AlonzoEra
  -> TxFee AlonzoEra
  -> TxMetadataInEra AlonzoEra
  -> TxGenerator AlonzoEra
genTxPlutusSpend protocolParameters collateral scriptWitness fee metadata inFunds outputs
  = case makeTransactionBody txBodyContent of
      Left err -> error $ show err
      Right b -> Right ( signShelleyTransaction b (map (WitnessPaymentKey . getFundKey) inFunds)
                       , getTxId b
                       )
 where
  txBodyContent = TxBodyContent {
      txIns = map (\f -> (getFundTxIn f, BuildTxWith $ ScriptWitness ScriptWitnessForSpending scriptWitness )) inFunds
    , txInsCollateral = TxInsCollateral CollateralInAlonzoEra $  map getFundTxIn collateral
    , txOuts = outputs
    , txFee = fee
    , txValidityRange = (TxValidityNoLowerBound, TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra)
    , txMetadata = metadata
    , txAuxScripts = TxAuxScriptsNone
    , txExtraKeyWits = TxExtraKeyWitnessesNone
    , txProtocolParams = BuildTxWith $ Just protocolParameters
    , txWithdrawals = TxWithdrawalsNone
    , txCertificates = TxCertificatesNone
    , txUpdateProposal = TxUpdateProposalNone
    , txMintValue = TxMintNone
    , txScriptValidity = TxScriptValidityNone
    }

preExecuteScript ::
     ProtocolParameters
  -> Script PlutusScriptV1
  -> ScriptData
  -> ScriptData
  -> Either String ExecutionUnits
preExecuteScript protocolParameters (PlutusScript _ (PlutusScriptSerialised script)) datum redeemer = do
  costModel <- case Map.lookup (AnyPlutusScriptVersion PlutusScriptV1) (protocolParamCostModels protocolParameters) of
    Just (CostModel x) -> Right x
    Nothing -> Left "costModel unavailable"
  let (_logout, res) = Plutus.evaluateScriptCounting Plutus.Verbose costModel script
                              [ toPlutusData datum
                              , toPlutusData redeemer
                              , Plutus.toData dummyContext ]
  case res of
     Left err -> Left $ show err
     Right exBudget -> case exBudgetToExUnits exBudget of
       Just x -> Right $ fromAlonzoExUnits x
       Nothing -> Left "exBudgetToExUnits exBudget == Nothing"
  where
    dummyContext :: ScriptContext
    dummyContext = ScriptContext dummyTxInfo (Spending dummyOutRef)

    dummyOutRef :: TxOutRef
    dummyOutRef = TxOutRef (Plutus.TxId "") 0
    dummyTxInfo :: TxInfo
    dummyTxInfo = TxInfo
      { txInfoInputs = []
      , txInfoOutputs = []
      , txInfoFee = mempty
      , txInfoMint = mempty
      , txInfoDCert = []
      , txInfoWdrl = []
      , txInfoValidRange = Plutus.always
      , txInfoSignatories = []
      , txInfoData = []
      , txInfoId = Plutus.TxId ""
      }
