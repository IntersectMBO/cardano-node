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
import           Cardano.Api.Shelley ( ProtocolParameters(..), PlutusScript(..), ReferenceScript(..)
                                     , fromAlonzoExUnits, protocolParamCostModels, toPlutusData)
import           Cardano.Ledger.Alonzo.TxInfo (exBudgetToExUnits)
import           Cardano.Benchmarking.FundSet
import           Cardano.Benchmarking.Wallet

import qualified Plutus.V1.Ledger.Api as Plutus
import           Plutus.V1.Ledger.Contexts (ScriptContext(..), ScriptPurpose(..), TxInfo(..), TxOutRef(..))

mkUtxoScript ::
     NetworkId
  -> (FilePath, Script PlutusScriptV1, ScriptData)
  -> Validity
  -> ToUTxO AlonzoEra
mkUtxoScript networkId (scriptFile, script, txOutDatum) validity values
  = ( map mkTxOut values
    , newFunds
    )
 where
  mkTxOut v = TxOut
                 plutusScriptAddr
                 (lovelaceToTxOutValue v)
                 (TxOutDatumHash ScriptDataInAlonzoEra $ hashScriptData txOutDatum)
                 ReferenceScriptNone   
                    
  plutusScriptAddr = makeShelleyAddressInEra
                       networkId
                       (PaymentCredentialByScript $ hashScript script)
                       NoStakeAddress

  newFunds txId = zipWith (mkNewFund txId) [TxIx 0 ..] values

  mkNewFund :: TxId -> TxIx -> Lovelace -> Fund
  mkNewFund txId txIx val = Fund $ InAnyCardanoEra AlonzoEra $ FundInEra {
      _fundTxIn = TxIn txId txIx
    , _fundVal = lovelaceToTxOutValue val
    , _fundSigningKey = Nothing
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
toScriptHash str =
  case deserialiseFromRawBytesHex (AsHash AsScriptData) (BSC.pack str) of
    Right x -> x
    Left e -> error $ "Invalid datum hash: " ++ displayError e

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
  evaluationContext <- case Plutus.mkEvaluationContext costModel of
    Right x  -> Right x
    Left err -> Left $ "evaluationContext unavailable: " <> show err
  let
    apiVersion = protocolParamProtocolVersion protocolParameters
    protocolVersion = Plutus.ProtocolVersion (fromIntegral $ fst apiVersion) (fromIntegral $ snd apiVersion)
    (_logout, res) = Plutus.evaluateScriptCounting protocolVersion Plutus.Verbose evaluationContext script
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
