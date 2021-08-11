{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.Benchmarking.PlutusExample
where
import Prelude

import qualified Data.ByteString.Char8 as BSC
import Control.Monad.Trans.Except

import Cardano.CLI.Shelley.Script (readFileScriptInAnyLang)

import Cardano.Api
import Cardano.Api.Shelley (ProtocolParameters)

import Cardano.Benchmarking.FundSet
import Cardano.Benchmarking.GeneratorTx.Tx as Tx (mkTxOutValueAdaOnly)
import Cardano.Benchmarking.Wallet

mkUtxoScript ::
     NetworkId
  -> SigningKey PaymentKey
  -> (Script PlutusScriptV1, Hash ScriptData)
  -> Validity
  -> ToUTxO AlonzoEra
mkUtxoScript networkId key (script, txOutDatumHash) validity values
  = ( map mkTxOut values
    , newFunds
    )
 where
  mkTxOut v = TxOut plutusScriptAddr (mkTxOutValueAdaOnly v) (TxOutDatumHash ScriptDataInAlonzoEra txOutDatumHash)

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
    , _fundVariant = PlutusScriptFund
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
    , txExtraScriptData = BuildTxWith TxExtraScriptDataNone
    , txExtraKeyWits = TxExtraKeyWitnessesNone
    , txProtocolParams = BuildTxWith $ Just protocolParameters
    , txWithdrawals = TxWithdrawalsNone
    , txCertificates = TxCertificatesNone
    , txUpdateProposal = TxUpdateProposalNone
    , txMintValue = TxMintNone
    , txScriptValidity = TxScriptValidityNone
    }
