{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module  Cardano.TxGenerator.Tx
        (module Cardano.TxGenerator.Tx)
        where

import           Data.Maybe (mapMaybe)

import           Cardano.Api
import           Cardano.Api.Shelley (ProtocolParameters)

import           Cardano.TxGenerator.Fund
import           Cardano.TxGenerator.Types
import           Cardano.TxGenerator.UTxO (ToUTxOList)


type CreateAndStore m era           = Lovelace -> (TxOut CtxTx era, TxIx -> TxId -> m ())
type CreateAndStoreList m era split = split -> ([TxOut CtxTx era], TxId -> m ())

--TODO: use Error monad
--TODO: need to break this up
sourceToStoreTransaction ::
     Monad m
  => TxGenerator era
  -> FundSource m
  -> ([Lovelace] -> split)
  -> ToUTxOList era split
  -> FundToStoreList m                --inline to ToUTxOList
  -> m (Either String (Tx era))
sourceToStoreTransaction txGenerator fundSource inToOut mkTxOut fundToStore = do
  fundSource >>= \case
    Left err -> return $ Left err
    Right inputFunds -> work inputFunds
 where
  work inputFunds = do
    let
      outValues = inToOut $ map getFundLovelace inputFunds
      (outputs, toFunds) = mkTxOut outValues
    case txGenerator inputFunds outputs of
        Left err -> return $ Left err
        Right (tx, txId) -> do
          fundToStore $ toFunds txId
          return $ Right tx

sourceToStoreTransactionNew ::
     Monad m
  => TxGenerator era
  -> FundSource m
  -> ([Lovelace] -> split)
  -> CreateAndStoreList m era split
  -> m (Either String (Tx era))
sourceToStoreTransactionNew txGenerator fundSource valueSplitter toStore = do
  fundSource >>= \case
    Left err -> return $ Left err
    Right inputFunds -> work inputFunds
 where
  work inputFunds = do
    let
      split = valueSplitter $ map getFundLovelace inputFunds
      (outputs, storeAction) = toStore split
    case txGenerator inputFunds outputs of
        Left err -> return $ Left err
        Right (tx, txId) -> do
          storeAction txId
          return $ Right tx

genTx :: forall era. IsShelleyBasedEra era =>
     ProtocolParameters
  -> (TxInsCollateral era, [Fund])
  -> TxFee era
  -> TxMetadataInEra era
  -> TxGenerator era
genTx protocolParameters (collateral, collFunds) fee metadata inFunds outputs
  = case makeTransactionBody txBodyContent of
      Left err -> Left $ show err
      Right b -> Right ( signShelleyTransaction b $ map WitnessPaymentKey allKeys
                       , getTxId b
                       )
 where
  allKeys = mapMaybe getFundKey $ inFunds ++ collFunds
  txBodyContent = TxBodyContent {
      txIns = map (\f -> (getFundTxIn f, BuildTxWith $ getFundWitness f)) inFunds
    , txInsCollateral = collateral
    , txInsReference = TxInsReferenceNone
    , txOuts = outputs
    , txFee = fee
    , txValidityRange = (TxValidityNoLowerBound, upperBound)
    , txMetadata = metadata
    , txAuxScripts = TxAuxScriptsNone
    , txExtraKeyWits = TxExtraKeyWitnessesNone
    , txProtocolParams = BuildTxWith $ Just protocolParameters
    , txWithdrawals = TxWithdrawalsNone
    , txCertificates = TxCertificatesNone
    , txUpdateProposal = TxUpdateProposalNone
    , txMintValue = TxMintNone
    , txScriptValidity = TxScriptValidityNone
    , txReturnCollateral = TxReturnCollateralNone
    , txTotalCollateral = TxTotalCollateralNone
    }

  upperBound :: TxValidityUpperBound era
  upperBound = case shelleyBasedEra @era of
    ShelleyBasedEraShelley -> TxValidityUpperBound ValidityUpperBoundInShelleyEra $ SlotNo maxBound
    ShelleyBasedEraAllegra -> TxValidityNoUpperBound ValidityNoUpperBoundInAllegraEra
    ShelleyBasedEraMary    -> TxValidityNoUpperBound ValidityNoUpperBoundInMaryEra
    ShelleyBasedEraAlonzo  -> TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra
    ShelleyBasedEraBabbage -> TxValidityNoUpperBound ValidityNoUpperBoundInBabbageEra
