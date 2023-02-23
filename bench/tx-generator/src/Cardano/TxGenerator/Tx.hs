{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module  Cardano.TxGenerator.Tx
        (module Cardano.TxGenerator.Tx)
        where

import           Data.Bifunctor (bimap, second)
import qualified Data.ByteString as BS (length)
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
  -> m (Either TxGenError (Tx era))
sourceToStoreTransaction txGenerator fundSource inToOut mkTxOut fundToStore =
  fundSource >>= either (return . Left) go
 where
  go inputFunds = do
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
  -> m (Either TxGenError (Tx era))
sourceToStoreTransactionNew txGenerator fundSource valueSplitter toStore =
  fundSource >>= either (return . Left) go
 where
  go inputFunds = do
    let
      split = valueSplitter $ map getFundLovelace inputFunds
      (outputs, storeAction) = toStore split
    case txGenerator inputFunds outputs of
        Left err -> return $ Left err
        Right (tx, txId) -> do
          storeAction txId
          return $ Right tx

-- just a preview of a transaction:
-- not intended to be submitted; funds remain unchanged
sourceTransactionPreview ::
     TxGenerator era
  -> [Fund]
  -> ([Lovelace] -> split)
  -> CreateAndStoreList m era split
  -> Either TxGenError (Tx era)
sourceTransactionPreview txGenerator inputFunds valueSplitter toStore =
  second fst $
    txGenerator inputFunds outputs
 where
  split         = valueSplitter $ map getFundLovelace inputFunds
  (outputs, _)  = toStore split

genTx :: forall era. IsShelleyBasedEra era =>
     ProtocolParameters
  -> (TxInsCollateral era, [Fund])
  -> TxFee era
  -> TxMetadataInEra era
  -> TxGenerator era
genTx protocolParameters (collateral, collFunds) fee metadata inFunds outputs
  = bimap
      ApiError
      (\b -> (signShelleyTransaction b $ map WitnessPaymentKey allKeys, getTxId b))
      (createAndValidateTransactionBody txBodyContent)
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


txSizeInBytes :: forall era. IsShelleyBasedEra era =>
     Tx era
  -> Int
txSizeInBytes
  = BS.length . serialiseToCBOR
