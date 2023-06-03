{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module  Cardano.TxGenerator.Tx
        (module Cardano.TxGenerator.Tx)
        where

import           Control.Monad.Trans.Except (ExceptT, runExceptT, except)
import           Control.Monad.Trans (lift)
import           Data.Bifunctor (bimap)
import qualified Data.ByteString as BS (length)
import           Data.Function ((&))
import           Data.Maybe (mapMaybe)

import           Cardano.Api
import           Cardano.Api.Shelley (ProtocolParameters)

import           Cardano.TxGenerator.Fund
import           Cardano.TxGenerator.Types
import           Cardano.TxGenerator.UTxO (ToUTxOList)


type CreateAndStore m era           = Lovelace -> (TxOut CtxTx era, TxIx -> TxId -> m ())
type CreateAndStoreList m era split = split -> ([TxOut CtxTx era], TxId -> m ())

-- TODO: 'sourceToStoreTransaction' et al need to be broken up.
-- These three functions are difficult to interpret, and should
-- probably be changed to return their results in an 'ExceptT'
-- monad instead of manually manipulating 'Either'.
sourceToStoreTransaction ::
     Monad m
  => TxGenerator era
  -> FundSource m
  -> ([Lovelace] -> ExceptT TxGenError m split)
  -> ToUTxOList era split
  -> FundToStoreList m                --inline to ToUTxOList
  -> m (Either TxGenError (Tx era))
sourceToStoreTransaction txGenerator fundSource inToOut mkTxOut fundToStore
  = runExceptT
  $ do
    inputFunds <- except =<< lift fundSource
    (outputs, toFunds) <- fmap mkTxOut . inToOut $ map getFundLovelace inputFunds
    (tx, txId) <- except $ txGenerator inputFunds outputs
    lift . fundToStore $ toFunds txId
    return tx

sourceToStoreTransactionNew ::
     Monad m
  => TxGenerator era
  -> FundSource m
  -> ([Lovelace] -> ExceptT TxGenError m split)
  -> CreateAndStoreList m era split
  -> m (Either TxGenError (Tx era))
sourceToStoreTransactionNew txGenerator fundSource valueSplitter toStore
  = runExceptT
  $ do
      inputFunds <- except =<< lift fundSource
      (outputs, storeAction) <- fmap toStore . valueSplitter $ map getFundLovelace inputFunds
      (tx, txId) <- except $ txGenerator inputFunds outputs
      lift $ storeAction txId
      return tx

-- just a preview of a transaction:
-- not intended to be submitted; funds remain unchanged
sourceTransactionPreview ::
  Monad m
  => TxGenerator era
  -> [Fund]
  -> ([Lovelace] -> ExceptT TxGenError m split)
  -> CreateAndStoreList m era split
  -> m (Either TxGenError (Tx era))
sourceTransactionPreview txGenerator inputFunds valueSplitter toStore
  = runExceptT
  $ do
    (outputs, _) <- fmap toStore . valueSplitter $ map getFundLovelace inputFunds
    fmap fst . except $ txGenerator inputFunds outputs

genTx :: forall era. ()
  => IsShelleyBasedEra era
  => CardanoEra era
  -> ProtocolParameters
  -> (TxInsCollateral era, [Fund])
  -> TxFee era
  -> TxMetadataInEra era
  -> TxGenerator era
genTx _era protocolParameters (collateral, collFunds) fee metadata inFunds outputs
  = bimap
      ApiError
      (\b -> (signShelleyTransaction b $ map WitnessPaymentKey allKeys, getTxId b))
      (createAndValidateTransactionBody txBodyContent)
 where
  allKeys = mapMaybe getFundKey $ inFunds ++ collFunds
  txBodyContent = defaultTxBodyContent
    & setTxIns (map (\f -> (getFundTxIn f, BuildTxWith $ getFundWitness f)) inFunds)
    & setTxInsCollateral collateral
    & setTxOuts outputs
    & setTxFee fee
    & setTxValidityRange (TxValidityNoLowerBound, upperBound)
    & setTxMetadata metadata
    & setTxProtocolParams (BuildTxWith (Just protocolParameters))

  upperBound :: TxValidityUpperBound era
  upperBound = case shelleyBasedEra @era of
    ShelleyBasedEraShelley -> TxValidityUpperBound ValidityUpperBoundInShelleyEra $ SlotNo maxBound
    ShelleyBasedEraAllegra -> TxValidityNoUpperBound ValidityNoUpperBoundInAllegraEra
    ShelleyBasedEraMary    -> TxValidityNoUpperBound ValidityNoUpperBoundInMaryEra
    ShelleyBasedEraAlonzo  -> TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra
    ShelleyBasedEraBabbage -> TxValidityNoUpperBound ValidityNoUpperBoundInBabbageEra
    ShelleyBasedEraConway  -> TxValidityNoUpperBound ValidityNoUpperBoundInConwayEra


txSizeInBytes :: forall era. IsShelleyBasedEra era =>
     Tx era
  -> Int
txSizeInBytes
  = BS.length . serialiseToCBOR
