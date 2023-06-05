{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module  Cardano.TxGenerator.Tx
        (module Cardano.TxGenerator.Tx)
        where

import           Control.Monad.Trans.Except (ExceptT, except)
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


-- | 'CreateAndStore' is meant to represent building a transaction
-- from a single number and presenting a function to carry out the
-- needed side effects.
-- This type alias is only used in "Cardano.Benchmarking.Wallet".
type CreateAndStore m era           = Lovelace -> (TxOut CtxTx era, TxIx -> TxId -> m ())

-- | 'CreateAndStoreList' is meant to represent building a transaction
-- and presenting a function to carry out the needed side effects.
-- This type alias is also only used in "Cardano.Benchmarking.Wallet".
-- The @split@ parameter seems to actualy be used for not much more
-- than lists and records containing lists.
type CreateAndStoreList m era split = split -> ([TxOut CtxTx era], TxId -> m ())


-- TODO: 'sourceToStoreTransaction' et al need to be broken up
-- for the sake of maintainability.

-- | 'sourceToStoreTransaction' builds a transaction out of several
-- arguments. "Cardano.Benchmarking.Script.PureExample" is the sole caller.
-- @txGenerator@ is just 'genTx' partially applied in all uses of all
-- these functions.
-- @inputFunds@ for this is a list of 'Lovelace' with some extra
-- fields to throw away and coproducts maintaining distinctions that
-- don't matter to these functions.
-- The @inToOut@ argument seems to just sum and subtract the fee in
-- seemingly all callers.
-- @mkTxOut@ gets built from functions in "Cardano.TxGenerator.UTxO".
-- The other functions take 'CreateAndStoreList' arguments and name
-- them @valueSplitter@ and callers construct the argument from the
-- mangling functions in 'Cardano.TxGenerator.Utils".
-- @fundToStore@ commits the single-threaded fund state in its sole
-- caller with 'Control.Monad.State.put', using a @State@ monad.
sourceToStoreTransaction ::
     Monad m
  => TxGenerator era
  -> [Fund]
  -> ([Lovelace] -> ExceptT TxGenError m split)
  -> ToUTxOList era split
  -> FundToStoreList m                --inline to ToUTxOList
  -> ExceptT TxGenError m (Tx era)
sourceToStoreTransaction txGenerator inputFunds inToOut mkTxOut fundToStore = do
  -- 'getFundLovelace' unwraps the 'TxOutValue' in a fund field so it's
  -- all just 'Lovelace' instead of a copruduct maintaining distinctions.
  (outputs, toFunds) <- fmap mkTxOut . inToOut $ map getFundLovelace inputFunds
  (tx, txId) <- except $ txGenerator inputFunds outputs
  lift . fundToStore $ toFunds txId
  return tx

-- | 'sourceToStoreTransactionNew' builds a new transaction out of
-- several things. 'Cardano.Benchmarking.Script.Core.evalGenerator' in
-- "Cardano.Benchmarking.Script.Core" is the sole caller.
-- @txGenerator@ is just 'genTx' partially applied in every use.
-- @inputFunds@ for this is a list of 'Lovelace' with some extra
-- fields to throw away and coproducts maintaining distinctions that
-- don't matter to these functions.
-- @valueSplitter@ is just 'Cardano.TxGenerator.Utils.includeChange' or
-- 'Cardano.TxGenerator.Utils.inputsToOutputsWithFee' at every use,
-- which just sum the inputs and subtract the fee.
-- @toStore@ is just a partial application of either
-- 'Cardano.Benchmarking.Wallet.mangleWithChange'
-- or 'Cardano.Benchmarking.Wallet.mangle' at every use.
sourceToStoreTransactionNew ::
     Monad m
  => TxGenerator era
  -> [Fund]
  -> ([Lovelace] -> ExceptT TxGenError m split)
  -> CreateAndStoreList m era split
  -> ExceptT TxGenError m (Tx era)
sourceToStoreTransactionNew txGenerator inputFunds valueSplitter toStore = do
  (outputs, storeAction) <- fmap toStore . valueSplitter $ map getFundLovelace inputFunds
  (tx, txId) <- except $ txGenerator inputFunds outputs
  lift $ storeAction txId
  return tx

-- | 'sourceTransactionPreview' is only used at one point in
-- 'Cardano.Benchmarking.Script.Core.evalGenerator' within
-- "Cardano.Benchmarking.Script.Core" to generate a hopefully pure
-- transaction to examine.
-- This only constructs a preview of a transaction not intended to be
-- submitted. Funds remain unchanged by dint of a different method
-- of wallet access.
-- @txGenerator@ is the same 'genTx' partial application passed to other
-- functions here.
-- @inputFunds@ for this is a list of 'Lovelace' with some extra
-- fields to throw away and coproducts maintaining distinctions that
-- don't matter to these functions. This is the only argument that differs
-- from 'sourceToStoreTransactionNew', being drawn from a use of
-- 'Cardano.Benchmarking.Wallet.walletPreview'.
-- @valueSplitter@ is just 'Cardano.TxGenerator.Utils.inputsToOutputsWithFee'
-- at the sole use, with the same variable for monad lifting etc. as
-- the other companion functions.
-- @toStore@ is just a partial application of
-- 'Cardano.Benchmarking.Wallet.mangle' at the sole use, with the same
-- expression involving the same function returned as a product of
-- 'Cardano.Benchmarking.Wallet.createAndStore' as the nearby invocation
-- of 'sourceToStoreTransactionNew' in "Cardano.Benchmarking.Script.Core".
sourceTransactionPreview ::
  Monad m
  => TxGenerator era
  -> [Fund]
  -> ([Lovelace] -> ExceptT TxGenError m split)
  -> CreateAndStoreList m era split
  -> ExceptT TxGenError m (Tx era)
sourceTransactionPreview txGenerator inputFunds valueSplitter toStore = do
  (outputs, _) <- fmap toStore . valueSplitter $ map getFundLovelace inputFunds
  fmap fst . except $ txGenerator inputFunds outputs

-- | 'genTx' seems to mostly be a wrapper for
-- 'Cardano.Api.TxBody.createAndValidateTransactionBody', which uses
-- the 'Either' convention in lieu of e.g.
-- 'Control.Monad.Trans.Except.ExceptT'. Then the pure function
-- 'Cardano.Api.Tx.makeSignedTransaction' is composed with it and
-- the 'Cardano.Api.Error' is lifted to 'Cardano.TxGenerator.Types.TxGenError'
-- as an 'Cardano.TxGenerator.Types.ApiError' case.
-- The @txGenerator@ arguments of the rest of the functions in this
-- module are all partial applications of this to its first 5 arguments.
-- The 7th argument comes from 'TxGenerator' being a being a type alias
-- for a function type -- of two arguments.
genTx :: forall era. ()
  => IsShelleyBasedEra era
  => CardanoEra era
  -> ProtocolParameters
  -> (TxInsCollateral era, [Fund])
  -> TxFee era
  -> TxMetadataInEra era
  -> TxGenerator era
genTx _era protocolParameters (collateral, collFunds) fee metadata inFunds outputs
  -- This use of 'Data.Bifunctor.bimap` lifts the error type to 'Env.Error'
  -- at the same time as it adds a signature to the transaction body and
  -- fetches the transaction ID from it too.
  = ApiError `bimap` (\b -> (signShelleyTransaction b allKeys, getTxId b))
        $ createAndValidateTransactionBody txBodyContent
 where
  allKeys = mapMaybe (fmap WitnessPaymentKey . getFundKey) $ inFunds ++ collFunds
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
