{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module  MonadicGen.Cardano.TxGenerator.Tx
        (module MonadicGen.Cardano.TxGenerator.Tx)
        where

import           Data.Bifunctor (bimap, second)
import qualified Data.ByteString as BS (length)
import           Data.Function ((&))
import           Data.Maybe (mapMaybe)

import           Cardano.Api
import           Cardano.Api.Shelley (ProtocolParameters)

import           MonadicGen.Cardano.TxGenerator.Fund
import           MonadicGen.Cardano.TxGenerator.Types
import           MonadicGen.Cardano.TxGenerator.UTxO (ToUTxOList)


-- | 'CreateAndStore' is meant to represent building a transaction
-- from a single number and presenting a function to carry out the
-- needed side effects.
-- This type alias is only used in "MonadicGen.Cardano.Benchmarking.Wallet".
type CreateAndStore m era           = Lovelace -> (TxOut CtxTx era, TxIx -> TxId -> m ())

-- | 'CreateAndStoreList' is meant to represent building a transaction
-- and presenting a function to carry out the needed side effects.
-- This type alias is also only used in "MonadicGen.Cardano.Benchmarking.Wallet".
-- The @split@ parameter seems to actualy be used for not much more
-- than lists and records containing lists.
type CreateAndStoreList m era split = split -> ([TxOut CtxTx era], TxId -> m ())


-- TODO: 'sourceToStoreTransaction' et al need to be broken up
-- for the sake of maintainability and use the Error monad.

-- | 'sourceToStoreTransaction' builds a transaction out of several
-- arguments. "MonadicGen.Cardano.Benchmarking.Script.PureExample" is the sole caller.
-- @txGenerator@ is just 'genTx' partially applied in all uses of all
-- these functions.
-- @inputFunds@ for this is a list of 'Lovelace' with some extra
-- fields to throw away and coproducts maintaining distinctions that
-- don't matter to these functions.
-- The @inToOut@ argument seems to just sum and subtract the fee in
-- seemingly all callers.
-- @mkTxOut@ gets built from functions in "MonadicGen.Cardano.TxGenerator.UTxO".
-- The other functions take 'CreateAndStoreList' arguments and name
-- them @valueSplitter@ and callers construct the argument from the
-- mangling functions in 'MonadicGen.Cardano.TxGenerator.Utils".
-- @fundToStore@ commits the single-threaded fund state in its sole
-- caller with 'Control.Monad.State.put', using a @State@ monad.
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
      -- 'getFundLovelace' unwraps the 'TxOutValue' in a fund field
      -- so it's all just 'Lovelace' instead of a coproduct
      -- maintaining distinctions.
      outValues = inToOut $ map getFundLovelace inputFunds
      (outputs, toFunds) = mkTxOut outValues
    case txGenerator inputFunds outputs of
        Left err -> return $ Left err
        Right (tx, txId) -> do
          fundToStore $ toFunds txId
          return $ Right tx

-- | 'sourceToStoreTransactionNew' builds a new transaction out of
-- several things. 'MonadicGen.Cardano.Benchmarking.Script.Core.evalGenerator'
-- in "MonadicGen.Cardano.Benchmarking.Script.Core" is the sole caller.
-- @txGenerator@ is just 'genTx' partially applied in every use.
-- @inputFunds@ for this is a list of 'Lovelace' with some extra
-- fields to throw away and coproducts maintaining distinctions that
-- don't matter to these functions.
-- @valueSplitter@ is just 'MonadicGen.Cardano.TxGenerator.Utils.includeChange' or
-- 'MonadicGen.Cardano.TxGenerator.Utils.inputsToOutputsWithFee' at every use,
-- which just sum the inputs and subtract the fee.
-- @toStore@ is just a partial application of either
-- 'MonadicGen.Cardano.Benchmarking.Wallet.mangleWithChange'
-- or 'MonadicGen.Cardano.Benchmarking.Wallet.mangle' at every use.
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

-- | 'sourceTransactionPreview' is only used at one point in
-- 'MonadicGen.Cardano.Benchmarking.Script.Core.evalGenerator' within
-- "MonadicGen.Cardano.Benchmarking.Script.Core" to generate a hopefully pure
-- transaction to examine.
-- This only constructs a preview of a transaction not intended
-- to be submitted. Funds remain unchanged by dint of a different
-- method of wallet access.
-- @txGenerator@ is the same 'genTx' partial application passed
-- to other functions here.
-- @inputFunds@ for this is a list of 'Lovelace' with some extra
-- fields to throw away and coproducts maintaining distinctions that
-- don't matter to these functions. This is the only argument that
-- differs -- from 'sourceToStoreTransactionNew', being drawn from
-- a use of 'MonadicGen.Cardano.Benchmarking.Wallet.walletPreview'.
-- @valueSplitter@ is just
-- 'MonadicGen.Cardano.TxGenerator.Utils.inputsToOutputsWithFee'
-- at the sole use, with the same variable for monad lifting
-- etc. as the other companion functions.
-- @toStore@ is just a partial application of
-- 'MonadicGen.Cardano.Benchmarking.Wallet.mangle' at the sole use, with the
-- same expression involving the same function returned as a
-- product of 'MonadicGen.Cardano.Benchmarking.Wallet.createAndStore' as the
-- nearby invocation of 'sourceToStoreTransactionNew' in
-- "MonadicGen.Cardano.Benchmarking.Script.Core".
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

-- | 'genTx' seems to mostly be a wrapper for
-- 'Cardano.Api.TxBody.createAndValidateTransactionBody', which uses
-- the 'Either' convention in lieu of e.g.
-- 'Control.Monad.Trans.Except.ExceptT'. Then the pure function
-- 'Cardano.Api.Tx.makeSignedTransaction' is composed with it and
-- the 'Cardano.Api.Error' is lifted to 'MonadicGen.Cardano.TxGenerator.Types.TxGenError'
-- as an 'MonadicGen.Cardano.TxGenerator.Types.ApiError' case.
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
