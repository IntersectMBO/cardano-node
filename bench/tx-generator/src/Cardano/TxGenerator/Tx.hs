{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module  Cardano.TxGenerator.Tx
        (module Cardano.TxGenerator.Tx)
        where

import           Cardano.Api hiding (txId)
import           Cardano.Api.Experimental (AnyWitness (..), Era, IsEra, LedgerEra, SignedTx (..),
                   makeKeyWitness, makeUnsignedTx, obtainCommonConstraints, signTx, useEra)
import qualified Cardano.Api.Experimental.Tx as Exp

import qualified Cardano.Ledger.Coin as L
import           Cardano.TxGenerator.Fund
import           Cardano.TxGenerator.Types
import           Cardano.TxGenerator.Utils (txIdFromSignedTx)
import           Cardano.TxGenerator.UTxO (ToUTxOList)

import           Data.Bifunctor (first, second)
import qualified Data.ByteString as BS (length)
import           Data.Function ((&))
import           Data.Maybe (mapMaybe)


-- | 'CreateAndStore' is meant to represent building a transaction
-- from a single number and presenting a function to carry out the
-- needed side effects.
-- This type alias is only used in "Cardano.Benchmarking.Wallet".
type CreateAndStore m era           = L.Coin -> (Exp.TxOut (LedgerEra era), TxIx -> TxId -> m ())

-- | 'CreateAndStoreList' is meant to represent building a transaction
-- and presenting a function to carry out the needed side effects.
-- This type alias is also only used in "Cardano.Benchmarking.Wallet".
-- The @split@ parameter seems to actually be used for not much more
-- than lists and records containing lists.
type CreateAndStoreList m era split = split -> ([Exp.TxOut (LedgerEra era)], TxId -> m ())


-- TODO: 'sourceToStoreTransaction' et al need to be broken up
-- for the sake of maintainability and use the Error monad.

-- | 'sourceToStoreTransaction' builds a transaction out of several
-- arguments. "Cardano.Benchmarking.Script.PureExample" is the sole caller.
-- @txGenerator@ is just 'genTx' partially applied in all uses of all
-- these functions.
-- @inputFunds@ for this is a list of 'L.Coin' with some extra
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
  -> FundSource m
  -> ([L.Coin] -> split)
  -> ToUTxOList era split
  -> FundToStoreList m                --inline to ToUTxOList
  -> m (Either TxGenError (SignedTx era))
sourceToStoreTransaction txGenerator fundSource inToOut mkTxOut fundToStore =
  fundSource >>= either (return . Left) go
 where
  go inputFunds = do
    let
      -- 'getFundCoin' unwraps the 'TxOutValue' in a fund field
      -- so it's all just 'Lovelace' instead of a coproduct
      -- maintaining distinctions.
      outValues = inToOut $ map getFundCoin inputFunds
      (outputs, toFunds) = mkTxOut outValues
    case txGenerator inputFunds outputs of
        Left err -> return $ Left err
        Right (tx, txId) -> do
          fundToStore $ toFunds txId
          return $ Right tx

-- | 'sourceToStoreTransactionNew' builds a new transaction out of
-- several things. 'Cardano.Benchmarking.Script.Core.evalGenerator'
-- in "Cardano.Benchmarking.Script.Core" is the sole caller.
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
  -> FundSource m
  -> ([L.Coin] -> split)
  -> CreateAndStoreList m era split
  -> m (Either TxGenError (SignedTx era))
sourceToStoreTransactionNew txGenerator fundSource valueSplitter toStore =
  fundSource >>= either (return . Left) go
 where
  go inputFunds = do
    let
      split = valueSplitter $ map getFundCoin inputFunds
      (outputs, storeAction) = toStore split
    case txGenerator inputFunds outputs of
        Left err -> return $ Left err
        Right (tx, txId) -> do
          storeAction txId
          return $ Right tx

-- | 'sourceTransactionPreview' is only used at one point in
-- 'Cardano.Benchmarking.Script.Core.evalGenerator' within
-- "Cardano.Benchmarking.Script.Core" to generate a hopefully pure
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
-- a use of 'Cardano.Benchmarking.Wallet.walletPreview'.
-- @valueSplitter@ is just
-- 'Cardano.TxGenerator.Utils.inputsToOutputsWithFee'
-- at the sole use, with the same variable for monad lifting
-- etc. as the other companion functions.
-- @toStore@ is just a partial application of
-- 'Cardano.Benchmarking.Wallet.mangle' at the sole use, with the
-- same expression involving the same function returned as a
-- product of 'Cardano.Benchmarking.Wallet.createAndStore' as the
-- nearby invocation of 'sourceToStoreTransactionNew' in
-- "Cardano.Benchmarking.Script.Core".
sourceTransactionPreview ::
     TxGenerator era
  -> [Fund]
  -> ([L.Coin] -> split)
  -> CreateAndStoreList m era split
  -> Either TxGenError (SignedTx era)
sourceTransactionPreview txGenerator inputFunds valueSplitter toStore =
  second fst $
    txGenerator inputFunds outputs
 where
  split         = valueSplitter $ map getFundCoin inputFunds
  (outputs, _)  = toStore split

-- | 'genTx' builds a signed transaction using the experimental API.
-- The @txGenerator@ arguments of the rest of the functions in this
-- module are all partial applications of this to its first 5 arguments.
-- The 7th argument comes from 'TxGenerator' being a type alias
-- for a function type -- of two arguments.
genTx ::
     Era era
  -> LedgerProtocolParameters era
  -> ([TxIn], [Fund])
  -> L.Coin
  -> TxMetadataInEra era
  -> TxGenerator era
genTx era (LedgerProtocolParameters pparams) (collateralIns, collFunds) fee metadata inFunds outputs =
  obtainCommonConstraints era $ do
    let allKeys = mapMaybe getFundKey $ inFunds ++ collFunds
        expInputs = map (\f -> (getFundTxIn f, AnyKeyWitnessPlaceholder)) inFunds
        expMetadata = case metadata of
          TxMetadataNone -> mempty
          TxMetadataInEra _ m -> m
        txBodyContent =
          Exp.defaultTxBodyContent
            & Exp.setTxIns expInputs
            & Exp.setTxInsCollateral collateralIns
            & Exp.setTxOuts outputs
            & Exp.setTxFee fee
            & Exp.setTxMetadata expMetadata
            & Exp.setTxProtocolParams pparams
    unsignedTx <- first (\err -> TxGenError $ "genTx: " ++ show err) $ makeUnsignedTx era txBodyContent
    let witVKeys = [makeKeyWitness era unsignedTx (WitnessPaymentKey key) | key <- allKeys]
    let tx = signTx era [] witVKeys unsignedTx
    Right (tx, txIdFromSignedTx tx)


txSizeInBytes :: forall era. IsEra era =>
     SignedTx era
  -> Int
txSizeInBytes tx
  = obtainCommonConstraints (useEra @era) $ BS.length $ serialiseToRawBytes tx
