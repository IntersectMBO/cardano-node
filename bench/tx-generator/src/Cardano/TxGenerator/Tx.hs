{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module  Cardano.TxGenerator.Tx
        (module Cardano.TxGenerator.Tx)
        where

import           Cardano.Api hiding (txId)
import qualified Cardano.Api.Experimental as Exp
import qualified Cardano.Api.Experimental.Tx as Exp

import qualified Cardano.Ledger.Coin as L
import qualified Cardano.Ledger.Core as L (PParams, addrTxWitsL, txIdTx, witsTxL)
import           Cardano.TxGenerator.Fund
import           Cardano.TxGenerator.Types
import           Cardano.TxGenerator.UTxO (ToUTxOList)

import           Data.Bifunctor (first, second)
import qualified Data.ByteString as BS (length)
import           Data.Function ((&))
import           Data.Maybe (mapMaybe)
import           Lens.Micro ((.~))


-- | 'CreateAndStore' is meant to represent building a transaction
-- from a single number and presenting a function to carry out the
-- needed side effects.
-- This type alias is only used in "Cardano.Benchmarking.Wallet".
type CreateAndStore m era           = L.Coin -> (Exp.TxOut (ShelleyLedgerEra era), TxIx -> TxId -> m ())

-- | 'CreateAndStoreList' is meant to represent building a transaction
-- and presenting a function to carry out the needed side effects.
-- This type alias is also only used in "Cardano.Benchmarking.Wallet".
-- The @split@ parameter seems to actually be used for not much more
-- than lists and records containing lists.
type CreateAndStoreList m era split = split -> ([Exp.TxOut (ShelleyLedgerEra era)], TxId -> m ())


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
  -> m (Either TxGenError (Tx era))
sourceToStoreTransaction txGenerator fundSource inToOut mkTxOut fundToStore =
  fundSource >>= either (return . Left) go
 where
  go inputFunds = do
    let
      -- 'getFundCoin' is the ada a fund holds.
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
  -> m (Either TxGenError (Tx era))
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
  -> Either TxGenError (Tx era)
sourceTransactionPreview txGenerator inputFunds valueSplitter toStore =
  second fst $
    txGenerator inputFunds outputs
 where
  split         = valueSplitter $ map getFundCoin inputFunds
  (outputs, _)  = toStore split

-- | 'genTx' builds and signs a transaction with the experimental API
-- ('Exp.makeUnsignedTx', 'Exp.makeKeyWitness' and 'Exp.signTx'), lifting
-- a 'Exp.MakeUnsignedTxError' to 'Cardano.TxGenerator.Types.TxGenError' as
-- an 'Cardano.TxGenerator.Types.ApiError' case. The signed ledger transaction
-- is returned as an old-API 'Tx', which is what the submission code consumes.
-- The @txGenerator@ arguments of the rest of the functions in this
-- module are all partial applications of this to its first 4 arguments.
-- The remaining 2 arguments come from 'TxGenerator' being a being a type alias
-- for a function type -- of two arguments.
genTx :: forall era. ()
  => Exp.EraCommonConstraints era
  => L.PParams (ShelleyLedgerEra era)
  -> ([TxIn], [Fund])
  -- ^ Collateral inputs, and the funds they spend (for their signing keys)
  -> L.Coin
  -> TxMetadata
  -> TxGenerator era
genTx ledgerParameters (collateral, collFunds) fee metadata inFunds outputs = do
  unsignedTx <- first ApiError $ Exp.makeUnsignedTx era txBodyContent
  let keyWitnesses = map (Exp.makeKeyWitness era unsignedTx . WitnessPaymentKey) allKeys
  case Exp.signTx era [] keyWitnesses unsignedTx of
    Exp.SignedTx ledgerTx ->
      return (ShelleyTx (shelleyBasedEra @era) ledgerTx, fromShelleyTxId $ L.txIdTx ledgerTx)
 where
  era = Exp.useEra @era
  allKeys = mapMaybe getFundKey $ inFunds ++ collFunds
  txBodyContent = Exp.defaultTxBodyContent
    & Exp.setTxIns (map (\f -> (getFundTxIn f, getFundWitness @era f)) inFunds)
    & Exp.setTxInsCollateral collateral
    & Exp.setTxOuts outputs
    & Exp.setTxFee fee
    & Exp.setTxMetadata metadata
    & Exp.setTxProtocolParams ledgerParameters


-- | 'estimateTxFee' estimates the minimum fee of a transaction, counting
-- @keyWitnesses@ key witnesses instead of the ones it already carries, so that
-- a signed transaction preview is estimated like its unsigned counterpart.
estimateTxFee :: Exp.EraCommonConstraints era
  => L.PParams (ShelleyLedgerEra era)
  -> Tx era
  -> Word
  -> L.Coin
estimateTxFee ledgerParameters (ShelleyTx _ ledgerTx) keyWitnesses =
  Exp.evaluateTransactionFee ledgerParameters unsignedTx keyWitnesses 0 0
 where
  unsignedTx = Exp.UnsignedTx $ ledgerTx & L.witsTxL . L.addrTxWitsL .~ mempty

txSizeInBytes :: forall era. IsShelleyBasedEra era =>
     Tx era
  -> Int
txSizeInBytes
  = BS.length . serialiseToCBOR
