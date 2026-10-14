{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Cardano.Benchmarking.Wallet
Description : Basic functions to manipulate wallets.

Besides the basic wallet functions, this module has the 'Destination's that
transactions pay their outputs to. A transaction's outputs are planned with
'payTo' before it is built, and once its id is known, 'storeOutputs' puts the
funds they become into their destinations' wallets.
-}
module Cardano.Benchmarking.Wallet where
import           Cardano.Api hiding (txId)

import qualified Cardano.Api.Experimental as Exp
import qualified Cardano.Api.Experimental.Tx as Exp
import qualified Cardano.Ledger.Coin as L
import           Cardano.TxGenerator.FundQueue as FundQueue
import           Cardano.TxGenerator.Types
import           Cardano.TxGenerator.UTxO

import           Prelude

import           Control.Concurrent.MVar
import           Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Sequence as Seq

import           Streaming

-- | All the actual functionality of Wallet / WalletRef has been removed
-- and WalletRef has been stripped down to MVar FundQueue.
-- The implementation of Wallet has become trivial.
-- Todo: Remove trivial wrapper functions.
type WalletRef = MVar FundQueue

-- | The transactions a generator produces. A `Control.Monad.Error.MonadError`
-- might also fit here, but that's a deprecated interface relative to
-- `Control.Monad.Trans.Except.ExceptT`:
-- @
-- type TxStream m era = Stream (Of (Tx era)) m (Maybe TxGenError)
-- @
type TxStream m era = Stream (Of (Either TxGenError (Tx era))) m ()

-- | This creates a new `MVar` with an `emptyFundQueue` vs. truly
-- initializing a preexisting `WalletRef`.
initWallet :: IO WalletRef
initWallet = newMVar emptyFundQueue

-- | This reads an `MVar` and applies a function to the copied content.
askWalletRef :: WalletRef -> (FundQueue -> a) -> IO a
askWalletRef r f = do
  w <- readMVar r
  return $ f w

-- | This does an insertion into the `MVar` contents.
walletRefInsertFund :: WalletRef -> Fund -> IO ()
walletRefInsertFund ref fund = modifyMVar_  ref $ \w -> return $ FundQueue.insertFund w fund

-- | 'walletSource' takes @munch@ funds from the front of a wallet, for a
-- transaction to spend.
walletSource :: WalletRef -> Int -> FundSource IO
walletSource ref munch = modifyMVar ref $ \fifo -> return $ case removeFunds munch fifo of
  Nothing -> (fifo, Left $ TxGenError "WalletSource: out of funds")
  Just (newFifo, funds) -> (newFifo, Right funds)

-- | Just a preview of the wallet's funds; wallet remains unmodified.
walletPreview :: WalletRef -> Int -> IO [Fund]
walletPreview ref munch = do
  fifo <- readMVar ref
  return $ maybe (toList fifo) snd (removeFunds munch fifo)

-- | Where a pay mode pays outputs to: the wallet that keeps the funds they
-- become, and the output paying a value as the @j@-th output paid to the
-- destination, counting from 0 across all transactions.
data Destination era = Destination
  { destinationWallet :: !WalletRef
  , destinationOutput :: Int -> L.Coin -> Output era
  , destinationPaid   :: !(IORef Int)
    -- ^ The number of outputs paid to the destination so far, by transactions
    -- built and stored.
  }

-- | A destination that pays consecutive groups of @groupSize@ outputs with one
-- of @outputs@ each, cycling through them: output @j@ is paid with
-- @outputs !! ((j `div` groupSize) `mod` length outputs)@.
newCyclingDestination :: WalletRef -> Int -> [L.Coin -> Output era] -> IO (Destination era)
newCyclingDestination wallet groupSize outputs = Destination wallet output <$> newIORef 0
 where
  table = Seq.fromList outputs
  output j = Seq.index table ((j `div` max 1 groupSize) `mod` Seq.length table)

-- | A destination that pays every output with @output@.
newDestination :: WalletRef -> (L.Coin -> Output era) -> IO (Destination era)
newDestination wallet output = newCyclingDestination wallet 1 [output]

-- | An output of a transaction being built: one paid to a destination, which
-- keeps the fund it becomes, or one that no wallet keeps, such as a reference
-- script output.
data TxOutput era
  = Paid !(Destination era) !(Output era)
  | Untracked !(Exp.TxOut (ShelleyLedgerEra era))

txOutputTxOut :: TxOutput era -> Exp.TxOut (ShelleyLedgerEra era)
txOutputTxOut (Paid _ output) = outputTxOut output
txOutputTxOut (Untracked txOut) = txOut

-- | The outputs paying @values@ to a destination, following on from the
-- outputs paid to it so far. Planning outputs pays nothing yet: only
-- 'storeOutputs' counts them as paid.
payTo :: Destination era -> [L.Coin] -> IO [TxOutput era]
payTo d values = do
  paid <- readIORef (destinationPaid d)
  return [ Paid d (destinationOutput d j value) | (j, value) <- zip [paid ..] values ]

-- | 'storeOutputs' stores the funds that the outputs of transaction @txId@
-- become in their destinations' wallets, in output order, and counts them as
-- paid. @outputs@ are given in the order the transaction creates them.
storeOutputs :: Exp.EraCommonConstraints era => TxId -> [TxOutput era] -> IO ()
storeOutputs txId outputs = sequence_
  [ do walletRefInsertFund (destinationWallet d) (outputFund txId txIx output)
       modifyIORef' (destinationPaid d) (+ 1)
  | (txIx, Paid d output) <- zip [TxIx 0 ..] outputs ]

-- | 'buildAndStore' builds the transaction spending @inFunds@ into @outputs@
-- with @txGenerator@, and stores the funds its outputs become.
buildAndStore :: Exp.EraCommonConstraints era
  => TxGenerator era
  -> [Fund]
  -> [TxOutput era]
  -> ExceptT TxGenError IO (Tx era)
buildAndStore txGenerator inFunds outputs = do
  (tx, txId) <- hoistEither $ txGenerator inFunds (map txOutputTxOut outputs)
  liftIO $ storeOutputs txId outputs
  return tx
