{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Cardano.Benchmarking.Wallet
Description : Basic functions to manipulate wallets.

Beyond the basic wallet functions, there are some functions
manipulating the 'FundQueue' held in a 'WalletRef' by side
effect, like 'createAndStore' and 'mangle'.
-}
module Cardano.Benchmarking.Wallet where
import           Cardano.Api hiding (txId)

import qualified Cardano.Ledger.Coin as L
import           Cardano.TxGenerator.FundQueue as FundQueue
import           Cardano.TxGenerator.Tx
import           Cardano.TxGenerator.Types
import           Cardano.TxGenerator.UTxO

import           Prelude

import           Control.Concurrent.MVar
import           Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map

import           Streaming

-- | All the actual functionality of Wallet / WalletRef has been removed
-- and WalletRef has been stripped down to MVar FundQueue.
-- The implementation of Wallet has become trivial.
-- Todo: Remove trivial wrapper functions.
type WalletRef = MVar FundQueue

-- | 'ToUTxOList era' is more powerful than '[ ToUTxO era ]' but
-- '[ ToUTxO era ]` is easier to construct.
-- There is also a thought that a `Control.Monad.Error.MonadError`
-- might fit here, but that's a deprecated interface relative to
-- `Control.Monad.Trans.Except.ExceptT`:
-- @
-- type TxStream m era = Stream (Of (Tx era)) m (Maybe TxGenError)
-- @
type TxStream m era = Stream (Of (Either TxGenError (Tx era))) m ()

-- | 'createAndStore' hides its 3rd argument in the 'CreateAndStore'
-- type alias. The sole uses are in "Cardano.Benchmarking.Script.Core",
-- in 'Cardano.Benchmarking.Script.Core.interpretPayMode'. The 2nd
-- @store@ argument is always passed as 'mkWalletFundStore' applied
-- to a 'WalletRef'.
createAndStore :: ToUTxO era -> (Fund -> m ()) -> CreateAndStore m era
createAndStore create store lovelace = (utxo, toStore)
  where
    (utxo, mkFund) = create lovelace
    toStore txIx txId = store $ mkFund txIx txId

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

-- | 'mkWalletFundStoreList' hides its second argument in
-- 'FundToStoreList'. This is not used anywhere.
mkWalletFundStoreList :: WalletRef -> FundToStoreList IO
mkWalletFundStoreList walletRef funds = modifyMVar_  walletRef
  $ \wallet -> return (foldl FundQueue.insertFund wallet funds)

-- | 'mkWalletFundStore' hides its second argument in 'FundToStore'.
-- This is only ever called in tandem with 'createAndStore' in
-- 'Cardano.Benchmarking.Script.Core.interpretPayMode'. It's only
-- ever partially applied to make a function that modifies the
-- 'WalletRef' 'MVar' by side effect.
mkWalletFundStore :: WalletRef -> FundToStore IO
mkWalletFundStore walletRef fund = modifyMVar_  walletRef
  $ \wallet -> return $ FundQueue.insertFund wallet fund

-- | 'walletSource' is only ever used in
-- 'Cardano.Benchmarking.Script.Core.evalGenerator' to pass
-- to 'Cardano.TxGenerator.Tx.sourceToStoreTransaction' and
-- its associated functions.
walletSource :: WalletRef -> Int -> FundSource IO
walletSource ref munch = modifyMVar ref $ \fifo -> return $ case removeFunds munch fifo of
  Nothing -> (fifo, Left $ TxGenError "WalletSource: out of funds")
  Just (newFifo, funds) -> (newFifo, Right funds)

-- | Just a preview of the wallet's funds; wallet remains unmodified.
walletPreview :: WalletRef -> Int -> IO [Fund]
walletPreview ref munch = do
  fifo <- readMVar ref
  return $ maybe (toList fifo) snd (removeFunds munch fifo)

-- | The second argument to 'mangleWithChange' is hidden in the
-- 'CreateAndStoreList' type. When there is change to be made,
-- it makes separate transactions to pay the change and sends
-- them off to 'mangle' to get zips of applications. This is
-- only ever used once, in
-- 'Cardano.Benchmarking.Script.Core.evalGenerator' for the
-- 'Cardano.Benchmarking.Script.Types.Split' case.
mangleWithChange :: Monad m => CreateAndStore m era -> [CreateAndStore m era] -> CreateAndStoreList m era PayWithChange
mangleWithChange mkChange mkPayments outs = case outs of
  PayExact l -> mangle mkPayments l
  PayWithChange change payments -> mangle (mkChange : mkPayments) (change : payments)

-- | The second argument to 'mangle' is hidden in the
-- 'CreateAndStoreList' type. This is basically
-- @second (\x -> (mapM_ ($ x)))
-- . unzip
-- $ zipWith3 (\x y z -> second ($ z) (x y))@
-- but relatively obfuscated.
-- This appears to mostly be list processing and function application.
-- and is only ever used by 'Cardano.Bencharking.Script.Core.evalGenerator'
-- to handle several of the cases of
-- 'Cardano.Benchmarking.Script.Types.Generator', though it's also
-- indirectly invoked via 'mangleWithChange' once.
-- The only caller not passing a constant list built with 'repeat'
-- as the first @fkts@ argument is 'mangleWithChange' above. This
-- is likely worth refactoring for the sake of maintainability.
-- NOTE: @russoul: I've got to say: this function is cryptic... Shall we invest in a straightforward design?
-- NOTE: This note extends to other functions such as `cyclicOutputs` too.
mangle :: Monad m => [ CreateAndStore m era ] -> CreateAndStoreList m era [ L.Coin ]
mangle fkts values
  = (outs, \txId -> mapM_ (\f -> f txId) fs)
  where
    (outs, fs) = unzip $ map worker $ zip3 fkts values [TxIx 0 ..]
    worker (toUTxO, value, idx)
      = let (o, f ) = toUTxO value
         in  (o, f idx)

-- | 'cyclingOutputs' creates outputs that lock consecutive groups of
-- @groupSize@ outputs at one variant each, cycling through the variants'
-- 'ToUTxO's, and stores them with @store@. It returns the output creators for
-- one transaction at a time: each transaction continues where the outputs
-- stored so far left off, so the cycle runs on across transactions.
cyclingOutputs :: Int -> [ToUTxO era] -> FundToStore IO -> IO (IO [CreateAndStore IO era])
cyclingOutputs groupSize variants store = do
  stored <- newIORef (0 :: Int)
  let
    table = Map.fromList $ zip [0 ..] variants
    count = Map.size table
    countAndStore fund = modifyIORef' stored (+ 1) >> store fund
    creator j = createAndStore (table Map.! ((j `div` max 1 groupSize) `mod` count)) countAndStore
  return $ do
    next <- readIORef stored
    return $ map creator [next ..]
