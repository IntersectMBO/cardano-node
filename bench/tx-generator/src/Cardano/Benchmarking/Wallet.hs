{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.Benchmarking.Wallet
where
import           Prelude

import           Streaming
import           Control.Concurrent.MVar

import           Cardano.Api

import           Cardano.TxGenerator.FundQueue as FundQueue
import           Cardano.TxGenerator.Types
import           Cardano.TxGenerator.Tx
import           Cardano.TxGenerator.UTxO

-- All the actual functionality of Wallet / WalletRef has been removed
-- and WalletRef has been stripped down to MVar FundQueue.
-- The implementation of Wallet has become trivial.
-- Todo: Remove trivial wrapper functions.

type WalletRef = MVar FundQueue

-- 'ToUTxOList era' is more powerful than '[ ToUTxO era ]' but
-- '[ ToUTxO era ]` is easier to construct.

--type TxStream m era = Stream (Of (Tx era)) m (Maybe TxGenError)
type TxStream m era = Stream (Of (Either TxGenError (Tx era))) m ()

createAndStore :: ToUTxO era -> (Fund -> m ()) -> CreateAndStore m era
createAndStore create store lovelace = (utxo, toStore)
  where
    (utxo, mkFund) = create lovelace
    toStore txIx txId = store $ mkFund txIx txId

initWallet :: IO WalletRef
initWallet = newMVar emptyFundQueue

askWalletRef :: WalletRef -> (FundQueue -> a) -> IO a
askWalletRef r f = do
  w <- readMVar r
  return $ f w

walletRefInsertFund :: WalletRef -> Fund -> IO ()
walletRefInsertFund ref fund = modifyMVar_  ref $ \w -> return $ FundQueue.insertFund w fund

mkWalletFundStoreList :: WalletRef -> FundToStoreList IO
mkWalletFundStoreList walletRef funds = modifyMVar_  walletRef
  $ \wallet -> return (foldl FundQueue.insertFund wallet funds)

mkWalletFundStore :: WalletRef -> FundToStore IO
mkWalletFundStore walletRef fund = modifyMVar_  walletRef
  $ \wallet -> return $ FundQueue.insertFund wallet fund

walletSource :: WalletRef -> Int -> FundSource IO
walletSource ref munch = modifyMVar ref $ \fifo -> return $ case removeFunds munch fifo of
  Nothing -> (fifo, Left $ TxGenError "WalletSource: out of funds")
  Just (newFifo, funds) -> (newFifo, Right funds)

-- just a preview of the wallet's funds; wallet remains unmodified
walletPreview :: WalletRef -> Int -> IO [Fund]
walletPreview ref munch = do
  fifo <- readMVar ref
  return $ maybe (toList fifo) snd (removeFunds munch fifo)

-- | The second argument to 'mangleWithChange' is hidden in the
-- 'CreateAndStoreList' type. When there is change to be made,
-- it makes separate transactions to pay the change and sends
-- them off to 'mangle' to get zips of applications.
mangleWithChange :: Monad m => CreateAndStore m era -> CreateAndStore m era -> CreateAndStoreList m era PayWithChange
mangleWithChange mkChange mkPayment outs = case outs of
  PayExact l -> mangle (repeat mkPayment) l
  PayWithChange change payments -> mangle (mkChange : repeat mkPayment) (change : payments)

-- | The second argument to 'mangle' is hidden in the
-- 'CreateAndStoreList' type. This is basically
-- @second (\x -> (mapM_ ($ x)))
-- . unzip
-- $ zipWith3 (\x y z -> second ($ z) (x y))@
-- but relatively obfuscated.
-- This appears to mostly be list processing and function application.
-- and gets used by 'Cardano.Bencharking.Script.Core.evalGenerator'
-- to handle several of the cases of
-- 'Cardano.Benchmarking.Script.Types.Generator'
mangle :: Monad m => [ CreateAndStore m era ] -> CreateAndStoreList m era [ Lovelace ]
mangle fkts values 
  = (outs, \txId -> mapM_ (\f -> f txId) fs)
  where
    (outs, fs) =unzip $ map worker $ zip3 fkts values [TxIx 0 ..]
    worker (toUTxO, value, idx)
      = let (o, f ) = toUTxO value
         in  (o, f idx) 
