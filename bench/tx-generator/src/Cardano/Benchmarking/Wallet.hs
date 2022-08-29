{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.Benchmarking.Wallet
where
import           Prelude

import           Control.Concurrent.MVar

import           Cardano.Api

import           Cardano.TxGenerator.FundQueue as FundQueue
import           Cardano.TxGenerator.Tx (CreateAndStore, CreateAndStoreList)
import           Cardano.TxGenerator.Types
import           Cardano.TxGenerator.Utxo

import           Cardano.Benchmarking.Types (NumberOfTxs (..))

-- All the actual functionality of Wallet / WalletRef has been removed
-- and WalletRef has been stripped down to MVar FundQueue.
-- The implementation of Wallet has become trivial.
-- TODO: Remove trivial wrapper functions.

type WalletRef = MVar FundQueue


-- 'ToUTxOList era' is more powerful than '[ ToUTxO era ]' but
-- '[ ToUTxO era ]` is easier to construct.
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
walletSource ref munch = modifyMVar ref $ \fifo -> return $ case FundQueue.removeFunds munch fifo of
  Nothing -> (fifo, Left "WalletSource: out of funds")
  Just (newFifo, funds) -> (newFifo, Right funds)


mangleWithChange :: Monad m => CreateAndStore m era -> CreateAndStore m era -> CreateAndStoreList m era PayWithChange
mangleWithChange mkChange mkPayment outs = case outs of
  PayExact l -> mangle (repeat mkPayment) l
  PayWithChange change payments -> mangle (mkChange : repeat mkPayment) (change : payments)

mangle :: Monad m => [ CreateAndStore m era ] -> CreateAndStoreList m era [ Lovelace ]
mangle fkts values
  = (outs, \txId -> mapM_ (\f -> f txId) fs)
  where
    (outs, fs) =unzip $ map worker $ zip3 fkts values [TxIx 0 ..]
    worker (toUTxO, value, idx)
      = let (o, f ) = toUTxO value
         in  (o, f idx)


newtype WalletScript era = WalletScript { runWalletScript :: IO (WalletStep era) }

data WalletStep era
  = Done
  | NextTx !(WalletScript era) !(Tx era)
  | Error String

-- TODO:
-- Define generator for a single transaction and define combinator for
-- repeat and sequence.

benchmarkWalletScript :: forall era .
     IsShelleyBasedEra era
  => IO (Either String (Tx era)) -- make polymorphic
  -> NumberOfTxs
  -> WalletScript era
benchmarkWalletScript sourceToStore totalCount
  = WalletScript $ walletStep totalCount
 where
  walletStep :: NumberOfTxs -> IO (WalletStep era)
  walletStep (NumberOfTxs 0) = return Done
  walletStep count = sourceToStore >>= \case
    Left err -> return $ Error err
    Right tx -> return $ NextTx (benchmarkWalletScript sourceToStore (pred count)) tx

limitSteps ::
     NumberOfTxs
  -> WalletScript era
  -> WalletScript era
limitSteps = undefined          -- FIXME
