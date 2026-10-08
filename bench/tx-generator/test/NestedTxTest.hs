{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
module NestedTxTest
  ( nestedTxTests
  ) where

import           Prelude

import           Cardano.Api (AsType (AsPaymentKey), CardanoEra (DijkstraEra), DijkstraEra,
                   InAnyCardanoEra (..), NetworkId (..), NetworkMagic (..), PaymentKey,
                   SigningKey, Tx (ShelleyTx), TxId, TxIn (..), TxIx (..), fromShelleyTxId,
                   generateSigningKey, serialiseToCBOR, toShelleyAddr, toShelleyTxIn)
import           Cardano.Api.Experimental qualified as Exp
import           Cardano.Api.Experimental.Tx qualified as Exp
import           Cardano.Api.Ledger qualified as L
import           Cardano.Benchmarking.Wallet (createAndStore, mangle)
import           Cardano.Ledger.Api qualified as LA
import           Cardano.Ledger.Dijkstra.TxBody (DijkstraEraTxBody (subTransactionsTxBodyL))
import           Cardano.TxGenerator.Fund (Fund (..), FundInEra (..), getFundCoin, getFundTxIn)
import           Cardano.TxGenerator.Tx (genNestedTx, genTx, sourceToStoreNestedTransaction)
import           Cardano.TxGenerator.Types (TxGenError)
import           Cardano.TxGenerator.Utils (inputsToOutputsWithFee, keyAddress, mkTxIn)
import           Cardano.TxGenerator.UTxO (mkTxOutToAddress, mkUTxOVariant)

import           Data.Foldable (toList)
import           Data.IORef (modifyIORef, newIORef, readIORef)
import           Data.Map.Strict qualified as Map
import           Data.Set qualified as Set
import           Data.Text qualified as Text
import           Lens.Micro ((^.))
import           Test.Tasty
import           Test.Tasty.HUnit


nestedTxTests :: TestTree
nestedTxTests = testGroup "nested transactions (Dijkstra)"
  [ testCase "without sub-transactions, the same transaction as genTx" $ do
      key <- generateSigningKey AsPaymentKey
      let funds = mkFunds key 0 2
          outputs = map (txOut key) [9_500_000, 9_500_000]
      plain <- either (assertFailure . show) (pure . fst) $
        genTx pparams ([], []) fee mempty funds outputs
      nested <- either (assertFailure . show) (pure . fst3) $
        genNestedTx pparams ([], []) fee mempty funds outputs []
      serialiseToCBOR nested @?= serialiseToCBOR plain

  , testCase "sub-transactions are included, with distinct ids, and the batch balances" $ do
      key <- generateSigningKey AsPaymentKey
      let topFunds = mkFunds key 0 2
          subTxFunds = [ mkFunds key (10 * i) 2 | i <- [1 .. 3] ]
          subTx :: [Fund] -> ([Fund], [Exp.TxOut LA.DijkstraEra])
          subTx funds = (funds, map (txOut key) [10_000_000, 10_000_000])
      (tx, _, subTxIds) <- either (assertFailure . show) pure $
        genNestedTx pparams ([], []) fee mempty topFunds (map (txOut key) [9_500_000, 9_500_000])
          (map subTx subTxFunds)
      let ledgerTx = ledgerTxOf tx
          includedIds = map (fromShelleyTxId . LA.txIdTx) $ toList $ ledgerTx ^. LA.bodyTxL . subTransactionsTxBodyL
          utxo :: L.UTxO LA.DijkstraEra
          utxo = L.UTxO $ Map.fromList
            [ (toShelleyTxIn $ getFundTxIn f, L.mkBasicTxOut (addr key) (L.inject $ getFundCoin f))
            | f <- topFunds ++ concat subTxFunds ]
      length includedIds @?= 3
      includedIds @?= subTxIds
      Set.size (Set.fromList subTxIds) @?= 3
      LA.evalBalanceTxBody pparams (const Nothing) (const False) utxo (ledgerTx ^. LA.bodyTxL) @?= mempty

  , testCase "the outputs of a sub-transaction are stored under its own id" $ do
      key <- generateSigningKey AsPaymentKey
      stored <- newIORef []
      let funds = mkFunds key 0 (2 + 2 * 2)
          toStore = mangle $ repeat $ createAndStore (mkUTxOVariant @DijkstraEra network key)
                                                     (\fund -> modifyIORef stored (fund :))
      result <- sourceToStoreNestedTransaction
                  (genNestedTx pparams ([], []) fee mempty)
                  (pure $ Right funds)
                  2 2
                  (inputsToOutputsWithFee fee 2)
                  (inputsToOutputsWithFee 0 3)
                  toStore
      tx <- either (assertFailure . show) pure (result :: Either TxGenError (Tx DijkstraEra))
      storedTxIns <- map getFundTxIn <$> readIORef stored
      let ledgerTx = ledgerTxOf tx
          topId = fromShelleyTxId $ LA.txIdTx ledgerTx
          subIds = map (fromShelleyTxId . LA.txIdTx) $ toList $ ledgerTx ^. LA.bodyTxL . subTransactionsTxBodyL
          expected = txIns topId 2 ++ concatMap (`txIns` 3) subIds
      length subIds @?= 2
      Set.fromList storedTxIns @?= Set.fromList expected
      length storedTxIns @?= length expected
  ]
 where
  fst3 :: (a, b, c) -> a
  fst3 (a, _, _) = a

  txIns :: TxId -> Int -> [TxIn]
  txIns txId n = [ TxIn txId (TxIx $ fromIntegral i) | i <- [0 .. n - 1] ]

  ledgerTxOf :: Tx DijkstraEra -> L.Tx L.TopTx LA.DijkstraEra
  ledgerTxOf (ShelleyTx _ ledgerTx) = ledgerTx

network :: NetworkId
network = Testnet (NetworkMagic 42)

fee :: L.Coin
fee = 1_000_000

pparams :: L.PParams LA.DijkstraEra
pparams = LA.emptyPParams

addr :: SigningKey PaymentKey -> L.Addr
addr key = toShelleyAddr (keyAddress @DijkstraEra network key)

txOut :: SigningKey PaymentKey -> L.Coin -> Exp.TxOut LA.DijkstraEra
txOut key = mkTxOutToAddress (keyAddress @DijkstraEra network key)

-- | @n@ funds of 10 ada each, spendable with @key@, at distinct inputs from @first@ on.
mkFunds :: SigningKey PaymentKey -> Int -> Int -> [Fund]
mkFunds key first n =
  [ Fund $ InAnyCardanoEra DijkstraEra FundInEra
      { _fundTxIn       = mkTxIn $ "900fc5da77a0747da53f7675cbb7d149d46779346dea2f879ab811ccc72a2162#" <> Text.pack (show i)
      , _fundWitness    = Exp.AnyKeyWitnessPlaceholder
      , _fundVal        = 10_000_000
      , _fundSigningKey = Just key
      }
  | i <- [first .. first + n - 1]
  ]
