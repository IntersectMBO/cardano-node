{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.TxCentrifuge.SizeTest (tests) where

import Cardano.Api qualified as Api
import Cardano.Benchmarking.PullFiction.Config.Raw qualified as Raw
import Cardano.Benchmarking.TxCentrifuge.Fund qualified as Fund
import Cardano.Benchmarking.TxCentrifuge.TxAssembly qualified as Tx
import Cardano.Benchmarking.TxCentrifuge.ValueBuilder qualified as Builder
import Cardano.Ledger.Api qualified as Ledger
import Cardano.Ledger.Coin (Coin(..))
import Control.Monad (forM_)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Lens.Micro ((^.))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import Test.TxCentrifuge.TxTest (testSetup, mkDummyFund)

tests :: TestTree
tests = testGroup "minimum signed size"
  [ testCase "minimum, CBOR boundaries, legacy filler and signed roundtrip" $ do
      (_, key, addr) <- testSetup
      let funds = [mkDummyFund key 0 1_000_000_000]
          buildRaw n = requireRight $ Tx.buildTx addr key funds 1 fee n
          buildSized n = requireRight $ Tx.buildTxSized addr key funds 1 fee n limits
      base <- buildRaw 0
      forM_ [1, size base] $ \target -> do
        built <- buildSized (fromIntegral target)
        bytes built @?= bytes base
      forM_ (map (size base +) ([1..180] ++ [1500..1600] ++ [17000])) $ \target -> do
        built@(tx, outs) <- buildSized (fromIntegral target)
        assertBool "minimum reached" (size built >= target)
        assertBool "bounded metadata overhead" (size built <= target + 48)
        let payloadBytes = sum (map BS.length (chunks tx))
        assertBool "nonempty, bounded byte strings" $
          payloadBytes > 0 && all (\b -> not (BS.null b) && BS.length b <= 64) (chunks tx)
        raw <- buildRaw (fromIntegral payloadBytes)
        bytes raw @?= bytes built
        smaller <- buildRaw (fromIntegral payloadBytes - 1)
        assertBool "smallest filler payload" (size smaller < target)
        map Fund.fundValue outs @?= [998_000_000]
        decoded <- requireRight $ Api.deserialiseFromCBOR (Api.AsTx Api.AsConwayEra) (bytes built)
        Api.serialiseToCBOR decoded @?= bytes built

  , testCase "witness count and output shape are included" $ do
      (_, key, addr) <- testSetup
      (_, secondKey, _) <- testSetup
      let funds = [mkDummyFund key 0 1_000_000_000, mkDummyFund secondKey 0 1_000_000_000]
      (tx, outs) <- requireRight $ Tx.buildTxSized addr key funds 10 fee 4096 limits
      length (Api.getTxWitnesses tx) @?= 2
      length outs @?= 10
      recycled <- requireRight $ Tx.buildTxSized addr key outs 1 fee 4096 limits
      length (Api.getTxWitnesses (fst recycled)) @?= 1
      assertBool "initial target reached" (BS.length (Api.serialiseToCBOR tx) >= 4096)
      assertBool "recycled target reached" (size recycled >= 4096)

  , testCase "fixed fee and final size checked, including rounding at maxTxSize" $ do
      (_, key, addr) <- testSetup
      let funds = [mkDummyFund key 0 1_000_000_000]
          build cost target cap = Tx.buildTxSized addr key funds 1 cost target cap
      built <- requireRight $ build fee 4096 limits
      let Api.ShelleyTx _ ledgerTx = fst built
          required = 155381 + 44 * fromIntegral (ledgerTx ^. Ledger.sizeTxF)
      _ <- requireRight $ build (Coin required) 4096 limits
      assertLeft "fee is too small" $ build (Coin (required - 1)) 4096 limits
      forM_ [0, 65537, 10 ^ (100 :: Int)] $ \target ->
        assertLeft "tx_size" $ build fee target limits
      base <- requireRight $ Tx.buildTx addr key funds 1 fee 0
      let maximumSize = fromIntegral (size base + 1)
      assertLeft "maxTxSize" $ build fee (fromIntegral maximumSize) (limits {Tx.maxTxSize = maximumSize})

  , testCase "builder defaults and mutually exclusive size modes" $ do
      let parse fields = Builder.parseValueBuilder $ Raw.Builder "value" (Aeson.object fields) Nothing
          common = ["fee" .= (2_000_000 :: Integer)]
      defaults <- requireRight $ parse common
      Builder.metadataBytes defaults @?= 0
      Builder.txSize defaults @?= Nothing
      sized <- requireRight $ parse (common ++ ["tx_size" .= (4096 :: Int)])
      Builder.txSize sized @?= Just 4096
      raw <- requireRight $ parse (common ++ ["metadata_bytes" .= (256 :: Int)])
      Builder.metadataBytes raw @?= 256
      forM_ [0, 256 :: Int] $ \metadata ->
        assertLeft "mutually exclusive" $ parse (common ++ ["metadata_bytes" .= metadata, "tx_size" .= (4096 :: Int)])
      assertLeft "mutually exclusive" $ parse (common ++ ["metadata_bytes" .= Aeson.Null, "tx_size" .= (4096 :: Int)])
      assertLeft "tx_size" $ parse (common ++ ["tx_size" .= (0 :: Int)])
      assertBool "negative size rejected" $ case parse (common ++ ["tx_size" .= (-1 :: Int)]) of
        Left _ -> True
        Right _ -> False
  ]
 where
  fee = Coin 2_000_000
  limits = Tx.TxLimits 65536 44 155381
  bytes = Api.serialiseToCBOR . fst
  size = BS.length . bytes

chunks :: Api.Tx Api.ConwayEra -> [BS.ByteString]
chunks tx = case Api.txMetadata (Api.getTxBodyContent (Api.getTxBody tx)) of
  Api.TxMetadataNone -> []
  Api.TxMetadataInEra _ (Api.TxMetadata metadata) -> case Map.lookup 0 metadata of
    Just (Api.TxMetaList xs) -> [bs | Api.TxMetaBytes bs <- xs]
    _ -> []

requireRight :: Show e => Either e a -> IO a
requireRight = either (\err -> assertFailure (show err) >> fail "unreachable") pure

assertLeft :: String -> Either String a -> IO ()
assertLeft expected result = case result of
  Left err -> assertBool err (expected `isInfixOf` err)
  Right _ -> assertFailure ("expected failure containing " ++ expected)
