{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

--------------------------------------------------------------------------------

module Test.TxCentrifuge.TxTest
  ( txTests
  , testSetup
  , mkDummyFund
  ) where

--------------------------------------------------------------------------------

----------
-- base --
----------
import System.IO (hFlush, hPutStrLn, stderr)
-----------
-- aeson --
-----------
import Data.Aeson qualified as Aeson
-----------------
-- cardano-api --
-----------------
import Cardano.Api qualified as Api
-------------------------
-- cardano-ledger-core --
-------------------------
import Cardano.Ledger.Coin qualified as L
-----------
-- clock --
-----------
import System.Clock qualified as Clock
-----------
-- tasty --
-----------
import Test.Tasty qualified as Tasty
-----------------
-- tasty-hunit --
-----------------
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
------------------
-- tx-generator --
------------------
import Cardano.TxGenerator.ProtocolParameters qualified as PP
---------------------
-- tx-centrifuge --
---------------------
import Cardano.Benchmarking.TxCentrifuge.Fund qualified as Fund
import Cardano.Benchmarking.TxCentrifuge.TxAssembly qualified as TxAssembly
import Paths_tx_centrifuge qualified as Paths

--------------------------------------------------------------------------------

txTests :: Tasty.TestTree
txTests = Tasty.testGroup "node"
  [ HUnit.testCase "buildTx: simple 1-in-1-out transaction" $ do
      (_ledgerPP, signKey, addr) <- testSetup
      let fund = mkDummyFund signKey 0 10_000_000
          fee  = L.Coin 200_000
      case TxAssembly.buildTx {-- ledgerPP --} addr signKey [fund] 1 fee of
        Left err ->
          HUnit.assertFailure $ "buildTx failed: " ++ err
        Right (tx, outFunds) -> do
          -- One output fund recycled.
          length outFunds @?= 1
          -- Output value = input - fee.
          Fund.fundValue (head outFunds) @?= (10_000_000 - 200_000)
          -- The recycled fund's TxIn references the new tx.
          let txId = Api.getTxId (Api.getTxBody tx)
          Fund.fundTxIn (head outFunds)
            @?= Api.TxIn txId (Api.TxIx 0)

  , HUnit.testCase "buildTx: 2-in-3-out transaction" $ do
      (_ledgerPP, signKey, addr) <- testSetup
      let fund1 = mkDummyFund signKey 0 5_000_000
          fund2 = mkDummyFund signKey 1 5_000_000
          fee   = L.Coin 200_000
      case TxAssembly.buildTx {-- ledgerPP --} addr signKey
             [fund1, fund2] 3 fee of
        Left err ->
          HUnit.assertFailure $ "buildTx failed: " ++ err
        Right (_tx, outFunds) -> do
          -- Three output funds.
          length outFunds @?= 3
          -- Total output = total input - fee.
          let totalOut = sum (map Fund.fundValue outFunds)
          totalOut @?= (10_000_000 - 200_000)

  , HUnit.testCase "buildTx: insufficient funds" $ do
      (_ledgerPP, signKey, addr) <- testSetup
      let fund = mkDummyFund signKey 0 100_000
          fee  = L.Coin 200_000
      case TxAssembly.buildTx {-- ledgerPP --} addr signKey [fund] 1 fee of
        Left _  -> pure () -- expected
        Right _ ->
          HUnit.assertFailure
            "buildTx should fail when funds < fee"

  , HUnit.testCase "buildTx: no input funds" $ do
      (_ledgerPP, signKey, addr) <- testSetup
      let fee = L.Coin 200_000
      case TxAssembly.buildTx {-- ledgerPP --} addr signKey [] 1 fee of
        Left _  -> pure () -- expected
        Right _ ->
          HUnit.assertFailure
            "buildTx should fail with no inputs"

  , HUnit.testCase "buildTx: zero outputs" $ do
      (_ledgerPP, signKey, addr) <- testSetup
      let fund = mkDummyFund signKey 0 10_000_000
          fee  = L.Coin 200_000
      case TxAssembly.buildTx {-- ledgerPP --} addr signKey [fund] 0 fee of
        Left _  -> pure () -- expected
        Right _ ->
          HUnit.assertFailure
            "buildTx should fail with 0 outputs"

  , HUnit.testCase
      "buildTx: signing throughput (single-threaded)" $ do
      (ledgerPP, signKey, addr) <- testSetup
      -- Build N transactions sequentially and measure wall-clock
      -- time. This quantifies the single-threaded builder bottleneck.
      let n = 10_000 :: Int
          fee = L.Coin 200_000
          -- Use a large initial fund so recycling doesn't deplete it.
          initialFund = mkDummyFund signKey 0 1_000_000_000_000

      start <- Clock.getTime Clock.MonotonicRaw
      go n initialFund ledgerPP addr signKey fee
      end <- Clock.getTime Clock.MonotonicRaw

      let elapsedNs = Clock.toNanoSecs (end - start)
          elapsedS  = fromIntegral elapsedNs / 1e9 :: Double
          tps       = fromIntegral n / elapsedS

      hPutStrLn stderr ""
      hPutStrLn stderr
        "      --- Single-threaded buildTx throughput ---"
      hPutStrLn stderr $ "        txs built:  " ++ show n
      hPutStrLn stderr $ "        elapsed:    " ++ show elapsedS ++ " s"
      hPutStrLn stderr $
        "        throughput: " ++ show (round tps :: Int) ++ " tx/s"
      hFlush stderr

      -- Sanity: we should be able to sign at least 1000 tx/s on any
      -- reasonable hardware. This is not a hard performance target,
      -- just a smoke test that buildTx isn't catastrophically slow.
      HUnit.assertBool
        ("buildTx throughput too low: "
          ++ show (round tps :: Int) ++ " tx/s")
        (tps > 1000)
  ]
  where
    -- Build N txs sequentially, recycling the first output each time.
    go :: Int -> Fund.Fund
       -> Api.LedgerProtocolParameters Api.ConwayEra
       -> Api.AddressInEra Api.ConwayEra
       -> Api.SigningKey Api.PaymentKey -> L.Coin -> IO ()
    go 0 _ _ _ _ _ = pure ()
    go remaining fund ledgerPP addr signKey fee =
      case TxAssembly.buildTx {-- ledgerPP --} addr signKey [fund] 1 fee of
        Left err ->
          error $ "throughput test: buildTx failed at iteration "
            ++ show remaining ++ ": " ++ err
        Right (_, outFunds) ->
          go (remaining - 1) (head outFunds)
            ledgerPP addr signKey fee

--------------------------------------------------------------------------------
-- Test helpers
--------------------------------------------------------------------------------

-- | Load protocol parameters and create common test fixtures.
testSetup
  :: IO ( Api.LedgerProtocolParameters Api.ConwayEra
        , Api.SigningKey Api.PaymentKey
        , Api.AddressInEra Api.ConwayEra
        )
testSetup = do
  -- Load protocol parameters from the CI test file.
  ppPath <- Paths.getDataFileName "data/protocol-parameters.ci-test.json"
  protocolParameters <-
    Aeson.eitherDecodeFileStrict' ppPath >>= either fail pure
  ledgerPP <-
    case PP.convertToLedgerProtocolParameters
           Api.ShelleyBasedEraConway protocolParameters of
      Left err ->
        fail $ "convertToLedgerProtocolParameters: " ++ show err
      Right pp -> pure pp

  -- Generate a fresh signing key and derive its address.
  signKey <- Api.generateSigningKey Api.AsPaymentKey
  let networkId = Api.Testnet (Api.NetworkMagic 42)
      addr = Api.shelleyAddressInEra
        (Api.shelleyBasedEra @Api.ConwayEra)
        $ Api.makeShelleyAddress networkId
            (Api.PaymentCredentialByKey
              (Api.verificationKeyHash
                (Api.getVerificationKey signKey)))
            Api.NoStakeAddress

  pure (ledgerPP, signKey, addr)

-- | Create a dummy fund with a synthetic TxIn.  Uses the signing key's
-- verification key hash to derive a deterministic TxId (via
-- 'Api.genesisUTxOPseudoTxIn') and the caller-supplied @index@ as the
-- 'Api.TxIx'.  Each distinct @index@ produces a unique 'Api.TxIn', so
-- multi-input tests can create several funds from the same key without
-- accidentally producing duplicate inputs.
mkDummyFund :: Api.SigningKey Api.PaymentKey -> Word -> Integer -> Fund.Fund
mkDummyFund signKey index lovelace = Fund.Fund
  { Fund.fundTxIn =
      let Api.TxIn txId _ = Fund.genesisTxIn
            (Api.Testnet (Api.NetworkMagic 42))
            signKey
      in Api.TxIn txId (Api.TxIx index)
  , Fund.fundValue = lovelace
  , Fund.fundSignKey = signKey
  }
