{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Cli.Transaction.Validate
  ( hprop_transaction_validate
  ) where

import           Cardano.Api

import           Cardano.Testnet

import           Prelude

import           Control.Monad (void)
import           Data.Default.Class
import qualified Data.Text as Text
import           GHC.IO.Exception (ExitCode (..))
import           System.FilePath ((</>))

import           Testnet.Components.Configuration
import           Testnet.Components.Query (findLargestUtxoForPaymentKey,
                   findLargestUtxoWithAddress, getEpochStateView, retryUntilJustM,
                   TestnetWaitPeriod (..), waitForBlocks)
import           Testnet.Defaults (plutusV3Script, plutusV3SupplementalDatumScript)
import           Testnet.Process.Run (execCli', execCliAny, mkExecConfig)
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Types

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- Deterministic signing key for reproducible golden tests.
-- Key hash: fe8c2213ed33ae44e3629706a3c7d34f0dce29753646602177756a5f
-- Address (testnet-magic 42): addr_test1vrlgcgsna5e6u38rv2tsdg786d8smn3fw5myvcppwa6k5hc4u8utx
testSigningKey :: String
testSigningKey =
  "{\"type\":\"PaymentSigningKeyShelley_ed25519\",\"description\":\"Payment Signing Key\",\"cborHex\":\"5820aabbccddee0011223344556677889900aabbccddee00112233445566778899aa\"}"

-- | Integration test for @cardano-cli latest transaction validate@.
--
-- Spins up a testnet then validates several transactions (both valid and
-- invalid) using the new validate command, comparing output against golden
-- files.
--
-- Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/transaction validate/"'@
-- If you want to recreate golden files, set RECREATE_GOLDEN_FILES=1
hprop_transaction_validate :: Property
hprop_transaction_validate = integrationRetryWorkspace 2 "transaction validate" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  conf@Conf{tempAbsPath} <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  let sbe = ShelleyBasedEraConway
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era
      tempBaseAbsPath = makeTmpBaseAbsPath $ TmpAbsolutePath tempAbsPath'
      creationOptions = def{creationEra = AnyShelleyBasedEra sbe}

  TestnetRuntime
    { configurationFile
    , testnetMagic
    , testnetNodes
    , wallets = wallet0 : wallet1 : _
    } <-
    createAndRunTestnet creationOptions def conf

  node <- H.headM testnetNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket node
  execConfig <- mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic
  epochStateView <- getEpochStateView configurationFile (nodeSocketPath node)

  -- Write deterministic key to file and derive address
  let testSKeyFile = work </> "test-skey.json"
      testVKeyFile = work </> "test-vkey.json"
  H.writeFile testSKeyFile testSigningKey
  void $ execCli' execConfig
    [ "latest", "key", "verification-key"
    , "--signing-key-file", testSKeyFile
    , "--verification-key-file", testVKeyFile
    ]
  testAddr <- filter (/= '\n') <$>
    execCli' execConfig
      [ "latest", "address", "build"
      , "--payment-verification-key-file", testVKeyFile
      ]
  H.note_ $ "Deterministic test address: " <> testAddr

  -- ====================================================================
  -- Setup: Fund the deterministic address with known amounts
  -- ====================================================================
  H.note_ "=== Setup: funding deterministic address ==="
  let fundingAmount = 10_000_000 :: Int
      wallet0SKeyFile = signingKeyFp $ paymentKeyInfoPair wallet0
      wallet0Addr = Text.unpack $ paymentKeyInfoAddr wallet0

  txinFund <- findLargestUtxoForPaymentKey epochStateView sbe wallet0
  let fundTxBody = work </> "fund-tx.body"
      fundTxSigned = work </> "fund-tx.signed"

  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "build"
    , "--change-address", wallet0Addr
    , "--tx-in", Text.unpack $ renderTxIn txinFund
    , "--tx-out", testAddr <> "+" <> show fundingAmount
    , "--tx-out", testAddr <> "+" <> show fundingAmount
    , "--tx-out", testAddr <> "+" <> show fundingAmount
    , "--tx-out", testAddr <> "+" <> show fundingAmount
    , "--out-file", fundTxBody
    ]

  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "sign"
    , "--tx-body-file", fundTxBody
    , "--signing-key-file", wallet0SKeyFile
    , "--out-file", fundTxSigned
    ]

  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "submit"
    , "--tx-file", fundTxSigned
    ]

  H.noteShowM_ $ waitForBlocks epochStateView 1

  -- Find the funded UTxOs at our deterministic address
  testUtxos <- retryUntilJustM epochStateView (WaitForBlocks 3) $ do
    utxos <- findLargestUtxoWithAddress epochStateView sbe $ Text.pack testAddr
    case utxos of
      Just _ -> return $ Just ()
      Nothing -> return Nothing

  _ <- H.note $ "Test UTxOs funded: " <> show testUtxos

  -- ====================================================================
  -- Test 1: Valid simple ADA transfer
  -- ====================================================================
  H.note_ "=== Test 1: Valid simple transaction ==="

  txin1 <- findLargestUtxoForPaymentKey epochStateView sbe wallet0
  let validTxBody = work </> "valid-tx.body"
      validTxSigned = work </> "valid-tx.signed"

  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "build"
    , "--change-address", wallet0Addr
    , "--tx-in", Text.unpack $ renderTxIn txin1
    , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet1) <> "+" <> show @Int 5_000_000
    , "--out-file", validTxBody
    ]

  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "sign"
    , "--tx-body-file", validTxBody
    , "--signing-key-file", wallet0SKeyFile
    , "--out-file", validTxSigned
    ]

  (exitCode1, stdout1, stderr1) <- execCliAny execConfig
    [ anyEraToString cEra, "transaction", "validate"
    , "--tx-file", validTxSigned
    ]

  H.note_ $ "Exit code: " <> show exitCode1
  H.note_ $ "Stdout: " <> stdout1
  H.note_ $ "Stderr: " <> stderr1

  exitCode1 H.=== ExitSuccess
  H.diffVsGoldenFile
    stdout1
    "test/cardano-testnet-test/files/golden/tx_validate/valid_simple.out"

  -- ====================================================================
  -- Test 2: Fee too low (build-raw with deterministic UTxO)
  -- The deterministic UTxO has exactly 10,000,000 lovelace.
  -- output = 10,000,000 - 100 = 9,999,900 so value IS conserved,
  -- but fee=100 < min_fee=194.
  -- ====================================================================
  H.note_ "=== Test 2: Fee too low ==="

  testTxin2 <- fmap fst . H.nothingFailM $
    findLargestUtxoWithAddress epochStateView sbe $ Text.pack testAddr
  let feeTooLowTxBody = work </> "fee-too-low-tx.body"
      feeTooLowTxSigned = work </> "fee-too-low-tx.signed"

  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "build-raw"
    , "--tx-in", Text.unpack $ renderTxIn testTxin2
    , "--tx-out", testAddr <> "+9999900"
    , "--fee", "100"
    , "--out-file", feeTooLowTxBody
    ]

  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "sign"
    , "--tx-body-file", feeTooLowTxBody
    , "--signing-key-file", testSKeyFile
    , "--out-file", feeTooLowTxSigned
    ]

  (exitCode2, stdout2, stderr2) <- execCliAny execConfig
    [ anyEraToString cEra, "transaction", "validate"
    , "--tx-file", feeTooLowTxSigned
    ]

  H.note_ $ "Exit code: " <> show exitCode2
  H.note_ $ "Stdout: " <> stdout2
  H.note_ $ "Stderr: " <> stderr2

  exitCode2 H.=== ExitFailure 1
  H.diffVsGoldenFile
    stdout2
    "test/cardano-testnet-test/files/golden/tx_validate/fee_too_low.out"

  -- ====================================================================
  -- Test 3: Value not conserved
  -- Deterministic UTxO has 10,000,000 lovelace.
  -- output = 999,999,000,000, fee = 200,000
  -- consumed = 10,000,000, produced = 999,999,200,000
  -- ====================================================================
  H.note_ "=== Test 3: Value not conserved ==="

  let valueNotConservedTxBody = work </> "value-not-conserved-tx.body"
      valueNotConservedTxSigned = work </> "value-not-conserved-tx.signed"

  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "build-raw"
    , "--tx-in", Text.unpack $ renderTxIn testTxin2
    , "--tx-out", testAddr <> "+999999000000"
    , "--fee", "200000"
    , "--out-file", valueNotConservedTxBody
    ]

  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "sign"
    , "--tx-body-file", valueNotConservedTxBody
    , "--signing-key-file", testSKeyFile
    , "--out-file", valueNotConservedTxSigned
    ]

  (exitCode3, stdout3, stderr3) <- execCliAny execConfig
    [ anyEraToString cEra, "transaction", "validate"
    , "--tx-file", valueNotConservedTxSigned
    ]

  H.note_ $ "Exit code: " <> show exitCode3
  H.note_ $ "Stdout: " <> stdout3
  H.note_ $ "Stderr: " <> stderr3

  exitCode3 H.=== ExitFailure 1
  H.diffVsGoldenFile
    stdout3
    "test/cardano-testnet-test/files/golden/tx_validate/value_not_conserved.out"

  -- ====================================================================
  -- Test 4: Missing witness
  -- Build a tx spending from the deterministic key's UTxO,
  -- but sign with wallet1's key instead.
  -- Expected missing key hash: fe8c2213ed33ae44e3629706a3c7d34f0dce29753646602177756a5f
  -- ====================================================================
  H.note_ "=== Test 4: Missing witness ==="

  let missingWitnessTxBody = work </> "missing-witness-tx.body"
      missingWitnessTxSigned = work </> "missing-witness-tx.signed"
      wallet1SKeyFile = signingKeyFp $ paymentKeyInfoPair wallet1

  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "build-raw"
    , "--tx-in", Text.unpack $ renderTxIn testTxin2
    , "--tx-out", testAddr <> "+9999600"
    , "--fee", "400"
    , "--out-file", missingWitnessTxBody
    ]

  -- Sign with wallet1's key instead of the deterministic test key
  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "sign"
    , "--tx-body-file", missingWitnessTxBody
    , "--signing-key-file", wallet1SKeyFile
    , "--out-file", missingWitnessTxSigned
    ]

  (exitCode4, stdout4, stderr4) <- execCliAny execConfig
    [ anyEraToString cEra, "transaction", "validate"
    , "--tx-file", missingWitnessTxSigned
    ]

  H.note_ $ "Exit code: " <> show exitCode4
  H.note_ $ "Stdout: " <> stdout4
  H.note_ $ "Stderr: " <> stderr4

  exitCode4 H.=== ExitFailure 1
  H.diffVsGoldenFile
    stdout4
    "test/cardano-testnet-test/files/golden/tx_validate/missing_witness.out"

  -- ====================================================================
  -- Test 5: Valid Plutus script transaction (always-succeeds)
  -- Uses plutusV3Script which is a trivial always-succeeds PlutusV3 script.
  -- ====================================================================
  H.note_ "=== Test 5: Valid Plutus script ==="
  let alwaysSucceedsScript = work </> "always-succeeds.plutusV3"
  H.writeFile alwaysSucceedsScript $ Text.unpack plutusV3Script

  alwaysSucceedsAddr <- filter (/= '\n') <$>
    execCli' execConfig
      [ "latest", "address", "build"
      , "--payment-script-file", alwaysSucceedsScript
      ]

  scriptDatumHash <- filter (/= '\n') <$>
    execCli' execConfig
      [ "latest", "transaction", "hash-script-data"
      , "--script-data-value", "0"
      ]

  -- Send ADA to the always-succeeds script address
  txinForScript <- findLargestUtxoForPaymentKey epochStateView sbe wallet0
  let sendToScriptTxBody = work </> "send-to-script-tx.body"
      sendToScriptTxSigned = work </> "send-to-script-tx.signed"

  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "build"
    , "--change-address", wallet0Addr
    , "--tx-in", Text.unpack $ renderTxIn txinForScript
    , "--tx-out", alwaysSucceedsAddr <> "+" <> show @Int 5_000_000
    , "--tx-out-datum-hash", scriptDatumHash
    , "--out-file", sendToScriptTxBody
    ]

  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "sign"
    , "--tx-body-file", sendToScriptTxBody
    , "--signing-key-file", wallet0SKeyFile
    , "--out-file", sendToScriptTxSigned
    ]

  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "submit"
    , "--tx-file", sendToScriptTxSigned
    ]

  -- Wait for the UTxO to appear at the script address
  txinCollateral <- findLargestUtxoForPaymentKey epochStateView sbe wallet1
  alwaysSucceedsTxIn <- fmap fst . retryUntilJustM epochStateView (WaitForBlocks 3) $
    findLargestUtxoWithAddress epochStateView sbe $ Text.pack alwaysSucceedsAddr

  -- Spend from the always-succeeds script address
  let spendScriptTxBody = work </> "spend-script-tx.body"
      spendScriptTxSigned = work </> "spend-script-tx.signed"

  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet1
    , "--tx-in-collateral", Text.unpack $ renderTxIn txinCollateral
    , "--tx-in", Text.unpack $ renderTxIn alwaysSucceedsTxIn
    , "--tx-in-script-file", alwaysSucceedsScript
    , "--tx-in-datum-value", "0"
    , "--tx-in-redeemer-value", "0"
    , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet1) <> "+" <> show @Int 2_000_000
    , "--out-file", spendScriptTxBody
    ]

  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "sign"
    , "--tx-body-file", spendScriptTxBody
    , "--signing-key-file", wallet1SKeyFile
    , "--out-file", spendScriptTxSigned
    ]

  (exitCode5, stdout5, stderr5) <- execCliAny execConfig
    [ anyEraToString cEra, "transaction", "validate"
    , "--tx-file", spendScriptTxSigned
    ]

  H.note_ $ "Exit code: " <> show exitCode5
  H.note_ $ "Stdout: " <> stdout5
  H.note_ $ "Stderr: " <> stderr5

  exitCode5 H.=== ExitSuccess
  H.diffVsGoldenFile
    stdout5
    "test/cardano-testnet-test/files/golden/tx_validate/valid_plutus.out"

  -- ====================================================================
  -- Test 6: Failing Plutus script (supplemental datum not found)
  -- Uses plutusV3SupplementalDatumScript which requires a supplemental
  -- datum with hash of integer 1 to be present in the transaction.
  -- We intentionally omit it so the script fails during evaluation.
  -- ====================================================================
  H.note_ "=== Test 6: Failing Plutus script ==="
  let supplementalDatumScript = work </> "supplemental-datum.plutusV3"
  H.writeFile supplementalDatumScript $ Text.unpack plutusV3SupplementalDatumScript

  supplementalDatumScriptAddr <- filter (/= '\n') <$>
    execCli' execConfig
      [ "latest", "address", "build"
      , "--payment-script-file", supplementalDatumScript
      ]

  -- Send ADA to the supplemental datum script address
  txinForScript2 <- findLargestUtxoForPaymentKey epochStateView sbe wallet1
  let sendToScript2TxBody = work </> "send-to-script-2-tx.body"
      sendToScript2TxSigned = work </> "send-to-script-2-tx.signed"

  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet1
    , "--tx-in", Text.unpack $ renderTxIn txinForScript2
    , "--tx-out", supplementalDatumScriptAddr <> "+" <> show @Int 5_000_000
    , "--tx-out-datum-hash", scriptDatumHash
    , "--out-file", sendToScript2TxBody
    ]

  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "sign"
    , "--tx-body-file", sendToScript2TxBody
    , "--signing-key-file", wallet1SKeyFile
    , "--out-file", sendToScript2TxSigned
    ]

  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "submit"
    , "--tx-file", sendToScript2TxSigned
    ]

  -- Wait for the UTxO to appear at the script address
  txinCollateral2 <- findLargestUtxoForPaymentKey epochStateView sbe wallet0
  supplementalDatumTxIn <- fmap fst . retryUntilJustM epochStateView (WaitForBlocks 3) $
    findLargestUtxoWithAddress epochStateView sbe $ Text.pack supplementalDatumScriptAddr

  -- Build spending tx WITHOUT the supplemental datum → script will fail
  let failScriptTxBody = work </> "fail-script-tx.body"
      failScriptTxSigned = work </> "fail-script-tx.signed"

  void $ execCli' execConfig
    [ anyEraToString cEra, "query", "protocol-parameters"
    , "--out-file", work </> "pparams.json"
    ]

  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "build-raw"
    , "--tx-in-collateral", Text.unpack $ renderTxIn txinCollateral2
    , "--tx-in", Text.unpack $ renderTxIn supplementalDatumTxIn
    , "--tx-in-script-file", supplementalDatumScript
    , "--tx-in-datum-value", "0"
    , "--tx-in-redeemer-value", "0"
    , "--tx-in-execution-units", "(5000000000, 10000000)"
    , "--tx-out", wallet0Addr <> "+" <> show @Int 2_000_000
    , "--fee", "3000000"
    , "--protocol-params-file", work </> "pparams.json"
    , "--out-file", failScriptTxBody
    ]

  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "sign"
    , "--tx-body-file", failScriptTxBody
    , "--signing-key-file", wallet0SKeyFile
    , "--out-file", failScriptTxSigned
    ]

  (exitCode6, stdout6, stderr6) <- execCliAny execConfig
    [ anyEraToString cEra, "transaction", "validate"
    , "--tx-file", failScriptTxSigned
    ]

  H.note_ $ "Exit code: " <> show exitCode6
  H.note_ $ "Stdout: " <> stdout6
  H.note_ $ "Stderr: " <> stderr6

  exitCode6 H.=== ExitFailure 1
  H.diffVsGoldenFile
    stdout6
    "test/cardano-testnet-test/files/golden/tx_validate/failing_plutus.out"

  -- ====================================================================
  -- Test 7: Multiple errors (fee too low + value not conserved)
  -- Deterministic UTxO has 10,000,000 lovelace.
  -- output = 999,999,000,000, fee = 100
  -- consumed = 10,000,000, produced = 999,999,000,100
  -- min_fee = 198, fee = 100
  -- ====================================================================
  H.note_ "=== Test 7: Multiple errors ==="

  -- Use a different deterministic UTxO
  testTxin7 <- fmap fst . H.nothingFailM $
    findLargestUtxoWithAddress epochStateView sbe $ Text.pack testAddr
  let multiErrorTxBody = work </> "multi-error-tx.body"
      multiErrorTxSigned = work </> "multi-error-tx.signed"

  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "build-raw"
    , "--tx-in", Text.unpack $ renderTxIn testTxin7
    , "--tx-out", testAddr <> "+999999000000"
    , "--fee", "100"
    , "--out-file", multiErrorTxBody
    ]

  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "sign"
    , "--tx-body-file", multiErrorTxBody
    , "--signing-key-file", testSKeyFile
    , "--out-file", multiErrorTxSigned
    ]

  (exitCode7, stdout7, stderr7) <- execCliAny execConfig
    [ anyEraToString cEra, "transaction", "validate"
    , "--tx-file", multiErrorTxSigned
    ]

  H.note_ $ "Exit code: " <> show exitCode7
  H.note_ $ "Stdout: " <> stdout7
  H.note_ $ "Stderr: " <> stderr7

  exitCode7 H.=== ExitFailure 1
  H.diffVsGoldenFile
    stdout7
    "test/cardano-testnet-test/files/golden/tx_validate/multi_error.out"

  H.success
