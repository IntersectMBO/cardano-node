{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Cli.Transaction.ValidateErrors
  ( hprop_transaction_validate_all_errors
  ) where

import           Cardano.Api

import           Cardano.Testnet

import           Prelude

import           Control.Monad (void)
import           Data.Default.Class
import           Data.List (intercalate, isInfixOf)
import qualified Cardano.Api.UTxO as Utxo
import qualified Data.Text as Text
import           GHC.IO.Exception (ExitCode (..))
import           System.FilePath ((</>))

import           Testnet.Components.Configuration
import           Testnet.Components.Query (findLargestUtxoForPaymentKey,
                   findLargestUtxoWithAddress, findUtxosWithAddress,
                   getEpochStateView, retryUntilJustM,
                   TestnetWaitPeriod (..), waitForBlocks)
import           Testnet.Defaults (plutusV3Script, plutusV3SupplementalDatumScript)
import           Testnet.Process.Run (execCli', execCliAny, mkExecConfig)
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Types

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- Same deterministic signing key as in the Validate test.
-- Key hash: fe8c2213ed33ae44e3629706a3c7d34f0dce29753646602177756a5f
testSigningKey :: String
testSigningKey =
  "{\"type\":\"PaymentSigningKeyShelley_ed25519\",\"description\":\"Payment Signing Key\",\"cborHex\":\"5820aabbccddee0011223344556677889900aabbccddee00112233445566778899aa\"}"

-- | Comprehensive integration test for @cardano-cli latest transaction validate@.
--
-- Exercises every category of validation error:
--
-- Phase 1 (ledger rules):
--   ConwayMempoolFailure, OutputTooSmallUTxO, FeeTooSmallUTxO, ValueNotConservedUTxO,
--   OutsideValidityIntervalUTxO, MissingVKeyWitnessesUTXOW, MaxTxSizeUTxO,
--   ExUnitsTooBigUTxO, TooManyCollateralInputs, NoCollateralInputs,
--   InsufficientCollateral, IncorrectTotalCollateralField,
--   MissingScriptWitnessesUTXOW
--
-- Phase 2 (Plutus script evaluation):
--   Script evaluation failure (CekError)
--
-- Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/transaction validate all errors/"'@
hprop_transaction_validate_all_errors :: Property
hprop_transaction_validate_all_errors = integrationRetryWorkspace 2 "validate-errors" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
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

  -- ====================================================================
  -- Setup: deterministic key + fund address with 6 UTxOs
  -- ====================================================================
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

  let fundingAmount = 10_000_000 :: Int
      wallet0SKeyFile = signingKeyFp $ paymentKeyInfoPair wallet0
      wallet0Addr = Text.unpack $ paymentKeyInfoAddr wallet0
      wallet1SKeyFile = signingKeyFp $ paymentKeyInfoPair wallet1

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

  _ <- retryUntilJustM epochStateView (WaitForBlocks 3) $
    findLargestUtxoWithAddress epochStateView sbe $ Text.pack testAddr

  testUtxosList <- Utxo.toList <$> findUtxosWithAddress epochStateView sbe (Text.pack testAddr)
  let testTxIns = map fst testUtxosList
  H.note_ $ "Found " <> show (length testTxIns) <> " UTxOs at test address"
  H.assert $ length testTxIns >= 6

  -- ====================================================================
  -- Setup: always-succeeds Plutus V3 script address + fund it
  -- ====================================================================
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

  txinForScript <- findLargestUtxoForPaymentKey epochStateView sbe wallet0
  let sendToScriptTxBody = work </> "send-to-script-tx.body"
      sendToScriptTxSigned = work </> "send-to-script-tx.signed"

  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "build"
    , "--change-address", wallet0Addr
    , "--tx-in", Text.unpack $ renderTxIn txinForScript
    , "--tx-out", alwaysSucceedsAddr <> "+5000000"
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

  scriptTxIn <- fmap fst . retryUntilJustM epochStateView (WaitForBlocks 3) $
    findLargestUtxoWithAddress epochStateView sbe $ Text.pack alwaysSucceedsAddr

  -- ====================================================================
  -- Setup: supplemental-datum Plutus V3 script address + fund it
  -- ====================================================================
  let supplementalDatumScript = work </> "supplemental-datum.plutusV3"
  H.writeFile supplementalDatumScript $ Text.unpack plutusV3SupplementalDatumScript

  supplementalDatumScriptAddr <- filter (/= '\n') <$>
    execCli' execConfig
      [ "latest", "address", "build"
      , "--payment-script-file", supplementalDatumScript
      ]

  txinForScript2 <- findLargestUtxoForPaymentKey epochStateView sbe wallet1
  let sendToScript2TxBody = work </> "send-to-script-2-tx.body"
      sendToScript2TxSigned = work </> "send-to-script-2-tx.signed"

  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet1
    , "--tx-in", Text.unpack $ renderTxIn txinForScript2
    , "--tx-out", supplementalDatumScriptAddr <> "+5000000"
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

  supplementalDatumTxIn <- fmap fst . retryUntilJustM epochStateView (WaitForBlocks 3) $
    findLargestUtxoWithAddress epochStateView sbe $ Text.pack supplementalDatumScriptAddr

  -- Fetch protocol parameters for build-raw with scripts
  void $ execCli' execConfig
    [ anyEraToString cEra, "query", "protocol-parameters"
    , "--out-file", work </> "pparams.json"
    ]

  -- Helper: assert that stdout contains an expected substring
  let assertContains :: String -> String -> H.Integration ()
      assertContains output expected = do
        H.note_ $ "  Checking for: " <> expected
        H.assert $ expected `isInfixOf` output

  -- Helper: sign tx body and run validate, returning (exitCode, stdout, stderr)
  let signAndValidate skey txBody txSigned = do
        void $ execCli' execConfig
          [ anyEraToString cEra, "transaction", "sign"
          , "--tx-body-file", txBody
          , "--signing-key-file", skey
          , "--out-file", txSigned
          ]
        execCliAny execConfig
          [ anyEraToString cEra, "transaction", "validate"
          , "--tx-file", txSigned
          ]

  -- ====================================================================
  -- Test 1: ConwayMempoolFailure (non-existent input)
  -- The all-zeros TxIn is caught by the mempool shortcircuit check
  -- before the UTxO rules can fire BadInputsUTxO.
  -- ====================================================================
  H.note_ "=== Test 1: ConwayMempoolFailure (non-existent input) ==="

  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "build-raw"
    , "--tx-in", "0000000000000000000000000000000000000000000000000000000000000000#0"
    , "--tx-out", testAddr <> "+9800000"
    , "--fee", "200000"
    , "--out-file", work </> "bad-input-tx.body"
    ]

  (ec1, out1, _stderr1) <- signAndValidate testSKeyFile
    (work </> "bad-input-tx.body") (work </> "bad-input-tx.signed")

  H.note_ $ "Output:\n" <> out1
  ec1 H.=== ExitFailure 1
  assertContains out1 "Phase 1: FAILED"
  assertContains out1 "ConwayMempoolFailure:"

  -- ====================================================================
  -- Test 2: Mega combo — 5 Phase 1 errors in one transaction
  --   OutputTooSmallUTxO + FeeTooSmallUTxO + ValueNotConservedUTxO
  --   + OutsideValidityIntervalUTxO + MissingVKeyWitnessesUTXOW
  --
  -- Uses a real input (10M lovelace):
  --   output₁ = 1 lovelace (too small)
  --   output₂ = 999,999,000,000 lovelace (blows up value conservation)
  --   fee = 100 (too small)
  --   --invalid-hereafter 1 (expired)
  --   signed with wallet1's key (wrong key for testAddr)
  -- ====================================================================
  H.note_ "=== Test 2: Mega combo (5 Phase 1 errors) ==="

  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "build-raw"
    , "--tx-in", Text.unpack $ renderTxIn (head testTxIns)
    , "--tx-out", testAddr <> "+1"
    , "--tx-out", testAddr <> "+999999000000"
    , "--fee", "100"
    , "--invalid-hereafter", "1"
    , "--out-file", work </> "mega-combo-tx.body"
    ]

  (ec2, out2, _stderr2) <- signAndValidate wallet1SKeyFile
    (work </> "mega-combo-tx.body") (work </> "mega-combo-tx.signed")

  H.note_ $ "Output:\n" <> out2
  ec2 H.=== ExitFailure 1
  assertContains out2 "Phase 1: FAILED"
  -- Values are deterministic: input = 10M, outputs = 1 + 999,999,000,000, fee = 100
  -- Missing key hash = fe8c22... (the deterministic test key)
  -- Slot 1 is the expired validity upper bound (current slot is dynamic)
  assertContains out2 "BabbageOutputTooSmallUTxO:"
  assertContains out2 "FeeTooSmallUTxO: minimum fee is"
  assertContains out2 "ValueNotConservedUTxO: supplied 10000000 lovelace, expected = 999999000101 lovelace"
  assertContains out2 "OutsideValidityIntervalUTxO:"
  assertContains out2 "MissingVKeyWitnessesUTXOW: fe8c2213ed33ae44e3629706a3c7d34f0dce29753646602177756a5f"

  -- ====================================================================
  -- Test 3: MaxTxSizeUTxO
  -- Attach enormous metadata (300 × 64-byte strings ≈ 21 KB > 16 KB limit).
  -- ====================================================================
  H.note_ "=== Test 3: MaxTxSizeUTxO ==="

  let metadataEntry i = "  \"" <> show @Int i <> "\": \"" <> replicate 64 'A' <> "\""
      largeMetadata = "{\n" <> intercalate ",\n" (map metadataEntry [0..299]) <> "\n}"
  H.writeFile (work </> "large-metadata.json") largeMetadata

  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "build-raw"
    , "--tx-in", Text.unpack $ renderTxIn (head testTxIns)
    , "--tx-out", testAddr <> "+9800000"
    , "--fee", "200000"
    , "--json-metadata-no-schema"
    , "--metadata-json-file", work </> "large-metadata.json"
    , "--out-file", work </> "max-tx-size-tx.body"
    ]

  (ec3, out3, _stderr3) <- signAndValidate testSKeyFile
    (work </> "max-tx-size-tx.body") (work </> "max-tx-size-tx.signed")

  H.note_ $ "Output:\n" <> out3
  ec3 H.=== ExitFailure 1
  assertContains out3 "Phase 1: FAILED"
  -- 300 entries × ~68 CBOR bytes = ~20 KB > 16384 byte limit
  assertContains out3 "MaxTxSizeUTxO: supplied 20657 bytes, expected ≤ 16384 bytes"
  H.diffVsGoldenFile out3
    "test/cardano-testnet-test/files/golden/tx_validate/errors_max_tx_size.out"

  -- ====================================================================
  -- Test 4: ExUnitsTooBigUTxO + TooManyCollateralInputs
  -- Script input with execution units far above max (140M mem, 10B steps).
  -- 4 collateral inputs (max allowed is 3).
  -- ====================================================================
  H.note_ "=== Test 4: ExUnitsTooBigUTxO + TooManyCollateralInputs ==="

  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "build-raw"
    , "--tx-in", Text.unpack $ renderTxIn scriptTxIn
    , "--tx-in-script-file", alwaysSucceedsScript
    , "--tx-in-datum-value", "0"
    , "--tx-in-redeemer-value", "0"
    , "--tx-in-execution-units", "(999999999999, 999999999999)"
    , "--tx-in", Text.unpack $ renderTxIn (head testTxIns)
    , "--tx-in-collateral", Text.unpack $ renderTxIn (testTxIns !! 1)
    , "--tx-in-collateral", Text.unpack $ renderTxIn (testTxIns !! 2)
    , "--tx-in-collateral", Text.unpack $ renderTxIn (testTxIns !! 3)
    , "--tx-in-collateral", Text.unpack $ renderTxIn (testTxIns !! 4)
    , "--tx-out", testAddr <> "+12000000"
    , "--fee", "3000000"
    , "--protocol-params-file", work </> "pparams.json"
    , "--out-file", work </> "exunits-collateral-tx.body"
    ]

  (ec4, out4, _stderr4) <- signAndValidate testSKeyFile
    (work </> "exunits-collateral-tx.body") (work </> "exunits-collateral-tx.signed")

  H.note_ $ "Output:\n" <> out4
  ec4 H.=== ExitFailure 1
  assertContains out4 "Phase 1: FAILED"
  -- ExUnits (999999999999, 999999999999) far exceed max (140000000, 10000000000)
  assertContains out4 "ExUnitsTooBigUTxO:"
  -- 4 collateral inputs > maxCollateralInputs (3)
  assertContains out4 "TooManyCollateralInputs: supplied 4, expected ≤ 3"

  -- ====================================================================
  -- Test 5: NoCollateralInputs
  -- Script transaction with no --tx-in-collateral at all.
  -- ====================================================================
  H.note_ "=== Test 5: NoCollateralInputs ==="

  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "build-raw"
    , "--tx-in", Text.unpack $ renderTxIn scriptTxIn
    , "--tx-in-script-file", alwaysSucceedsScript
    , "--tx-in-datum-value", "0"
    , "--tx-in-redeemer-value", "0"
    , "--tx-in-execution-units", "(1000000, 2000000)"
    , "--tx-out", testAddr <> "+2000000"
    , "--fee", "3000000"
    , "--protocol-params-file", work </> "pparams.json"
    , "--out-file", work </> "no-collateral-tx.body"
    ]

  (ec5, out5, _stderr5) <- signAndValidate testSKeyFile
    (work </> "no-collateral-tx.body") (work </> "no-collateral-tx.signed")

  H.note_ $ "Output:\n" <> out5
  ec5 H.=== ExitFailure 1
  assertContains out5 "Phase 1: FAILED"
  assertContains out5 "NoCollateralInputs"
  -- 0 collateral, required = fee 3M × 150% = 4.5M
  assertContains out5 "InsufficientCollateral: actual collateral is 0 lovelace, required collateral is 4500000 lovelace"
  H.diffVsGoldenFile out5
    "test/cardano-testnet-test/files/golden/tx_validate/errors_no_collateral.out"

  -- ====================================================================
  -- Test 6: InsufficientCollateral
  -- Script tx with fee = 10M → required collateral = 15M (150%).
  -- Collateral UTxO has only 10M → insufficient.
  -- Uses a second regular input (10M) so that value is conserved:
  --   consumed = 5M (script) + 10M (regular) = 15M
  --   produced = 5M (output) + 10M (fee) = 15M
  -- ====================================================================
  H.note_ "=== Test 6: InsufficientCollateral ==="

  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "build-raw"
    , "--tx-in", Text.unpack $ renderTxIn scriptTxIn
    , "--tx-in-script-file", alwaysSucceedsScript
    , "--tx-in-datum-value", "0"
    , "--tx-in-redeemer-value", "0"
    , "--tx-in-execution-units", "(1000000, 2000000)"
    , "--tx-in", Text.unpack $ renderTxIn (head testTxIns)
    , "--tx-in-collateral", Text.unpack $ renderTxIn (testTxIns !! 1)
    , "--tx-out", testAddr <> "+5000000"
    , "--fee", "10000000"
    , "--protocol-params-file", work </> "pparams.json"
    , "--out-file", work </> "insufficient-collateral-tx.body"
    ]

  (ec6, out6, _stderr6) <- signAndValidate testSKeyFile
    (work </> "insufficient-collateral-tx.body") (work </> "insufficient-collateral-tx.signed")

  H.note_ $ "Output:\n" <> out6
  ec6 H.=== ExitFailure 1
  assertContains out6 "Phase 1: FAILED"
  -- 10M collateral < 10M fee × 150% = 15M required
  assertContains out6 "InsufficientCollateral: actual collateral is 10000000 lovelace, required collateral is 15000000 lovelace"

  -- ====================================================================
  -- Test 7: IncorrectTotalCollateralField
  -- Declares --tx-total-collateral 1 but the actual collateral is 10M.
  -- Value-balanced, fee reasonable, so only the total-collateral mismatch fires.
  -- ====================================================================
  H.note_ "=== Test 7: IncorrectTotalCollateralField ==="

  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "build-raw"
    , "--tx-in", Text.unpack $ renderTxIn scriptTxIn
    , "--tx-in-script-file", alwaysSucceedsScript
    , "--tx-in-datum-value", "0"
    , "--tx-in-redeemer-value", "0"
    , "--tx-in-execution-units", "(1000000, 2000000)"
    , "--tx-in", Text.unpack $ renderTxIn (head testTxIns)
    , "--tx-in-collateral", Text.unpack $ renderTxIn (testTxIns !! 1)
    , "--tx-total-collateral", "1"
    , "--tx-out", testAddr <> "+12000000"
    , "--fee", "3000000"
    , "--protocol-params-file", work </> "pparams.json"
    , "--out-file", work </> "incorrect-total-collateral-tx.body"
    ]

  (ec7, out7, _stderr7) <- signAndValidate testSKeyFile
    (work </> "incorrect-total-collateral-tx.body") (work </> "incorrect-total-collateral-tx.signed")

  H.note_ $ "Output:\n" <> out7
  ec7 H.=== ExitFailure 1
  assertContains out7 "Phase 1: FAILED"
  -- Declared --tx-total-collateral 1, but actual sum of collateral inputs is 10M
  assertContains out7 "IncorrectTotalCollateralField: declared total collateral is 1 lovelace, actual total collateral is 10000000 lovelace"

  -- ====================================================================
  -- Test 8: MissingScriptWitnessesUTXOW
  -- Spend a UTxO at the script address without providing the script.
  -- ====================================================================
  H.note_ "=== Test 8: MissingScriptWitnessesUTXOW ==="

  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "build-raw"
    , "--tx-in", Text.unpack $ renderTxIn scriptTxIn
    , "--tx-out", testAddr <> "+2000000"
    , "--fee", "3000000"
    , "--out-file", work </> "missing-script-witness-tx.body"
    ]

  (ec8, out8, _stderr8) <- signAndValidate testSKeyFile
    (work </> "missing-script-witness-tx.body") (work </> "missing-script-witness-tx.signed")

  H.note_ $ "Output:\n" <> out8
  ec8 H.=== ExitFailure 1
  assertContains out8 "Phase 1: FAILED"
  -- The always-succeeds script hash
  assertContains out8 "MissingScriptWitnessesUTXOW: 186e32faa80a26810392fda6d559c7ed4721a65ce1c9d4ef3e1c87b4"
  H.diffVsGoldenFile out8
    "test/cardano-testnet-test/files/golden/tx_validate/errors_missing_script_witness.out"

  -- ====================================================================
  -- Test 9: Phase 2 script evaluation failure
  -- The supplemental-datum script requires datum with hash of integer 1
  -- in the transaction.  We omit it → script fails at evaluation.
  -- ====================================================================
  H.note_ "=== Test 9: Phase 2 script failure ==="

  txinCollateral9 <- findLargestUtxoForPaymentKey epochStateView sbe wallet0

  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "build-raw"
    , "--tx-in-collateral", Text.unpack $ renderTxIn txinCollateral9
    , "--tx-in", Text.unpack $ renderTxIn supplementalDatumTxIn
    , "--tx-in-script-file", supplementalDatumScript
    , "--tx-in-datum-value", "0"
    , "--tx-in-redeemer-value", "0"
    , "--tx-in-execution-units", "(5000000000, 10000000)"
    , "--tx-out", wallet0Addr <> "+2000000"
    , "--fee", "3000000"
    , "--protocol-params-file", work </> "pparams.json"
    , "--out-file", work </> "phase2-fail-tx.body"
    ]

  (ec9, out9, _stderr9) <- signAndValidate wallet0SKeyFile
    (work </> "phase2-fail-tx.body") (work </> "phase2-fail-tx.signed")

  H.note_ $ "Output:\n" <> out9
  ec9 H.=== ExitFailure 1
  assertContains out9 "Phase 2: FAILED"
  H.diffVsGoldenFile out9
    "test/cardano-testnet-test/files/golden/tx_validate/errors_phase2_failure.out"

  H.success
