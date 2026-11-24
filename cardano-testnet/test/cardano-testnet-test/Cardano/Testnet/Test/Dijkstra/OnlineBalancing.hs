{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Testnet.Test.Dijkstra.OnlineBalancing
  ( hprop_online_sub_transaction_balances
  , hprop_online_sub_transaction_certificates
  , hprop_online_sub_transaction_script
  ) where

import           Cardano.Api hiding (label)
import qualified Cardano.Api.Experimental as Exp
import qualified Cardano.Api.Ledger as L

import qualified Cardano.Ledger.Dijkstra.TxBody as L
import           Cardano.Testnet.Test.Dijkstra.Common
import           Cardano.Testnet.Test.Dijkstra.NestedTransaction (UtxoEntry (..))

import           Control.Monad (forM_, void, when)
import           Data.Foldable (toList)
import           Lens.Micro ((^.))
import           System.FilePath ((</>))

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- Requires cardano-cli#1507 and its matching API dependencies. Run with:
-- CARDANO_CLI=/path/to/cardano-cli DISABLE_RETRIES=1 cabal test cardano-testnet-test
--   --test-options '-p "/Dijkstra online sub-transaction/"'
-- Only the outer transaction is balanced online; the children are already signed.
hprop_online_sub_transaction_balances :: Property
hprop_online_sub_transaction_balances = withDijkstra "dijkstra-online-balances" False $ \ctx -> do
  -- Exercise both a net surplus and a net deficit across two signed children.
  -- Neither child input is supplied as a top-level --tx-in.
  forM_ [("surplus", 13_000_000, 19_000_000), ("deficit", 26_000_000, 22_000_000)] $ \(label, firstAmount, secondAmount) -> do
    funding <- fund ctx label
      [(address $ owner ctx, 20_000_000, []), (address $ owner ctx, 20_000_000, [])]
    let firstInput = funding <> "#0"
        secondInput = funding <> "#1"
    first <- buildSignedSub ctx (label <> "-first")
      ["--tx-in", firstInput, "--tx-out", address (owner ctx) <> "+" <> show firstAmount
      , "--read-only-tx-in-reference", collateral ctx]
      [skey $ owner ctx]
    second <- buildSignedSub ctx (label <> "-second")
      ["--tx-in", secondInput, "--tx-out", address (owner ctx) <> "+" <> show secondAmount]
      [skey $ owner ctx]
    input <- largest ctx (payer ctx)
    -- Reverse the input order to catch builders that sort signed children.
    void $ buildSubmitBalanced ctx label input [second, first]
      (40_000_000 - firstAmount - secondAmount) []
    mapM_ (assertSpent ctx) [firstInput, secondInput]
    assertReference ctx (collateral ctx)

-- The deposit is paid by the outer input, then returned to the outer change.
-- The stake key is only present in the signed child, never in the outer witnesses.
hprop_online_sub_transaction_certificates :: Property
hprop_online_sub_transaction_certificates = withDijkstra "dijkstra-online-certificates" False $ \ctx -> do
  let vkey = directory ctx </> "stake.vkey"
      key = directory ctx </> "stake.skey"
  void $ cli ctx ["dijkstra", "stake-address", "key-gen", "--verification-key-file", vkey, "--signing-key-file", key]
  account <- trim <$> cli ctx ["dijkstra", "stake-address", "build", "--stake-verification-key-file", vkey]
  stakeInfo ctx account >>= (H.=== [])
  deposit <- parameter ctx "stakeAddressDeposit"
  forM_ [("registration", negate deposit), ("deregistration", deposit)] $ \(label, adjustment) -> do
    let cert = directory ctx </> label <> ".cert"
    void $ cli ctx ["dijkstra", "stake-address", label <> "-certificate", "--stake-verification-key-file", vkey
      , "--key-reg-deposit-amt", show deposit, "--out-file", cert]
    ownerInput <- largest ctx (owner ctx)
    child <- buildSignedSub ctx label
      ["--tx-in", ueTxIn ownerInput, "--tx-out", address (owner ctx) <> "+" <> show (ueLovelace ownerInput)
      , "--certificate-file", cert]
      [skey $ owner ctx, key]
    input <- largest ctx (payer ctx)
    void $ buildSubmitBalanced ctx label input [child] adjustment []
    assertSpent ctx (ueTxIn ownerInput)
    entries <- stakeInfo ctx account
    length entries H.=== if label == "registration" then 1 else 0
    when (label == "registration") $ rewardBalance ctx account >>= (H.=== 0)

-- Online evaluation must query script inputs and reference scripts belonging
-- only to the signed child, and preserve its fixed execution budget and signature.
hprop_online_sub_transaction_script :: Bool -> Property
hprop_online_sub_transaction_script useReference = withDijkstra ("dijkstra-online-script-" <> show useReference) False $ \ctx -> do
  let source = if useReference then ViaReference else ViaFile
      script = v4Script ctx
      label = "script-spend"
      amount = 20_000_000
      datumFlag = if useReference then "--spending-reference-tx-in-inline-datum-present" else "--tx-in-inline-datum-present"
  ref <- referenceOutput ctx "script-reference" script
  addr <- scriptAddress ctx script
  funding <- fund ctx "script-input" [(addr, amount, ["--tx-out-inline-datum-value", "0"])]
  let scriptInput = funding <> "#0"
  child <- buildSignedSub ctx label
    (["--tx-in", scriptInput] ++ v4Witness source "tx-in" script ref
      ++ [datumFlag, "--tx-out", address (owner ctx) <> "+" <> show amount])
    [skey $ owner ctx]
  when useReference $ assertReferenceOnly ctx label
  input <- largest ctx (payer ctx)
  void $ buildSubmitBalanced ctx label input [child] 0 ["--tx-in-collateral", collateral ctx]
  assertSpent ctx scriptInput
  assertReference ctx ref
  assertReference ctx (collateral ctx)

-- adjustment is the signed children's input value minus outputs and deposits,
-- plus refunds. No manually computed fee or change is passed to the CLI.
buildSubmitBalanced
  :: DijkstraTest
  -> String
  -> UtxoEntry
  -> [FilePath]
  -> Integer
  -> [String]
  -> H.Integration String
buildSubmitBalanced ctx label input children adjustment extras = do
  let bodyFile = directory ctx </> label <> ".body"
  void $ cli ctx $
    ["dijkstra", "transaction", "build", "--tx-in", ueTxIn input
    , "--change-address", address (payer ctx), "--out-file", bodyFile]
    ++ concatMap (\child -> ["--signed-sub-tx-file", child]) children ++ extras
  expected <- mapM readSub children
  assertChildren bodyFile expected
  -- Collateral belongs to a separate wallet. The owners' keys and stake key
  -- deliberately do not sign the enclosing transaction.
  let keys = skey (payer ctx) : [skey $ collateralWallet ctx | not $ null extras]
  signed <- signBody ctx bodyFile keys
  assertChildren signed expected
  ShelleyTx _ tx <- readTransaction signed
  let body = tx ^. L.bodyTxL
      outputs = toList $ body ^. L.outputsTxBodyL
      actualFee = body ^. L.feeTxBodyL
  H.assert $ actualFee > 0
  length outputs H.=== 1
  sum (map (^. L.coinTxOutL) outputs) + actualFee H.=== Coin (ueLovelace input + adjustment)
  txid <- submitSigned ctx signed
  assertOutputs ctx txid [(address (payer ctx), ueLovelace input + adjustment - unCoin actualFee)]
  assertSpent ctx (ueTxIn input)
  -- Check the child outputs under each child's own id after confirmation.
  forM_ children $ \childFile -> do
    childId <- trim <$> cli ctx ["dijkstra", "sub-transaction", "txid", "--signed-sub-tx-file", childFile, "--output-text"]
    Exp.SignedSubTx child <- readSub childFile
    let childOutputs = toList $ child ^. L.bodyTxL . L.outputsTxBodyL
    assertOutputs ctx childId [(address (owner ctx), unCoin $ output ^. L.coinTxOutL) | output <- childOutputs]
  pure txid

readSub :: FilePath -> H.Integration Exp.SignedSubTx
readSub file = H.evalEither =<< H.evalIO (readFileTextEnvelope $ File file)

readTransaction :: FilePath -> H.Integration (Tx DijkstraEra)
readTransaction file = H.evalEither =<< H.evalIO (readFileTextEnvelope $ File file)

-- Compare complete CBOR, including witness sets, in the supplied order both
-- before and after signing the outer transaction.
assertChildren :: FilePath -> [Exp.SignedSubTx] -> H.Integration ()
assertChildren file expected = do
  ShelleyTx _ tx <- readTransaction file
  let actual = map Exp.SignedSubTx $ toList $ tx ^. L.bodyTxL . L.subTransactionsTxBodyL
  map serialiseToCBOR actual H.=== map serialiseToCBOR expected

assertOutputs :: DijkstraTest -> String -> [(String, Integer)] -> H.Integration ()
assertOutputs ctx txid outputs = forM_ (zip [0 :: Int ..] outputs) $ \(index, (addr, amount)) -> do
  let txin = txid <> "#" <> show index
  confirmed <- queryJson ctx ["utxo", "--tx-in", txin, "--output-json"] >>= field txin
  field "address" confirmed >>= (H.=== addr)
  (field "value" confirmed >>= field "lovelace") >>= (H.=== amount)
