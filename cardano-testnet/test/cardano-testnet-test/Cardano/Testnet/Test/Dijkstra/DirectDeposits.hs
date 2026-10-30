{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Testnet.Test.Dijkstra.DirectDeposits
  ( DepositBuilder (..)
  , hprop_direct_deposits_top_level
  , hprop_direct_deposits_nested
  , hprop_direct_deposits_withdraw
  , hprop_direct_deposits_rejected
  ) where

import           Cardano.Api hiding (label)
import qualified Cardano.Api.Ledger as L

import qualified Cardano.Ledger.Address as L
import qualified Cardano.Ledger.Dijkstra.TxBody as L
import           Cardano.Testnet.Test.Dijkstra.Common
import           Cardano.Testnet.Test.Dijkstra.NestedTransaction (UtxoEntry (..))

import           Control.Monad (forM_, void, when, (>=>))
import           Data.Foldable (toList)
import           Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           Lens.Micro ((^.))
import           System.Exit (ExitCode (..))
import           System.FilePath ((</>))

import           Testnet.Process.Run (execCliAny)

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

data DepositBuilder = Raw | Online | Estimate deriving Show

type Deposits = [(String, Integer)]

-- Each builder gets its own testnet so a failure cannot hide another builder.
-- Undelegated recipients avoid epoch rewards changing the expected balances.
hprop_direct_deposits_top_level :: DepositBuilder -> Property
hprop_direct_deposits_top_level builder = withDijkstra ("dijkstra-deposits-" <> show builder) False $ \ctx -> do
  (keyAccount, scriptAccount) <- registerRecipients ctx
  forM_ [1, 2 :: Integer] $ \roundNumber -> do
    let deposits = [(keyAccount, 2_000_000), (scriptAccount, 3_000_000)]
    input <- largest ctx (payer ctx)
    body <- buildDeposit ctx builder ("deposit-" <> show roundNumber) input deposits
    signed <- signBody ctx body [skey $ payer ctx]
    assertDeposits signed False deposits
    txid <- submitSigned ctx signed
    assertConservation ctx signed txid input 5_000_000
    forM_ deposits $ \(account, amount) -> rewardBalance ctx account >>= (H.=== roundNumber * amount)

hprop_direct_deposits_nested :: Property
hprop_direct_deposits_nested = withDijkstra "dijkstra-deposits-nested" False $ \ctx -> do
  (keyAccount, scriptAccount) <- registerRecipients ctx
  forM_ [1, 2 :: Integer] $ \roundNumber -> do
    let deposits = [(keyAccount, 2_000_000), (scriptAccount, 3_000_000)]
        label = "nested-" <> show roundNumber
    ownerInput <- largest ctx (owner ctx)
    args <- keyInput ctx
    sub <- buildSignedSub ctx label (args ++ depositArgs deposits) [skey $ owner ctx]
    payerInput <- largest ctx (payer ctx)
    body <- buildOuter ctx label sub payerInput (-5_000_000) []
    signed <- signBody ctx body [skey $ payer ctx, skey $ collateralWallet ctx]
    assertDeposits signed True deposits
    txid <- submitSigned ctx signed
    assertConservation ctx signed txid payerInput 5_000_000
    assertSpent ctx (ueTxIn ownerInput)
    -- The nested input is returned unchanged; only the outer payer funds deposits.
    ownerId <- subId ctx label
    ownerOutput <- queryJson ctx ["utxo", "--tx-in", ownerId <> "#0", "--output-json"] >>= field (ownerId <> "#0")
    field "address" ownerOutput >>= (H.=== address (owner ctx))
    (field "value" ownerOutput >>= field "lovelace") >>= (H.=== ueLovelace ownerInput)
    forM_ deposits $ \(account, amount) -> rewardBalance ctx account >>= (H.=== roundNumber * amount)
    assertReference ctx (collateral ctx)

hprop_direct_deposits_withdraw :: Bool -> Property
hprop_direct_deposits_withdraw useReference = withDijkstra ("dijkstra-deposits-withdraw-" <> show useReference) False $ \ctx -> do
  let source = if useReference then ViaReference else ViaFile
      script = v4Script ctx
      amount = 7_000_000
  ref <- referenceOutput ctx "withdraw-reference" script
  account <- registerStake ctx source "register" script ref
  input <- largest ctx (payer ctx)
  body <- buildDeposit ctx Raw "fund-account" input [(account, amount)]
  signed <- signBody ctx body [skey $ payer ctx]
  assertDeposits signed False [(account, amount)]
  txid <- submitSigned ctx signed
  assertConservation ctx signed txid input amount
  rewardBalance ctx account >>= (H.=== amount)
  args <- keyInput ctx
  -- submitSub checks that the batcher's confirmed change includes the withdrawal.
  void $ submitSub ctx "withdraw"
    (args ++ ["--withdrawal", account <> "+" <> show amount] ++ v4Witness source "withdrawal" script ref)
    [skey $ owner ctx] amount
  rewardBalance ctx account >>= (H.=== 0)
  assertReference ctx ref
  assertReference ctx (collateral ctx)
  when useReference $ assertReferenceOnly ctx "withdraw"

hprop_direct_deposits_rejected :: Bool -> Property
hprop_direct_deposits_rejected nested = withDijkstra ("dijkstra-deposits-rejected-" <> show nested) False $ \ctx -> do
  registered <- registerKeyAccount ctx "registered"
  (missing, _, _) <- newKeyAccount ctx "missing"
  StakeAddress _ credential <- H.evalMaybe $ deserialiseAddress AsStakeAddress (Text.pack registered)
  let wrongNetwork = Text.unpack $ serialiseAddress $ StakeAddress L.Mainnet credential
      prefix = if nested then "Sub" else ""
  forM_ [("missing", missing, "DirectDepositAccountsMissing"),
         ("network", wrongNetwork, "DirectDepositAddressesWithWrongNetwork")] $ \(label, target, reason) -> do
    input <- largest ctx (payer ctx)
    ownerInput <- largest ctx (owner ctx)
    -- Include a valid recipient too: rejection must not partially credit it.
    let deposits = [(registered, 2_000_000), (target, 3_000_000)]
    body <- if nested then do
      args <- keyInput ctx
      sub <- buildSignedSub ctx label (args ++ depositArgs deposits) [skey $ owner ctx]
      buildOuter ctx label sub input (-5_000_000) []
      else buildDeposit ctx Raw label input deposits
    signed <- signBody ctx body [skey $ payer ctx, skey $ collateralWallet ctx]
    assertDeposits signed nested deposits
    let protected = [ueTxIn input, ueTxIn ownerInput, collateral ctx]
        snapshot txin = queryJson ctx ["utxo", "--tx-in", txin, "--output-json"]
    before <- mapM snapshot protected
    balance <- rewardBalance ctx registered
    (code, _, stderr) <- execCliAny (exec ctx) ["dijkstra", "transaction", "submit", "--tx-file", signed]
    H.note_ stderr
    H.assert $ case code of ExitFailure _ -> True; ExitSuccess -> False
    H.assert $ (prefix <> reason) `isInfixOf` stderr
    mapM snapshot protected >>= (H.=== before)
    rewardBalance ctx registered >>= (H.=== balance)
    stakeInfo ctx missing >>= (H.=== [])
  -- Reuse the same funds successfully to distinguish validation from node failure.
  input <- largest ctx (payer ctx)
  let deposits = [(registered, 2_000_000)]
  body <- if nested then do
    args <- keyInput ctx
    sub <- buildSignedSub ctx "recovery" (args ++ depositArgs deposits) [skey $ owner ctx]
    buildOuter ctx "recovery" sub input (-2_000_000) []
    else buildDeposit ctx Raw "recovery" input deposits
  signed <- signBody ctx body [skey $ payer ctx, skey $ collateralWallet ctx]
  assertDeposits signed nested deposits
  txid <- submitSigned ctx signed
  assertConservation ctx signed txid input 2_000_000
  rewardBalance ctx registered >>= (H.=== 2_000_000)

depositArgs :: Deposits -> [String]
depositArgs = concatMap $ \(account, amount) -> ["--direct-deposit", account <> "+" <> show amount]

buildDeposit :: DijkstraTest -> DepositBuilder -> String -> UtxoEntry -> Deposits -> H.Integration FilePath
buildDeposit ctx builder label input deposits = do
  let body = directory ctx </> label <> ".body"
      change = ueLovelace input - fee - sum (map snd deposits)
      (command, options) = case builder of
        Raw -> ("build-raw", ["--tx-out", address (payer ctx) <> "+" <> show change, "--fee", show fee])
        Online -> ("build", ["--change-address", address (payer ctx), "--witness-override", "1"])
        Estimate -> ("build-estimate",
          ["--change-address", address (payer ctx), "--total-utxo-value", show (ueLovelace input)
          , "--shelley-key-witnesses", "1", "--protocol-params-file", protocolParams ctx])
  H.assert $ change > 0
  void $ cli ctx $ ["dijkstra", "transaction", command, "--tx-in", ueTxIn input]
    ++ options ++ depositArgs deposits ++ ["--out-file", body]
  pure body

readTransaction :: FilePath -> H.Integration (Tx DijkstraEra)
readTransaction path = H.evalEither =<< H.evalIO (readFileTextEnvelope $ File path)

assertDeposits :: FilePath -> Bool -> Deposits -> H.Integration ()
assertDeposits signed nested deposits = do
  expected <- L.DirectDeposits . Map.fromList <$> mapM (\(account, amount) -> do
    addr <- H.evalMaybe $ deserialiseAddress AsStakeAddress (Text.pack account)
    pure (toShelleyStakeAddr addr, Coin amount)) deposits
  ShelleyTx _ tx <- readTransaction signed
  let body = tx ^. L.bodyTxL
      children = toList $ body ^. L.subTransactionsTxBodyL
  if nested then do
    body ^. L.directDepositsTxBodyL H.=== L.DirectDeposits Map.empty
    length children H.=== 1
    forM_ children $ \child -> child ^. L.bodyTxL . L.directDepositsTxBodyL H.=== expected
    else do
      body ^. L.directDepositsTxBodyL H.=== expected
      length children H.=== 0

-- Check confirmed outputs against the signed body, including the builder's fee.
-- Nested owners balance their own inputs/outputs, so this checks the outer payer.
assertConservation :: DijkstraTest -> FilePath -> String -> UtxoEntry -> Integer -> H.Integration ()
assertConservation ctx signed txid input deposits = do
  ShelleyTx _ tx <- readTransaction signed
  let body = tx ^. L.bodyTxL
      outputs = toList $ body ^. L.outputsTxBodyL
      actualFee = body ^. L.feeTxBodyL
  sum (map (^. L.coinTxOutL) outputs) + actualFee + Coin deposits H.=== Coin (ueLovelace input)
  forM_ (zip [0 :: Int ..] outputs) $ \(index, output) -> do
    let txin = txid <> "#" <> show index
    confirmed <- queryJson ctx ["utxo", "--tx-in", txin, "--output-json"] >>= field txin
    actual <- field "value" confirmed >>= field "lovelace"
    Coin actual H.=== output ^. L.coinTxOutL
    field "address" confirmed >>= (H.=== address (payer ctx))
  assertSpent ctx (ueTxIn input)

registerRecipients :: DijkstraTest -> H.Integration (String, String)
registerRecipients ctx = do
  key <- registerKeyAccount ctx "key-recipient"
  script <- registerStake ctx ViaFile "script-recipient" (v4Script ctx) ""
  forM_ [key, script] $ rewardBalance ctx >=> (H.=== 0)
  pure (key, script)

newKeyAccount :: DijkstraTest -> String -> H.Integration (String, FilePath, FilePath)
newKeyAccount ctx label = do
  let vkey = directory ctx </> label <> ".vkey"
      key = directory ctx </> label <> ".skey"
  void $ cli ctx ["dijkstra", "stake-address", "key-gen", "--verification-key-file", vkey, "--signing-key-file", key]
  account <- trim <$> cli ctx ["dijkstra", "stake-address", "build", "--stake-verification-key-file", vkey]
  pure (account, vkey, key)

registerKeyAccount :: DijkstraTest -> String -> H.Integration String
registerKeyAccount ctx label = do
  (account, vkey, key) <- newKeyAccount ctx label
  deposit <- parameter ctx "stakeAddressDeposit"
  let cert = directory ctx </> label <> ".regcert"
      body = directory ctx </> label <> ".regbody"
  void $ cli ctx ["dijkstra", "stake-address", "registration-certificate", "--stake-verification-key-file", vkey
    , "--key-reg-deposit-amt", show deposit, "--out-file", cert]
  input <- largest ctx (payer ctx)
  void $ cli ctx ["dijkstra", "transaction", "build-raw", "--tx-in", ueTxIn input
    , "--tx-out", address (payer ctx) <> "+" <> show (ueLovelace input - fee - deposit)
    , "--fee", show fee, "--certificate-file", cert, "--out-file", body]
  void $ signSubmit ctx body [skey $ payer ctx, key]
  rewardBalance ctx account >>= (H.=== 0)
  pure account
