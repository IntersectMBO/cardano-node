{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Testnet.Test.Dijkstra.Scripts
  ( hprop_sub_transaction_guards
  , hprop_sub_transaction_plutus_v4
  , hprop_top_level_plutus_v4
  , hprop_top_level_plutus_v4_reference
  ) where

import           Cardano.Api (DijkstraEra, File (..), Tx (..), readFileTextEnvelope)
import qualified Cardano.Api.Ledger as L

import qualified Cardano.Ledger.Dijkstra.TxBody as L
import           Cardano.Testnet.Test.Dijkstra.Common
import           Cardano.Testnet.Test.Dijkstra.NestedTransaction (UtxoEntry (..), queryUtxos)

import           Control.Monad (forM_, void, when)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Lens.Micro ((^.))
import           System.FilePath ((</>))

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- | A native RequireGuard script both locks a UTxO and controls a mint. Its
-- credential is authorized by a separate key, a native script, or a V4 script.
-- Run with: --test-options '-p "/Dijkstra sub-transaction guards/"'
hprop_sub_transaction_guards :: Property
hprop_sub_transaction_guards = withDijkstra "dijkstra-guards" False $ \ctx -> do
  let key = directory ctx </> "guard.skey"
      vkey = directory ctx </> "guard.vkey"
      native = directory ctx </> "signature.json"
  void $ cli ctx ["address", "key-gen", "--signing-key-file", key, "--verification-key-file", vkey]
  keyHash <- trim <$> cli ctx ["address", "key-hash", "--payment-verification-key-file", vkey]
  H.writeFile native $ B.unpack $ A.encode $ A.object ["type" A..= ("sig" :: String), "keyHash" A..= keyHash]
  nativeHash <- scriptHash ctx native
  plutusHash <- scriptHash ctx (v4Script ctx)
  forM_ [ViaReference, ViaFile] $ \source -> do
    nativeRef <- referenceOutput ctx (show source <> "-native-guard-ref") native
    plutusRef <- referenceOutput ctx (show source <> "-plutus-guard-ref") (v4Script ctx)
    let cases =
          [ ("key", A.object ["keyHash" A..= keyHash], ["--guard-key-hash", keyHash], [key])
          , ("native", A.object ["scriptHash" A..= nativeHash], guardArgs source native nativeHash nativeRef, [key])
          , ("plutus", A.object ["scriptHash" A..= plutusHash]
            , guardArgs source (v4Script ctx) plutusHash plutusRef
              ++ ["--guard-redeemer-value", "0", "--guard-execution-units", units], [])
          ]
    forM_ cases $ \(kind, credential, guards, keys) -> do
      let label = show source <> "-" <> kind
          policy = directory ctx </> label <> "-require-guard.json"
      H.note_ $ "Native RequireGuard spending/minting authorized by " <> kind <> ", " <> show source
      H.writeFile policy $ B.unpack $ A.encode $ A.object ["type" A..= ("guard" :: String), "credential" A..= credential]
      mintAndSpend ctx label source False policy guards keys
    assertReference ctx nativeRef
    assertReference ctx plutusRef

-- | V4 Spending and Minting purposes in the same sub-transaction, using direct
-- witnesses and then reference witnesses. Both must execute on the ledger.
-- Run with: --test-options '-p "/Dijkstra sub-transaction Plutus V4/"'
hprop_sub_transaction_plutus_v4 :: Property
hprop_sub_transaction_plutus_v4 = withDijkstra "dijkstra-plutus-v4" False $ \ctx ->
  forM_ [ViaFile, ViaReference] $ \source ->
    mintAndSpend ctx (show source <> "-v4") source True (v4Script ctx) [] []

-- | Establish that V4 Spending and Minting work in a top-level transaction
-- without guards or sub-transactions. Keep the witness variants independent
-- so a failure in one cannot prevent the other from running.
hprop_top_level_plutus_v4 :: Property
hprop_top_level_plutus_v4 = topLevelMintAndSpend ViaFile

hprop_top_level_plutus_v4_reference :: Property
hprop_top_level_plutus_v4_reference = topLevelMintAndSpend ViaReference

topLevelMintAndSpend :: ScriptSource -> Property
topLevelMintAndSpend source = withDijkstra ("dijkstra-top-level-v4-" <> show source) False $ \ctx -> do
  let script = v4Script ctx
      label = "top-level-v4"
      body = directory ctx </> label <> ".body"
  addr <- scriptAddress ctx script
  policy <- scriptHash ctx script
  funding <- fund ctx (label <> "-fund") [(addr, 20_000_000, ["--tx-out-inline-datum-value", "0"])]
  reference <- case source of
    ViaFile -> pure Nothing
    ViaReference -> Just <$> referenceOutput ctx (label <> "-reference") script
  payerInput <- largest ctx (payer ctx)
  let scriptInput = funding <> "#0"
      ref = fromMaybe "" reference
      asset = "1 " <> policy <> ".54" -- T
      change = ueLovelace payerInput - fee
      datumArgs = case source of
        ViaFile -> ["--tx-in-inline-datum-present"]
        ViaReference -> ["--spending-reference-tx-in-inline-datum-present"]
      policyArgs = case source of
        ViaFile -> []
        ViaReference -> ["--policy-id", policy]
  H.assert $ change > 0
  H.note_ $ "Top-level V4 spending and minting without guards: " <> show source
  void $ cli ctx $
    ["dijkstra", "transaction", "build-raw", "--tx-in", scriptInput]
    ++ v4Witness source "tx-in" script ref ++ datumArgs
    ++ ["--tx-in", ueTxIn payerInput, "--tx-in-collateral", collateral ctx, "--mint", asset]
    ++ v4Witness source "mint" script ref ++ policyArgs
    ++ [ "--tx-out", address (owner ctx) <> "+20000000+" <> asset
       , "--tx-out", address (payer ctx) <> "+" <> show change
       , "--fee", show fee, "--protocol-params-file", protocolParams ctx, "--out-file", body
       ]
  signed <- signBody ctx body [skey $ payer ctx, skey $ collateralWallet ctx]
  signedTx <- H.evalEither =<< H.evalIO (readFileTextEnvelope $ File signed)
  case (signedTx :: Tx DijkstraEra) of
    ShelleyTx _ tx -> do
      tx ^. L.bodyTxL . L.guardsTxBodyL H.=== mempty
      tx ^. L.bodyTxL . L.requiredTopLevelGuardsL H.=== mempty
      tx ^. L.bodyTxL . L.subTransactionsTxBodyL H.=== mempty
      Map.size (tx ^. L.witsTxL . L.scriptTxWitsL) H.=== case source of
        ViaFile -> 1
        ViaReference -> 0
  txid <- submitSigned ctx signed
  forM_ [scriptInput, ueTxIn payerInput] $ assertSpent ctx
  outputs <- queryUtxos (exec ctx) policy "54" (address $ owner ctx)
  [output] <- pure $ filter ((== txid <> "#0") . ueTxIn) outputs
  ueTokens output H.=== 1
  ueLovelace output H.=== 20_000_000
  payerOutput <- queryJson ctx ["utxo", "--tx-in", txid <> "#1", "--output-json"] >>= field (txid <> "#1")
  (field "value" payerOutput >>= field "lovelace") >>= (H.=== change)
  assertReference ctx (collateral ctx)
  forM_ reference $ assertReference ctx

mintAndSpend :: DijkstraTest -> String -> ScriptSource -> Bool -> FilePath -> [String] -> [FilePath] -> H.Integration ()
mintAndSpend ctx label source plutus script guards keys = do
  addr <- scriptAddress ctx script
  policy <- scriptHash ctx script
  let datum = if plutus then ["--tx-out-inline-datum-value", "0"] else []
      asset = "1 " <> policy <> ".54" -- T
  funding <- fund ctx (label <> "-fund") [(addr, 20_000_000, datum)]
  ref <- referenceOutput ctx (label <> "-reference") script
  let input = funding <> "#0"
      spending = case (plutus, source) of
        (True, ViaFile) -> v4Witness source "tx-in" script ref ++ ["--tx-in-inline-datum-present"]
        (True, ViaReference) -> v4Witness source "tx-in" script ref ++ ["--spending-reference-tx-in-inline-datum-present"]
        (False, ViaFile) -> ["--tx-in-script-file", script]
        (False, ViaReference) -> ["--simple-script-tx-in-reference", ref]
      minting = case (plutus, source) of
        (True, ViaFile) -> v4Witness source "mint" script ref
        (True, ViaReference) -> v4Witness source "mint" script ref ++ ["--policy-id", policy]
        (False, ViaFile) -> ["--mint-script-file", script]
        (False, ViaReference) -> ["--simple-minting-script-tx-in-reference", ref, "--policy-id", policy]
  before <- queryUtxos (exec ctx) policy "54" (address $ owner ctx)
  -- A validity lower bound, metadata and canonical output get one happy-path
  -- exercise here, without separate permutations for each auxiliary option.
  slot <- queryJson ctx ["tip"] >>= field "slot"
  let metadata = directory ctx </> label <> "-metadata.json"
  H.writeFile metadata "{\"1\":\"Dijkstra sub-transaction smoke test\"}"
  void $ submitSub ctx label
    (["--tx-in", input] ++ spending
      ++ ["--mint", asset] ++ minting ++ guards
      ++ [ "--tx-out", address (owner ctx) <> "+20000000+" <> asset
         , "--invalid-before", show (slot :: Integer)
         , "--metadata-json-file", metadata, "--out-canonical-cbor"
         ]) keys 0
  assertSpent ctx input
  after <- queryUtxos (exec ctx) policy "54" (address $ owner ctx)
  sum (map ueTokens after) H.=== sum (map ueTokens before) + 1
  H.assert $ any (\u -> ueTokens u == 1 && ueLovelace u == 20_000_000) after
  assertReference ctx ref
  when (source == ViaReference) $ assertReferenceOnly ctx label
