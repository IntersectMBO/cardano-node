{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Testnet.Test.Dijkstra.Scripts
  ( hprop_sub_transaction_guards
  , hprop_sub_transaction_plutus_v4
  ) where

import           Cardano.Testnet.Test.Dijkstra.Common
import           Cardano.Testnet.Test.Dijkstra.NestedTransaction (UtxoEntry (..), queryUtxos)

import           Control.Monad (forM_, void)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as B
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
  if source == ViaReference then assertReferenceOnly ctx label else pure ()
