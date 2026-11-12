{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Testnet.Test.Dijkstra.TopLevelGuards
  ( hprop_top_level_key_guards
  , hprop_top_level_native_guards
  , hprop_top_level_plutus_v4_guards
  , GuardedPurpose (..)
  , hprop_top_level_guard_purpose
  , hprop_top_level_guard_missing_datum
  ) where

import           Cardano.Api hiding (label)
import qualified Cardano.Api.Experimental as Exp
import qualified Cardano.Api.Ledger as L

import qualified Cardano.Ledger.Dijkstra.Scripts as L
import qualified Cardano.Ledger.Dijkstra.TxBody as L
import           Cardano.Ledger.Keys (coerceKeyRole)
import qualified Cardano.Ledger.Plutus.Language as L
import           Cardano.Testnet.Test.Dijkstra.Common
import           Cardano.Testnet.Test.Dijkstra.NestedTransaction (UtxoEntry (..))

import           Control.Monad (forM_, void, when)
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           GHC.Exts (fromList)
import           Lens.Micro ((&), (.~), (^.))
import           System.Exit (ExitCode (..))
import           System.FilePath ((</>))

import           Testnet.Components.Query (TestnetWaitPeriod (..), retryUntilM)
import           Testnet.Process.Run (execCliAny)

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- The requirement belongs to the sub-transaction; its authorization belongs
-- to the enclosing transaction. Keep the guard key separate from both wallets.
data TopGuard = TopGuard
  { credential :: L.Credential L.Guard
  , requirementArgs :: [String]
  , witnessScript :: Maybe (L.Script (ShelleyLedgerEra DijkstraEra))
  , witnessKeys :: [FilePath]
  , isPlutus :: Bool
  , referenceInput :: Maybe String
  }

hprop_top_level_key_guards :: Property
hprop_top_level_key_guards = withDijkstra "dijkstra-top-level-key-guards" False $ \ctx -> do
  (hash, key) <- freshGuardKey ctx
  PaymentKeyHash kh <- H.evalEither $ deserialiseFromRawBytesHex $ BS.pack hash
  runGuardCase ctx "key" True $ TopGuard
    (L.KeyHashObj $ coerceKeyRole kh) ["--top-level-guard-key-hash", hash] Nothing [key] False Nothing

hprop_top_level_native_guards :: Property
hprop_top_level_native_guards = withDijkstra "dijkstra-top-level-native-guards" False $ \ctx -> do
  (hash, key) <- freshGuardKey ctx
  let path = directory ctx </> "signature.json"
  H.writeFile path $ B.unpack $ A.encode $ A.object ["type" A..= ("sig" :: String), "keyHash" A..= hash]
  script <- H.readJsonFileOk path
  scriptCases ctx path (toShelleyScript $ ScriptInEra SimpleScriptInDijkstra $ SimpleScript script) [key] False

hprop_top_level_plutus_v4_guards :: Property
hprop_top_level_plutus_v4_guards = withDijkstra "dijkstra-top-level-plutus-v4-guards" False $ \ctx -> do
  script <- H.evalEither =<< H.evalIO (readFileTextEnvelope $ File $ v4Script ctx)
  scriptCases ctx (v4Script ctx)
    (toShelleyScript $ ScriptInEra PlutusScriptV4InDijkstra $ PlutusScript PlutusScriptV4 script) [] True

data GuardedPurpose = Spending | Minting | Certifying | Rewarding | Proposing | Voting
  deriving Show

-- Each purpose gets its own testnet so a decoder failure cannot hide the other
-- cases. Setup transactions are unguarded; the operation under test is a
-- sub-transaction requiring a V4 guard in the top-level transaction.
hprop_top_level_guard_purpose :: GuardedPurpose -> Property
hprop_top_level_guard_purpose purpose = withDijkstra ("dijkstra-required-guard-" <> show purpose) True $ \ctx -> do
  let script = v4Script ctx
      witness kind = v4Witness ViaFile kind script ""
      submit args adjustment = do
        (signed, payerInput) <- buildGuardedPurpose ctx "guarded" (Just 42) args adjustment
        txid <- submitSigned ctx signed
        assertSpent ctx (ueTxIn payerInput)
        assertOutput ctx (txid <> "#0") (address $ payer ctx) (ueLovelace payerInput - fee + adjustment)
        assertReference ctx (collateral ctx)
        subId ctx "guarded"
      register label = registerStake ctx ViaFile label script ""
      makeProposal addr = do
        deposit <- parameter ctx "govActionDeposit"
        hash <- scriptHash ctx script
        let proposal = directory ctx </> "guarded.proposal"
        void $ cli ctx $ ["dijkstra", "governance", "action", "create-protocol-parameters-update", "--testnet"
          , "--governance-action-deposit", show deposit, "--deposit-return-stake-address", addr
          , "--constitution-script-hash", hash, "--min-fee-linear", "45"]
          ++ anchorArgs ++ ["--out-file", proposal]
        pure (proposal, deposit)
  case purpose of
    Spending -> do
      addr <- scriptAddress ctx script
      funding <- fund ctx "script-input" [(addr, 20_000_000, ["--tx-out-inline-datum-value", "0"])]
      let input = funding <> "#0"
      sid <- submit (["--tx-in", input] ++ witness "tx-in"
        ++ ["--tx-in-inline-datum-present", "--tx-out", address (owner ctx) <> "+20000000"]) 0
      assertSpent ctx input
      assertOutput ctx (sid <> "#0") (address $ owner ctx) 20_000_000
    Minting -> do
      input <- largest ctx (owner ctx)
      policy <- scriptHash ctx script
      let asset = "1 " <> policy <> ".54"
      sid <- submit (["--tx-in", ueTxIn input, "--mint", asset] ++ witness "mint"
        ++ ["--tx-out", address (owner ctx) <> "+" <> show (ueLovelace input) <> "+" <> asset]) 0
      assertSpent ctx (ueTxIn input)
      output <- queryJson ctx ["utxo", "--tx-in", sid <> "#0", "--output-json"] >>= field (sid <> "#0")
      tokens <- field "value" output >>= field policy >>= field "54"
      (tokens :: Integer) H.=== 1
    Certifying -> do
      deposit <- parameter ctx "stakeAddressDeposit"
      let cert = directory ctx </> "guarded.regcert"
      void $ cli ctx ["dijkstra", "stake-address", "registration-certificate", "--stake-script-file", script
        , "--key-reg-deposit-amt", show deposit, "--out-file", cert]
      input <- keyInput ctx
      void $ submit (input ++ ["--certificate-file", cert] ++ witness "certificate") (-deposit)
      addr <- stakeAddress ctx script
      entries <- stakeInfo ctx addr
      length entries H.=== 1
    Rewarding -> do
      addr <- register "withdrawal-account"
      deposit <- parameter ctx "govActionDeposit"
      lifetime <- parameter ctx "govActionLifetime"
      let proposal = directory ctx </> "reward-source.info"
      void $ cli ctx $ ["dijkstra", "governance", "action", "create-info", "--testnet"
        , "--governance-action-deposit", show deposit, "--deposit-return-stake-address", addr]
        ++ anchorArgs ++ ["--out-file", proposal]
      setupInput <- keyInput ctx
      void $ submitSub ctx "reward-source" (setupInput ++ ["--proposal-file", proposal]) [skey $ owner ctx] (-deposit)
      reward <- retryUntilM (epochView ctx) (WaitForEpochs $ L.EpochInterval $ fromIntegral $ lifetime + 4)
        (rewardBalance ctx addr) (> 0)
      reward H.=== deposit
      input <- keyInput ctx
      void $ submit (input ++ ["--withdrawal", addr <> "+" <> show reward] ++ witness "withdrawal") reward
      rewardBalance ctx addr >>= (H.=== 0)
    Proposing -> do
      addr <- register "proposal-return"
      (proposal, deposit) <- makeProposal addr
      input <- keyInput ctx
      sid <- submit (input ++ ["--proposal-file", proposal] ++ witness "proposal") (-deposit)
      void $ proposalState ctx sid
    Voting -> do
      addr <- register "proposal-return"
      hash <- scriptHash ctx script
      deposit <- parameter ctx "dRepDeposit"
      let cert = directory ctx </> "voter.regcert"
      void $ cli ctx ["dijkstra", "governance", "drep", "registration-certificate", "--drep-script-hash", hash
        , "--key-reg-deposit-amt", show deposit, "--out-file", cert]
      voterInput <- keyInput ctx
      void $ submitSub ctx "register-voter" (voterInput ++ ["--certificate-file", cert] ++ witness "certificate")
        [skey $ owner ctx] (-deposit)
      (proposal, actionDeposit) <- makeProposal addr
      proposalInput <- keyInput ctx
      void $ submitSub ctx "vote-target" (proposalInput ++ ["--proposal-file", proposal] ++ witness "proposal")
        [skey $ owner ctx] (-actionDeposit)
      actionId <- subId ctx "vote-target"
      state <- proposalState ctx actionId
      index <- field "actionId" state >>= field "govActionIx"
      let vote = directory ctx </> "guarded.vote"
      void $ cli ctx ["dijkstra", "governance", "vote", "create", "--yes", "--governance-action-tx-id", actionId
        , "--governance-action-index", show (index :: Integer), "--drep-script-hash", hash, "--out-file", vote]
      input <- keyInput ctx
      void $ submit (input ++ ["--vote-file", vote] ++ witness "vote") 0
      recorded <- proposalState ctx actionId >>= field "dRepVotes" >>= field ("scriptHash-" <> hash)
      (recorded :: String) H.=== "VoteYes"

-- A missing datum is deliberately invalid for a required Plutus guard. Check
-- that submission reaches ledger validation, rather than calling it a success.
hprop_top_level_guard_missing_datum :: Property
hprop_top_level_guard_missing_datum = withDijkstra "dijkstra-required-guard-missing-datum" False $ \ctx -> do
  ownerInput <- largest ctx (owner ctx)
  input <- keyInput ctx
  (signed, payerInput) <- buildGuardedPurpose ctx "missing-datum" Nothing input 0
  let protected = [ueTxIn ownerInput, ueTxIn payerInput, collateral ctx]
      snapshot ref = queryJson ctx ["utxo", "--tx-in", ref, "--output-json"]
  before <- mapM snapshot protected
  (code, _, stderr) <- execCliAny (exec ctx) ["dijkstra", "transaction", "submit", "--tx-file", signed]
  H.note_ stderr
  H.assert $ case code of ExitFailure _ -> True; ExitSuccess -> False
  H.assert $ "MalformedGuardDatums" `isInfixOf` stderr
  after <- mapM snapshot protected
  before H.=== after

buildGuardedPurpose :: DijkstraTest -> String -> Maybe Integer -> [String] -> Integer -> H.Integration (FilePath, UtxoEntry)
buildGuardedPurpose ctx label datum args adjustment = do
  hash <- scriptHash ctx (v4Script ctx)
  ScriptHash sh <- H.evalEither $ deserialiseFromRawBytesHex $ BS.pack hash
  -- The hash form permits the deliberately invalid no-datum control to reach
  -- the node; the script-file form validates datum presence in the CLI.
  let requirement = ["--top-level-guard-script-hash", hash]
        ++ maybe [] (\n -> ["--top-level-guard-datum-value", show n]) datum
  sub <- buildSignedSub ctx label (args ++ requirement) [skey $ owner ctx]
  Exp.UnsignedSubTx subTx <- H.evalEither =<< H.evalIO
    (readFileTextEnvelope $ File $ directory ctx </> label <> ".unsigned")
  let expected = maybe L.SNothing (L.SJust . toAlonzoData . unsafeHashableScriptData . ScriptDataNumber) datum
  subTx ^. L.bodyTxL . L.requiredTopLevelGuardsL H.=== Map.singleton (L.ScriptHashObj sh) expected
  input <- largest ctx (payer ctx)
  body <- buildOuter ctx label sub input adjustment []
  plutus <- H.evalEither =<< H.evalIO (readFileTextEnvelope $ File $ v4Script ctx)
  let script = toShelleyScript $ ScriptInEra PlutusScriptV4InDijkstra $ PlutusScript PlutusScriptV4 plutus
      guard = TopGuard (L.ScriptHashObj sh) requirement (Just script) [] True Nothing
  addOuterScriptGuard ctx body guard script
  signed <- signBody ctx body [skey $ payer ctx, skey $ collateralWallet ctx]
  ShelleyTx _ tx <- readOuter signed
  tx ^. L.bodyTxL . L.guardsTxBodyL H.=== fromList [L.ScriptHashObj sh]
  pure (signed, input)

freshGuardKey :: DijkstraTest -> H.Integration (String, FilePath)
freshGuardKey ctx = do
  let key = directory ctx </> "top-guard.skey"
      vkey = directory ctx </> "top-guard.vkey"
  void $ cli ctx ["address", "key-gen", "--signing-key-file", key, "--verification-key-file", vkey]
  hash <- trim <$> cli ctx ["address", "key-hash", "--payment-verification-key-file", vkey]
  pure (hash, key)

scriptCases :: DijkstraTest -> FilePath -> L.Script (ShelleyLedgerEra DijkstraEra) -> [FilePath] -> Bool -> H.Integration ()
scriptCases ctx path script keys plutus = do
  hash <- scriptHash ctx path
  ScriptHash sh <- H.evalEither $ deserialiseFromRawBytesHex $ BS.pack hash
  -- Exercise a success case before the omission/recovery case, so a failure
  -- while rejecting malformed transactions does not hide the happy path.
  forM_ [ViaReference, ViaFile] $ \source -> do
    let label = show source
    ref <- case source of
      ViaFile -> pure Nothing
      ViaReference -> Just <$> referenceOutput ctx "top-guard-reference" path
    let requirement = case source of
          ViaFile -> ["--top-level-guard-script-file", path]
          ViaReference -> ["--top-level-guard-script-hash", hash]
        datum = if plutus then ["--top-level-guard-datum-value", "42"] else []
    runGuardCase ctx label (source == ViaFile) $
      TopGuard (L.ScriptHashObj sh) (requirement ++ datum) (Just script) keys plutus ref

runGuardCase :: DijkstraTest -> String -> Bool -> TopGuard -> H.Integration ()
runGuardCase ctx label checkMissing guard = do
  ownerInput <- largest ctx (owner ctx)
  payerInput <- largest ctx (payer ctx)
  sub <- buildSignedSub ctx label
    (["--tx-in", ueTxIn ownerInput, "--tx-out", address (owner ctx) <> "+" <> show (ueLovelace ownerInput)]
      ++ requirementArgs guard)
    [skey $ owner ctx]
  Exp.UnsignedSubTx subTx <- H.evalEither =<< H.evalIO
    (readFileTextEnvelope $ File $ directory ctx </> label <> ".unsigned")
  let expectedDatum = if isPlutus guard
        then L.SJust $ toAlonzoData $ unsafeHashableScriptData $ ScriptDataNumber 42
        else L.SNothing
  subTx ^. L.bodyTxL . L.requiredTopLevelGuardsL H.=== Map.singleton (credential guard) expectedDatum
  subTx ^. L.bodyTxL . L.guardsTxBodyL H.=== mempty
  subTx ^. L.witsTxL . L.scriptTxWitsL H.=== mempty
  subTx ^. L.witsTxL . L.rdmrsTxWitsL H.=== mempty
  let signingKeys = [skey $ payer ctx, skey $ collateralWallet ctx] ++ witnessKeys guard
      protectedInputs = [ueTxIn ownerInput, ueTxIn payerInput, collateral ctx]
      snapshot input = queryJson ctx ["utxo", "--tx-in", input, "--output-json"]
  when checkMissing $ do
    H.note_ "The node must reject the enclosing transaction without its required guard"
    before <- mapM snapshot protectedInputs
    body <- buildOuter ctx (label <> "-missing") sub payerInput 0 []
    signed <- signBody ctx body signingKeys
    (exitCode, _, stderr) <- execCliAny (exec ctx) ["dijkstra", "transaction", "submit", "--tx-file", signed]
    H.note_ stderr
    H.assert $ case exitCode of ExitFailure _ -> True; ExitSuccess -> False
    H.assert $ "MissingRequiredGuards" `isInfixOf` stderr
    after <- mapM snapshot protectedInputs
    before H.=== after
  H.note_ "Satisfy the requirement and submit the same signed sub-transaction"
  let extras = case credential guard of
        L.KeyHashObj kh -> ["--required-signer-hash", BS.unpack $ serialiseToRawBytesHex $ PaymentKeyHash $ coerceKeyRole kh]
        L.ScriptHashObj _ -> maybe [] (\ref -> ["--read-only-tx-in-reference", ref]) (referenceInput guard)
  body <- buildOuter ctx label sub payerInput 0 extras
  forM_ (witnessScript guard) $ addOuterScriptGuard ctx body guard
  signed <- signBody ctx body signingKeys
  -- Check the final signed file: CLI signing must preserve the API-built guard.
  ShelleyTx _ outer <- readOuter signed
  outer ^. L.bodyTxL . L.guardsTxBodyL H.=== fromList [credential guard]
  case referenceInput guard of
    Nothing -> pure ()
    Just _ -> do
      outer ^. L.witsTxL . L.scriptTxWitsL H.=== mempty
      H.assert $ not $ Set.null $ outer ^. L.bodyTxL . L.referenceInputsTxBodyL
  txid <- submitSigned ctx signed
  forM_ [ueTxIn ownerInput, ueTxIn payerInput] $ assertSpent ctx
  ownerId <- subId ctx label
  assertOutput ctx (ownerId <> "#0") (address $ owner ctx) (ueLovelace ownerInput)
  assertOutput ctx (txid <> "#0") (address $ payer ctx) (ueLovelace payerInput - fee)
  assertReference ctx (collateral ctx)
  forM_ (referenceInput guard) $ assertReference ctx

assertOutput :: DijkstraTest -> String -> String -> Integer -> H.Integration ()
assertOutput ctx input expectedAddress expectedLovelace = do
  output <- queryJson ctx ["utxo", "--tx-in", input, "--output-json"] >>= field input
  field "address" output >>= (H.=== expectedAddress)
  (field "value" output >>= field "lovelace") >>= (H.=== expectedLovelace)

readOuter :: FilePath -> H.Integration (Tx DijkstraEra)
readOuter path = H.evalEither =<< H.evalIO (readFileTextEnvelope $ File path)

-- The pinned CLI exposes required guards in sub-transactions but cannot add
-- native/Plutus guards to the enclosing transaction. Construct those fields
-- through the API before CLI signing. The script integrity calculation follows
-- CLI's addGuardWitnesses: guard indices follow the body's ordered guard set.
addOuterScriptGuard :: DijkstraTest -> FilePath -> TopGuard -> L.Script (ShelleyLedgerEra DijkstraEra) -> H.Integration ()
addOuterScriptGuard ctx body guard script = do
  ShelleyTx _ tx <- readOuter body
  let scripts = case referenceInput guard of
        Nothing -> Map.singleton (L.hashScript script) script
        Just _ -> mempty
      -- This fixture has exactly one outer guard, at index zero.
      redeemers = if isPlutus guard then L.Redeemers $ Map.singleton (L.GuardingPurpose $ L.AsIx 0)
        (toAlonzoData $ unsafeHashableScriptData $ ScriptDataNumber 0, L.ExUnits 1_000_000 100_000_000)
        else mempty
      guarded = tx
        & L.bodyTxL . L.guardsTxBodyL .~ fromList [credential guard]
        & L.witsTxL . L.scriptTxWitsL .~ scripts
        & L.witsTxL . L.rdmrsTxWitsL .~ redeemers
  complete <- if isPlutus guard then do
    pp <- H.readJsonFileOk (protocolParams ctx)
    let integrity = getScriptIntegrityHash (BuildTxWith $ Just $ LedgerProtocolParameters pp) (Set.singleton L.PlutusV4)
          (TxBodyScriptData AlonzoEraOnwardsDijkstra (guarded ^. L.witsTxL . L.datsTxWitsL) redeemers)
    pure $ guarded & L.bodyTxL . L.scriptIntegrityHashTxBodyL .~ integrity
    else pure guarded
  H.evalEither =<< H.evalIO (writeTxFileTextEnvelope ShelleyBasedEraDijkstra (File body) (ShelleyTx ShelleyBasedEraDijkstra complete))
