{-# LANGUAGE OverloadedStrings #-}

module Cardano.Testnet.Test.Dijkstra.Governance
  ( hprop_sub_transaction_governance
  ) where

import           Cardano.Testnet.Test.Dijkstra.Common

import           Control.Monad (forM_, void)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import           System.FilePath ((</>))

import           Hedgehog (Property)
import qualified Hedgehog as H

-- | V4 Proposing and Voting purposes, with real proposals and a registered
-- script DRep. Confirm recorded votes; ratification is not needed here.
-- Run with: --test-options '-p "/Dijkstra sub-transaction governance/"'
hprop_sub_transaction_governance :: Property
hprop_sub_transaction_governance = withDijkstra "dijkstra-governance" True $ \ctx -> do
  let script = v4Script ctx
      drepCert = directory ctx </> "drep.regcert"
  ref <- referenceOutput ctx "governance-reference" script
  hash <- scriptHash ctx script
  returnAddr <- registerStake ctx ViaFile "return-account" script ref
  drepDeposit <- parameter ctx "dRepDeposit"
  actionDeposit <- parameter ctx "govActionDeposit"
  void $ cli ctx ["dijkstra", "governance", "drep", "registration-certificate"
    , "--drep-script-hash", hash, "--key-reg-deposit-amt", show drepDeposit, "--out-file", drepCert]
  registrationInput <- keyInput ctx
  void $ submitSub ctx "register-drep"
    (registrationInput ++ ["--certificate-file", drepCert] ++ v4Witness ViaFile "certificate" script ref)
    [skey $ owner ctx] (-drepDeposit)
  forM_ [ViaFile, ViaReference] $ \source -> do
    let label = show source
        proposal = directory ctx </> label <> ".proposal"
        vote = directory ctx </> label <> ".vote"
    void $ cli ctx $ ["dijkstra", "governance", "action", "create-protocol-parameters-update", "--testnet"
      , "--governance-action-deposit", show actionDeposit, "--deposit-return-stake-address", returnAddr
      , "--constitution-script-hash", hash, "--min-fee-linear", "45"]
      ++ anchorArgs ++ ["--out-file", proposal]
    input <- keyInput ctx
    void $ submitSub ctx (label <> "-propose")
      (input ++ ["--proposal-file", proposal] ++ v4Witness source "proposal" script ref)
      [skey $ owner ctx] (-actionDeposit)
    actionId <- subId ctx (label <> "-propose")
    state <- proposalState ctx actionId
    action <- field "actionId" state
    index <- field "govActionIx" action
    void $ cli ctx ["dijkstra", "governance", "vote", "create", "--yes"
      , "--governance-action-tx-id", actionId, "--governance-action-index", show (index :: Integer)
      , "--drep-script-hash", hash, "--out-file", vote]
    input' <- keyInput ctx
    void $ submitSub ctx (label <> "-vote")
      (input' ++ ["--vote-file", vote] ++ v4Witness source "vote" script ref)
      [skey $ owner ctx] 0
    votes <- proposalState ctx actionId >>= field "dRepVotes"
    KM.lookup (K.fromString $ "scriptHash-" <> hash) votes H.=== Just (A.String "VoteYes")
    assertReference ctx ref
    if source == ViaReference then
      forM_ ["-propose", "-vote"] $ \suffix -> assertReferenceOnly ctx (label <> suffix)
    else pure ()
