{-# LANGUAGE OverloadedStrings #-}

module Cardano.Testnet.Test.Dijkstra.Staking
  ( hprop_sub_transaction_staking
  ) where

import           Cardano.Api.Ledger (EpochInterval (..))
import           Cardano.Testnet.Test.Dijkstra.Common

import           Control.Monad (forM_, void)
import           System.FilePath ((</>))

import           Testnet.Components.Query

import           Hedgehog (Property)
import qualified Hedgehog as H

-- | Certificates and real withdrawals, with direct and reference V4 witnesses.
-- Run with: --test-options '-p "/Dijkstra sub-transaction staking/"'
hprop_sub_transaction_staking :: Property
hprop_sub_transaction_staking = withDijkstra "dijkstra-staking" False $ \ctx -> do
  ref <- referenceOutput ctx "stake-reference" (v4Script ctx)
  deposit <- parameter ctx "stakeAddressDeposit"
  actionDeposit <- parameter ctx "govActionDeposit"
  lifetime <- parameter ctx "govActionLifetime"
  forM_ [ViaFile, ViaReference] $ \source -> do
    let label = show source
        script = v4Script ctx
    addr <- registerStake ctx source (label <> "-register") script ref
    -- An info action cannot enact. Its returned deposit gives the registered
    -- script account a positive balance without relying on pool reward timing.
    let action = directory ctx </> label <> ".info"
    void $ cli ctx $ ["dijkstra", "governance", "action", "create-info", "--testnet"
      , "--governance-action-deposit", show actionDeposit, "--deposit-return-stake-address", addr]
      ++ anchorArgs ++ ["--out-file", action]
    input <- keyInput ctx
    void $ submitSub ctx (label <> "-info") (input ++ ["--proposal-file", action])
      [skey $ owner ctx] (-actionDeposit)
    reward <- retryUntilM (epochView ctx) (WaitForEpochs $ EpochInterval $ fromIntegral $ lifetime + 4)
      (rewardBalance ctx addr) (> 0)
    reward H.=== actionDeposit
    input' <- keyInput ctx
    void $ submitSub ctx (label <> "-withdraw")
      (input' ++ ["--withdrawal", addr <> "+" <> show reward] ++ v4Witness source "withdrawal" script ref)
      [skey $ owner ctx] reward
    rewardBalance ctx addr >>= (H.=== 0)
    let cert = directory ctx </> label <> ".deregcert"
    void $ cli ctx ["dijkstra", "stake-address", "deregistration-certificate", "--stake-script-file", script
      , "--key-reg-deposit-amt", show deposit, "--out-file", cert]
    input'' <- keyInput ctx
    void $ submitSub ctx (label <> "-deregister")
      (input'' ++ ["--certificate-file", cert] ++ v4Witness source "certificate" script ref)
      [skey $ owner ctx] deposit
    stakeInfo ctx addr >>= (H.=== [])
    assertReference ctx ref
    if source == ViaReference then
      forM_ ["-register", "-withdraw", "-deregister"] $ \suffix -> assertReferenceOnly ctx (label <> suffix)
    else pure ()
