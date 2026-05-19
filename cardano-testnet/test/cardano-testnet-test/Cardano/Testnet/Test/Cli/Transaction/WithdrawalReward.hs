{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Smoke tests for @transaction build@ with a @--withdrawal@ flag,
-- covering both key-witnessed and Plutus-script-witnessed withdrawals.
-- Loosely related to https://github.com/IntersectMBO/cardano-cli/issues/965.
module Cardano.Testnet.Test.Cli.Transaction.WithdrawalReward
  ( hprop_tx_withdrawal_reward
  , hprop_tx_withdrawal_reward_plutus_v3
  ) where

import           Cardano.Api as Api
import qualified Cardano.Api.Ledger as L

import           Cardano.Testnet

import           Prelude

import           Control.Monad (void)
import           Data.Default.Class
import qualified Data.Text as Text
import           System.Exit (ExitCode (..))
import           System.FilePath ((</>))

import           Testnet.Components.Configuration
import           Testnet.Components.Query
import           Testnet.Defaults (plutusV3Script)
import           Testnet.Process.Run (execCli, execCliAny, mkExecConfig)
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Start.Types
import           Testnet.Types

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H


-- | Smoke test: @transaction build@ builds a transaction whose output
-- exceeds the input, with a @--withdrawal@ covering the shortfall
-- (@output = 2 * input@, @withdrawal = 2 * input@). The auto-balancer
-- sums the withdrawal into consumed value, producing
-- @change = inputs + withdrawal - outputs - fee = input - fee@ (positive).
--
-- The withdrawal amount is fictional; the CLI does not validate it against
-- on-chain reward balances at build time, and the test only exercises the
-- build step.
--
-- Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/transaction build with withdrawal/"'@
hprop_tx_withdrawal_reward :: Property
hprop_tx_withdrawal_reward = integrationRetryWorkspace 2 "tx-withdrawal-reward" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  conf@Conf { tempAbsPath } <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  let ceo = ConwayEraOnwardsConway
      sbe = convert ceo
      eraName = eraToString sbe
      creationOptions = def
        { creationEra = AnyShelleyBasedEra sbe
        , creationGenesisOptions = def { genesisEpochLength = 100 }
        }

  TestnetRuntime
    { testnetMagic
    , testnetNodes
    , wallets = wallet0:_
    , configurationFile
    }
    <- createAndRunTestnet creationOptions def conf

  node <- H.headM testnetNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket node
  execConfig <- mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic
  let socketPath = nodeSocketPath node

  epochStateView <- getEpochStateView configurationFile socketPath

  let delegatorStakeVKeyFp = tempAbsPath' </> "stake-delegators" </> "delegator1" </> "staking.vkey"

  delegatorStakeAddress <- filter (/= '\n') <$>
    execCli
      [ "latest", "stake-address", "build"
      , "--stake-verification-key-file", delegatorStakeVKeyFp
      , "--testnet-magic", show @Int testnetMagic
      ]
  H.note_ $ "Delegator stake address: " <> delegatorStakeAddress

  (txinForWithdrawal, TxOut _ txinValue _ _) <- H.nothingFailM $
    findLargestUtxoWithAddress epochStateView sbe $ paymentKeyInfoAddr wallet0
  let L.Coin inputLovelace = txOutValueToLovelace txinValue
  H.note_ $ "Input UTxO: " <> show txinForWithdrawal <> " = " <> show inputLovelace <> " lovelace"

  let withdrawalLovelace = inputLovelace * 2
      outputLovelace     = withdrawalLovelace

  let withdrawalTxBodyFp = work </> "withdrawal.txbody"

  (exitCode, _stdout, stderr) <- execCliAny execConfig
    [ eraName, "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet0
    , "--tx-in", Text.unpack $ renderTxIn txinForWithdrawal
    , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet0) <> "+" <> show outputLovelace
    , "--withdrawal", delegatorStakeAddress <> "+" <> show withdrawalLovelace
    , "--witness-override", show @Int 2
    , "--out-file", withdrawalTxBodyFp
    ]
  H.note_ stderr

  exitCode H.=== ExitSuccess

  -- Verify the transaction body was written and can be read back
  void $ H.readFile withdrawalTxBodyFp

-- | Plutus-script-witnessed withdrawal variant of 'hprop_tx_withdrawal_reward'.
-- A Plutus V3 always-succeeds script acts as the stake credential, exercising
-- the script-witness branch of the auto-balancer. Same balance arithmetic
-- (output exceeds input, withdrawal covers the difference), and the
-- withdrawal amount is likewise fictional.
--
-- Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/transaction build with plutus withdrawal/"'@
hprop_tx_withdrawal_reward_plutus_v3 :: Property
hprop_tx_withdrawal_reward_plutus_v3 = integrationRetryWorkspace 2 "tx-withdrawal-reward-plutus-v3" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  conf@Conf { tempAbsPath } <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  let ceo = ConwayEraOnwardsConway
      sbe = convert ceo
      eraName = eraToString sbe
      creationOptions = def
        { creationEra = AnyShelleyBasedEra sbe
        , creationGenesisOptions = def { genesisEpochLength = 100 }
        }

  TestnetRuntime
    { testnetMagic
    , testnetNodes
    , wallets = wallet0:wallet1:_
    , configurationFile
    }
    <- createAndRunTestnet creationOptions def conf

  node <- H.headM testnetNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket node
  execConfig <- mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic
  let socketPath = nodeSocketPath node

  epochStateView <- getEpochStateView configurationFile socketPath

  -- Write the always-succeeds Plutus V3 script used as stake credential
  plutusScriptFp <- H.note $ work </> "always-succeeds.plutusV3"
  H.writeFile plutusScriptFp $ Text.unpack plutusV3Script

  -- Build the bech32 stake address for the script credential
  scriptStakeAddr <- filter (/= '\n') <$> execCli
    [ eraName, "stake-address", "build"
    , "--stake-script-file", plutusScriptFp
    , "--testnet-magic", show @Int testnetMagic
    ]
  H.note_ $ "Script stake address: " <> scriptStakeAddr

  (txinForWithdrawal, TxOut _ txinValue _ _) <- H.nothingFailM $
    findLargestUtxoWithAddress epochStateView sbe $ paymentKeyInfoAddr wallet0
  let L.Coin inputLovelace = txOutValueToLovelace txinValue
  H.note_ $ "Input UTxO: " <> show txinForWithdrawal <> " = " <> show inputLovelace <> " lovelace"

  -- Plutus scripts require collateral
  txinCollateral <- findLargestUtxoForPaymentKey epochStateView sbe wallet1

  let withdrawalLovelace = inputLovelace * 2
      outputLovelace     = withdrawalLovelace

  let withdrawalTxBodyFp = work </> "withdrawal.txbody"

  (exitCode, _stdout, stderr) <- execCliAny execConfig
    [ eraName, "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet0
    , "--tx-in-collateral", Text.unpack $ renderTxIn txinCollateral
    , "--tx-in", Text.unpack $ renderTxIn txinForWithdrawal
    , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet0) <> "+" <> show outputLovelace
    , "--withdrawal", scriptStakeAddr <> "+" <> show withdrawalLovelace
    , "--withdrawal-script-file", plutusScriptFp
    , "--withdrawal-redeemer-value", "0"
    , "--witness-override", show @Int 2
    , "--out-file", withdrawalTxBodyFp
    ]
  H.note_ stderr

  exitCode H.=== ExitSuccess

  -- Verify the transaction body was written and can be read back
  void $ H.readFile withdrawalTxBodyFp
