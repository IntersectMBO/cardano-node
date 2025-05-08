{-# LANGUAGE FlexibleInstances #-}

module Main
  ( main
  ) where

import qualified Cardano.Crypto.Init as Crypto
import qualified Cardano.Testnet.Test.Api.TxSupplementalDatum
import qualified Cardano.Testnet.Test.Cli.KesPeriodInfo
import qualified Cardano.Testnet.Test.Cli.Query
import qualified Cardano.Testnet.Test.Cli.QuerySlotNumber
import qualified Cardano.Testnet.Test.Cli.Plutus.Scripts
import qualified Cardano.Testnet.Test.Cli.Plutus.CostCalculation
import qualified Cardano.Testnet.Test.Cli.StakeSnapshot
import qualified Cardano.Testnet.Test.Cli.Transaction
import qualified Cardano.Testnet.Test.Cli.Transaction.RegisterDeregisterStakeAddress
import qualified Cardano.Testnet.Test.FoldEpochState
import qualified Cardano.Testnet.Test.Gov.CommitteeAddNew as Gov
import qualified Cardano.Testnet.Test.Gov.DRepDeposit as Gov
import qualified Cardano.Testnet.Test.Gov.DRepRetirement as Gov
import qualified Cardano.Testnet.Test.Gov.GovActionTimeout as Gov
import qualified Cardano.Testnet.Test.Gov.InfoAction as LedgerEvents
import qualified Cardano.Testnet.Test.Gov.PParamChangeFailsSPO as Gov
import qualified Cardano.Testnet.Test.Gov.ProposeNewConstitution as Gov
import qualified Cardano.Testnet.Test.Gov.Transaction.HashMismatch as WrongHash
import qualified Cardano.Testnet.Test.Gov.TreasuryDonation as Gov
import qualified Cardano.Testnet.Test.Gov.TreasuryWithdrawal as Gov
import qualified Cardano.Testnet.Test.Node.Shutdown
import qualified Cardano.Testnet.Test.SanityCheck as LedgerEvents
import qualified Cardano.Testnet.Test.SubmitApi.Transaction

import           Prelude

import qualified System.Environment as E
import           System.IO (BufferMode (LineBuffering), hSetBuffering, hSetEncoding, stdout, utf8)

import           Testnet.Property.Run (ignoreOnMacAndWindows, ignoreOnWindows)

import qualified Test.Tasty as T
import           Test.Tasty (TestTree)

-- import qualified Cardano.Testnet.Test.Cli.LeadershipSchedule
-- import qualified Cardano.Testnet.Test.Gov.NoConfidence as Gov
-- import qualified Cardano.Testnet.Test.Gov.ProposeNewConstitutionSPO as Gov
-- import qualified Cardano.Testnet.Test.Cli.LeadershipSchedule
-- import qualified Cardano.Testnet.Test.Gov.TreasuryGrowth as Gov

tests :: IO TestTree
tests = do
  pure $ T.testGroup "test/Spec.hs"
    [ T.testGroup "Spec"
        [ T.testGroup "Ledger Events"
           [ ignoreOnWindows "Sanity Check" LedgerEvents.hprop_ledger_events_sanity_check
           -- FIXME this tests gets stuck - investigate why
           -- , ignoreOnWindows "Treasury Growth" Gov.prop_check_if_treasury_is_growing
            -- TODO: Replace foldBlocks with checkConditionResult
            , T.testGroup "Governance"
               [ ignoreOnMacAndWindows "Committee Add New" Gov.hprop_constitutional_committee_add_new
               -- FIXME No Confidence has SPO voting, requires multiple SPOs
               -- , ignoreOnMacAndWindows "Committee Motion Of No Confidence"  Gov.hprop_gov_no_confidence
               -- TODO: Disabled because proposals for parameter changes are not working
               -- , ignoreOnWindows "DRep Activity" Gov.hprop_check_drep_activity
               -- , ignoreOnWindows "Predefined Abstain DRep" Gov.hprop_check_predefined_abstain_drep
               , ignoreOnWindows "DRep Deposits" Gov.hprop_ledger_events_drep_deposits
               , ignoreOnWindows "DRep Retirement" Gov.hprop_drep_retirement
               , ignoreOnMacAndWindows "Propose And Ratify New Constitution" Gov.hprop_ledger_events_propose_new_constitution
               -- FIXME: this test is flaky when there are >1 SPOs in testnet
               -- , ignoreOnWindows "Propose New Constitution SPO" Gov.hprop_ledger_events_propose_new_constitution_spo
               , ignoreOnWindows "Gov Action Timeout" Gov.hprop_check_gov_action_timeout
               , ignoreOnWindows "Treasury Donation" Gov.hprop_ledger_events_treasury_donation
               , ignoreOnMacAndWindows "Treasury Withdrawal" Gov.hprop_ledger_events_treasury_withdrawal
               , ignoreOnWindows "PParam change fails for SPO" Gov.hprop_check_pparam_fails_spo
               , ignoreOnWindows "InfoAction" LedgerEvents.hprop_ledger_events_info_action
               , ignoreOnWindows "Transaction Build Wrong Hash" WrongHash.hprop_transaction_build_wrong_hash
               ]
            , T.testGroup "Plutus"
                [ ignoreOnWindows "PlutusV3 purposes" Cardano.Testnet.Test.Cli.Plutus.Scripts.hprop_plutus_purposes_v3
                , ignoreOnWindows "PlutusV2 transaction with two script certs" Cardano.Testnet.Test.Cli.Plutus.Scripts.hprop_tx_two_script_certs_v2
                , T.testGroup "Cost Calc"
                  [ ignoreOnWindows "Ref Script" Cardano.Testnet.Test.Cli.Plutus.CostCalculation.hprop_ref_plutus_cost_calculation
                  , ignoreOnWindows "Normal Script" Cardano.Testnet.Test.Cli.Plutus.CostCalculation.hprop_included_plutus_cost_calculation
                  , ignoreOnWindows "Simple Script" Cardano.Testnet.Test.Cli.Plutus.CostCalculation.hprop_included_simple_script_cost_calculation
                  ]
                ]
           ]
        , T.testGroup "API"
        [ignoreOnWindows "transaction with supplemental datum" Cardano.Testnet.Test.Api.TxSupplementalDatum.hprop_tx_supp_datum]
        , T.testGroup "CLI"
          [ ignoreOnWindows "Shutdown" Cardano.Testnet.Test.Node.Shutdown.hprop_shutdown
          -- ShutdownOnSigint fails on Mac with
          -- "Log file: /private/tmp/tmp.JqcjW7sLKS/kes-period-info-2-test-30c2d0d8eb042a37/logs/test-spo.stdout.log had no logs indicating the relevant node has minted blocks."
          , ignoreOnMacAndWindows "Shutdown On Sigint" Cardano.Testnet.Test.Node.Shutdown.hprop_shutdownOnSigint
          , ignoreOnWindows "Shutdown On SlotSynced" Cardano.Testnet.Test.Node.Shutdown.hprop_shutdownOnSlotSynced
          , ignoreOnWindows "stake snapshot" Cardano.Testnet.Test.Cli.StakeSnapshot.hprop_stakeSnapshot
          , ignoreOnWindows "simple transaction build" Cardano.Testnet.Test.Cli.Transaction.hprop_transaction
          , ignoreOnWindows "register deregister stake address in transaction build"  Cardano.Testnet.Test.Cli.Transaction.RegisterDeregisterStakeAddress.hprop_tx_register_deregister_stake_address
          -- FIXME
          -- , ignoreOnMacAndWindows "leadership-schedule" Cardano.Testnet.Test.Cli.LeadershipSchedule.hprop_leadershipSchedule

          -- TODO: Conway -  Re-enable when create-staked is working in conway again
          --, T.testGroup "Conway"
          --  [ ignoreOnWindows "stake-snapshot" Cardano.Testnet.Test.Cli.Conway.StakeSnapshot.hprop_stakeSnapshot
          --  ]
            -- Ignored on Windows due to <stdout>: commitBuffer: invalid argument (invalid character)
            -- as a result of the kes-period-info output to stdout.
          , ignoreOnWindows "kes-period-info" Cardano.Testnet.Test.Cli.KesPeriodInfo.hprop_kes_period_info
          , ignoreOnWindows "query-slot-number" Cardano.Testnet.Test.Cli.QuerySlotNumber.hprop_querySlotNumber
          , ignoreOnWindows "foldEpochState receives ledger state" Cardano.Testnet.Test.FoldEpochState.prop_foldEpochState
          , ignoreOnMacAndWindows "CliQueries" Cardano.Testnet.Test.Cli.Query.hprop_cli_queries
          ]
        ]
    , T.testGroup "SubmitApi"
        [ ignoreOnMacAndWindows "transaction" Cardano.Testnet.Test.SubmitApi.Transaction.hprop_transaction
        ]
    ]

main :: IO ()
main = do
  Crypto.cryptoInit

  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  args <- E.getArgs

  E.withArgs args $ tests >>= T.defaultMainWithIngredients T.defaultIngredients
