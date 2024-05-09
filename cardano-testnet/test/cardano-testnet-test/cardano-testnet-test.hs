{-# LANGUAGE FlexibleInstances #-}

module Main
  ( main
  ) where

import qualified Cardano.Crypto.Init as Crypto
import qualified Cardano.Testnet.Test.Cli.Babbage.LeadershipSchedule
import qualified Cardano.Testnet.Test.Cli.Babbage.StakeSnapshot
import qualified Cardano.Testnet.Test.Cli.Babbage.Transaction
import qualified Cardano.Testnet.Test.Cli.Conway.Plutus
import qualified Cardano.Testnet.Test.Cli.KesPeriodInfo
import qualified Cardano.Testnet.Test.Cli.Query
import qualified Cardano.Testnet.Test.Cli.QuerySlotNumber
import qualified Cardano.Testnet.Test.FoldEpochState
import qualified Cardano.Testnet.Test.Gov.DRepDeposit as Gov
import qualified Cardano.Testnet.Test.Gov.DRepRetirement as Gov
import qualified Cardano.Testnet.Test.Gov.ProposeNewConstitution as Gov
import qualified Cardano.Testnet.Test.Gov.ProposeNewConstitutionSPO as Gov
import qualified Cardano.Testnet.Test.Gov.TreasuryGrowth as Gov
import qualified Cardano.Testnet.Test.Gov.TreasuryWithdrawal as Gov
import qualified Cardano.Testnet.Test.Node.Shutdown
import qualified Cardano.Testnet.Test.SanityCheck as LedgerEvents
import qualified Cardano.Testnet.Test.SubmitApi.Babbage.Transaction

import           Prelude

import qualified System.Environment as E
import qualified System.Exit as IO
import qualified System.IO as IO
import           System.IO (BufferMode (LineBuffering), hSetBuffering, hSetEncoding, stdout, utf8)

import qualified Testnet.Property.Run as H

import qualified Test.Tasty as T
import           Test.Tasty (TestTree)
import qualified Test.Tasty.Ingredients as T
import qualified Test.Tasty.Options as T
import qualified Test.Tasty.Runners as T

tests :: IO TestTree
tests = do
  pure $ T.testGroup "test/Spec.hs"
    [ T.testGroup "Spec"
        [ T.testGroup "Ledger Events"
            [ H.ignoreOnWindows "Sanity Check" LedgerEvents.hprop_ledger_events_sanity_check
            , H.ignoreOnWindows "Treasury Growth" Gov.prop_check_if_treasury_is_growing
            -- TODO: Replace foldBlocks with checkLedgerStateCondition
            , T.testGroup "Governance"
                [
               -- TODO: "DRep Activity" is too flaky at the moment. Disabling until we can fix it.
               -- , H.ignoreOnWindows "DRep Activity" Cardano.Testnet.Test.LedgerEvents.Gov.DRepActivity.hprop_check_drep_activity
                  H.ignoreOnWindows "DRep Deposits" Gov.hprop_ledger_events_drep_deposits
                  -- FIXME Those tests are flaky
                  -- , H.ignoreOnWindows "InfoAction" LedgerEvents.hprop_ledger_events_info_action
                , H.ignoreOnWindows "DRep Retirement" Gov.hprop_drep_retirement
                , H.ignoreOnMacAndWindows "Propose And Ratify New Constitution" Gov.hprop_ledger_events_propose_new_constitution
                , H.ignoreOnWindows "Propose New Constitution SPO" Gov.hprop_ledger_events_propose_new_constitution_spo
                , H.ignoreOnWindows "Treasury Withdrawal" Gov.hprop_ledger_events_treasury_withdrawal
                ]
            , T.testGroup "Plutus"
                [ H.ignoreOnWindows "PlutusV3" Cardano.Testnet.Test.Cli.Conway.Plutus.hprop_plutus_v3]
            ]
        , T.testGroup "CLI"
          [ H.ignoreOnWindows "Shutdown" Cardano.Testnet.Test.Node.Shutdown.hprop_shutdown
          -- ShutdownOnSigint fails on Mac with
          -- "Log file: /private/tmp/tmp.JqcjW7sLKS/kes-period-info-2-test-30c2d0d8eb042a37/logs/test-spo.stdout.log had no logs indicating the relevant node has minted blocks."
          , H.ignoreOnMacAndWindows "Shutdown On Sigint" Cardano.Testnet.Test.Node.Shutdown.hprop_shutdownOnSigint
          -- ShutdownOnSlotSynced FAILS Still. The node times out and it seems the "shutdown-on-slot-synced" flag does nothing
          -- , H.ignoreOnWindows "ShutdownOnSlotSynced" Cardano.Testnet.Test.Node.Shutdown.hprop_shutdownOnSlotSynced
          , T.testGroup "Babbage"
              [ H.ignoreOnMacAndWindows "leadership-schedule" Cardano.Testnet.Test.Cli.Babbage.LeadershipSchedule.hprop_leadershipSchedule -- FAILS
              , H.ignoreOnWindows "stake-snapshot" Cardano.Testnet.Test.Cli.Babbage.StakeSnapshot.hprop_stakeSnapshot
              , H.ignoreOnWindows "transaction" Cardano.Testnet.Test.Cli.Babbage.Transaction.hprop_transaction
              ]
          -- TODO: Conway -  Re-enable when create-staked is working in conway again
          --, T.testGroup "Conway"
          --  [ H.ignoreOnWindows "stake-snapshot" Cardano.Testnet.Test.Cli.Conway.StakeSnapshot.hprop_stakeSnapshot
          --  ]
            -- Ignored on Windows due to <stdout>: commitBuffer: invalid argument (invalid character)
            -- as a result of the kes-period-info output to stdout.
          , H.ignoreOnWindows "kes-period-info" Cardano.Testnet.Test.Cli.KesPeriodInfo.hprop_kes_period_info
          , H.ignoreOnWindows "query-slot-number" Cardano.Testnet.Test.Cli.QuerySlotNumber.hprop_querySlotNumber
          , H.ignoreOnWindows "foldEpochState receives ledger state" Cardano.Testnet.Test.FoldEpochState.prop_foldEpochState
          , H.ignoreOnWindows "CliQueries" Cardano.Testnet.Test.Cli.Query.hprop_cli_queries
          ]
        ]
    , T.testGroup "SubmitApi"
        [ T.testGroup "Babbage"
            [ H.ignoreOnWindows "transaction" Cardano.Testnet.Test.SubmitApi.Babbage.Transaction.hprop_transaction
            ]
        ]
    ]

defaultMainWithIngredientsAndOptions :: [T.Ingredient] -> T.OptionSet -> T.TestTree -> IO ()
defaultMainWithIngredientsAndOptions ins opts testTree = do
  T.installSignalHandlers
  parsedOpts <- T.parseOptions ins testTree
  let opts' = opts <> parsedOpts

  case T.tryIngredients ins opts' testTree of
    Nothing -> do
      IO.hPutStrLn IO.stderr
        "No ingredients agreed to run. Something is wrong either with your ingredient set or the options."
      IO.exitFailure
    Just act -> do
      ok <- act
      if ok then IO.exitSuccess else IO.exitFailure

main :: IO ()
main = do
  Crypto.cryptoInit

  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  args <- E.getArgs

  let opts = T.singleOption $ T.NumThreads 1

  E.withArgs args $ tests >>= defaultMainWithIngredientsAndOptions T.defaultIngredients opts
