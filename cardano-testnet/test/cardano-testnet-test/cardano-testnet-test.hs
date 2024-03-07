{-# LANGUAGE FlexibleInstances #-}

module Main
  ( main
  ) where

import qualified Cardano.Crypto.Init as Crypto
import qualified Cardano.Testnet.Test.Cli.Babbage.LeadershipSchedule
import qualified Cardano.Testnet.Test.Cli.Babbage.StakeSnapshot
import qualified Cardano.Testnet.Test.Cli.Babbage.Transaction
import qualified Cardano.Testnet.Test.Cli.Conway.DRepRetirement as DRepRetirement
import qualified Cardano.Testnet.Test.Cli.Conway.Plutus
import qualified Cardano.Testnet.Test.Cli.KesPeriodInfo
import qualified Cardano.Testnet.Test.Cli.QuerySlotNumber
import qualified Cardano.Testnet.Test.FoldBlocks
import qualified Cardano.Testnet.Test.LedgerEvents.Gov.ProposeNewConstitution
import qualified Cardano.Testnet.Test.LedgerEvents.Gov.ProposeNewConstitutionSPO as LedgerEvents
import qualified Cardano.Testnet.Test.LedgerEvents.SanityCheck as LedgerEvents
import qualified Cardano.Testnet.Test.LedgerEvents.TreasuryGrowth as LedgerEvents
import qualified Cardano.Testnet.Test.Node.Shutdown
import qualified Cardano.Testnet.Test.SubmitApi.Babbage.Transaction

import           Prelude

import qualified System.Environment as E
import           System.IO (BufferMode (LineBuffering), hSetBuffering, hSetEncoding, stdout, utf8)

import qualified Testnet.Property.Run as H

import qualified Test.Tasty as T
import           Test.Tasty (TestTree)
import qualified Test.Tasty.Ingredients as T

tests :: IO TestTree
tests = do
  testGroup <- runTestGroup <$> shouldRunInParallel
  pure $ testGroup "test/Spec.hs"
    [ testGroup "Spec"
        [ testGroup "Ledger Events"
            [ H.ignoreOnWindows "Sanity Check" LedgerEvents.hprop_ledger_events_sanity_check
            , H.ignoreOnWindows "Treasury Growth" LedgerEvents.prop_check_if_treasury_is_growing
            -- TODO: Replace foldBlocks with checkLedgerStateCondition
            , testGroup "Governance"
                [ H.ignoreOnMacAndWindows "ProposeAndRatifyNewConstitution" Cardano.Testnet.Test.LedgerEvents.Gov.ProposeNewConstitution.hprop_ledger_events_propose_new_constitution
                  -- FIXME Those tests are flaky
                  -- , H.ignoreOnWindows "InfoAction" LedgerEvents.hprop_ledger_events_info_action
                , H.ignoreOnWindows "ProposeNewConstitutionSPO" LedgerEvents.hprop_ledger_events_propose_new_constitution_spo
                , H.ignoreOnWindows "DRepRetirement" DRepRetirement.hprop_drep_retirement
                ]
            , testGroup "Plutus"
                [ H.ignoreOnWindows "PlutusV3" Cardano.Testnet.Test.Cli.Conway.Plutus.hprop_plutus_v3]
            ]
        , testGroup "CLI"
          [ H.ignoreOnWindows "Shutdown" Cardano.Testnet.Test.Node.Shutdown.hprop_shutdown
          -- ShutdownOnSigint fails on Mac with
          -- "Log file: /private/tmp/tmp.JqcjW7sLKS/kes-period-info-2-test-30c2d0d8eb042a37/logs/test-spo.stdout.log had no logs indicating the relevant node has minted blocks."
          , H.ignoreOnMacAndWindows "ShutdownOnSigint" Cardano.Testnet.Test.Node.Shutdown.hprop_shutdownOnSigint
          -- ShutdownOnSlotSynced FAILS Still. The node times out and it seems the "shutdown-on-slot-synced" flag does nothing
          -- , H.ignoreOnWindows "ShutdownOnSlotSynced" Cardano.Testnet.Test.Node.Shutdown.hprop_shutdownOnSlotSynced
          , testGroup "Babbage"
              [ H.ignoreOnMacAndWindows "leadership-schedule" Cardano.Testnet.Test.Cli.Babbage.LeadershipSchedule.hprop_leadershipSchedule -- FAILS
              , H.ignoreOnWindows "stake-snapshot" Cardano.Testnet.Test.Cli.Babbage.StakeSnapshot.hprop_stakeSnapshot
              , H.ignoreOnWindows "transaction" Cardano.Testnet.Test.Cli.Babbage.Transaction.hprop_transaction
              ]
          -- TODO: Conway -  Re-enable when create-staked is working in conway again
          --, testGroup "Conway"
          --  [ H.ignoreOnWindows "stake-snapshot" Cardano.Testnet.Test.Cli.Conway.StakeSnapshot.hprop_stakeSnapshot
          --  ]
            -- Ignored on Windows due to <stdout>: commitBuffer: invalid argument (invalid character)
            -- as a result of the kes-period-info output to stdout.
          , H.ignoreOnWindows "kes-period-info" Cardano.Testnet.Test.Cli.KesPeriodInfo.hprop_kes_period_info
          , H.ignoreOnWindows "query-slot-number" Cardano.Testnet.Test.Cli.QuerySlotNumber.hprop_querySlotNumber
          , H.ignoreOnWindows "foldBlocks receives ledger state" Cardano.Testnet.Test.FoldBlocks.prop_foldBlocks
          ]

        ]
    , testGroup "SubmitApi"
        [ testGroup "Babbage"
            [ H.ignoreOnWindows "transaction" Cardano.Testnet.Test.SubmitApi.Babbage.Transaction.hprop_transaction
            ]
        ]
    ]

shouldRunInParallel :: IO Bool
shouldRunInParallel = (== Just "1") <$> E.lookupEnv "PARALLEL_TESTNETS"

-- FIXME Right now when running tests concurrently it makes them flaky
runTestGroup
  :: Bool -- ^ True to run in parallel
  -> T.TestName
  -> [TestTree]
  -> TestTree
runTestGroup True name = T.testGroup name
runTestGroup False name = T.sequentialTestGroup name T.AllFinish

ingredients :: [T.Ingredient]
ingredients = T.defaultIngredients

main :: IO ()
main = do
  Crypto.cryptoInit

  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  args <- E.getArgs

  E.withArgs args $ tests >>= T.defaultMainWithIngredients ingredients
