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
import qualified Cardano.Testnet.Test.Cli.Queries
import qualified Cardano.Testnet.Test.Cli.QuerySlotNumber
import qualified Cardano.Testnet.Test.FoldBlocks
import qualified Cardano.Testnet.Test.LedgerEvents.Gov.ProposeNewConstitution
import qualified Cardano.Testnet.Test.LedgerEvents.Gov.ProposeNewConstitutionSPO as LedgerEvents
import qualified Cardano.Testnet.Test.LedgerEvents.SanityCheck as LedgerEvents
import qualified Cardano.Testnet.Test.LedgerEvents.TreasuryGrowth as LedgerEvents
import qualified Cardano.Testnet.Test.Node.Shutdown
import qualified Cardano.Testnet.Test.SubmitApi.Babbage.Transaction

import           Prelude

import qualified Control.Concurrent.QSem as IO
import qualified System.Environment as E
import qualified System.IO as IO
import           System.IO (BufferMode (LineBuffering), hSetBuffering, hSetEncoding, stdout, utf8)
import qualified System.IO.Unsafe as IO

import qualified Testnet.Property.Run as H

import qualified Test.Tasty as T
import           Test.Tasty (TestTree)
import qualified Test.Tasty.Ingredients as T

sem :: IO.QSem
sem = IO.unsafePerformIO $ IO.newQSem 1
{-# NOINLINE sem #-}

-- | Designate a test section that can be run concurrently or sequentially.
-- When running sequentially, the section will be protected by a semaphore.
mkSection :: Bool -> IO (TestTree -> TestTree)
mkSection concurrent =
  if concurrent
    then do
      IO.putStrLn "With concurrent sections"
      pure id
    else do
      IO.putStrLn "With sequential sections"
      pure $ T.withResource (IO.waitQSem sem) (const (IO.signalQSem sem)) . const

tests :: IO TestTree
tests = do
  section <- shouldRunConcurrently >>= mkSection
  pure $ T.testGroup "test/Spec.hs"
    [ T.testGroup "Spec"
      [ T.testGroup "Ledger Events"
        [ section $ H.ignoreOnWindows "Sanity Check" LedgerEvents.hprop_ledger_events_sanity_check
        , section $ H.ignoreOnWindows "Treasury Growth" LedgerEvents.prop_check_if_treasury_is_growing
        -- TODO: Replace foldBlocks with checkLedgerStateCondition
        , T.testGroup "Governance"
            [ section $ H.ignoreOnMacAndWindows "ProposeAndRatifyNewConstitution" Cardano.Testnet.Test.LedgerEvents.Gov.ProposeNewConstitution.hprop_ledger_events_propose_new_constitution
              -- FIXME Those tests are flaky
              -- , H.ignoreOnWindows "InfoAction" LedgerEvents.hprop_ledger_events_info_action
            , section $ H.ignoreOnWindows "ProposeNewConstitutionSPO" LedgerEvents.hprop_ledger_events_propose_new_constitution_spo
            , section $ H.ignoreOnWindows "DRepRetirement" DRepRetirement.hprop_drep_retirement
            ]
        , T.testGroup "Plutus"
            [ section $ H.ignoreOnWindows "PlutusV3" Cardano.Testnet.Test.Cli.Conway.Plutus.hprop_plutus_v3
            ]
        ]
      , T.testGroup "CLI"
        [ section $ H.ignoreOnWindows "Shutdown" Cardano.Testnet.Test.Node.Shutdown.hprop_shutdown
        -- ShutdownOnSigint fails on Mac with
        -- "Log file: /private/tmp/tmp.JqcjW7sLKS/kes-period-info-2-test-30c2d0d8eb042a37/logs/test-spo.stdout.log had no logs indicating the relevant node has minted blocks."
        , section $ H.ignoreOnMacAndWindows "ShutdownOnSigint" Cardano.Testnet.Test.Node.Shutdown.hprop_shutdownOnSigint
        -- ShutdownOnSlotSynced FAILS Still. The node times out and it seems the "shutdown-on-slot-synced" flag does nothing
        -- , H.ignoreOnWindows "ShutdownOnSlotSynced" Cardano.Testnet.Test.Node.Shutdown.hprop_shutdownOnSlotSynced
        , T.testGroup "Babbage"
            [ section $ H.ignoreOnMacAndWindows "leadership-schedule" Cardano.Testnet.Test.Cli.Babbage.LeadershipSchedule.hprop_leadershipSchedule -- FAILS
            , section $ H.ignoreOnWindows "stake-snapshot" Cardano.Testnet.Test.Cli.Babbage.StakeSnapshot.hprop_stakeSnapshot
            , section $ H.ignoreOnWindows "transaction" Cardano.Testnet.Test.Cli.Babbage.Transaction.hprop_transaction
            ]
        -- TODO: Conway -  Re-enable when create-staked is working in conway again
        --, T.testGroup "Conway"
        --  [ H.ignoreOnWindows "stake-snapshot" Cardano.Testnet.Test.Cli.Conway.StakeSnapshot.hprop_stakeSnapshot
        --  ]
          -- Ignored on Windows due to <stdout>: commitBuffer: invalid argument (invalid character)
          -- as a result of the kes-period-info output to stdout.
        , section $ H.ignoreOnWindows "kes-period-info" Cardano.Testnet.Test.Cli.KesPeriodInfo.hprop_kes_period_info
        , section $ H.ignoreOnWindows "query-slot-number" Cardano.Testnet.Test.Cli.QuerySlotNumber.hprop_querySlotNumber
        , section $ H.ignoreOnWindows "foldBlocks receives ledger state" Cardano.Testnet.Test.FoldBlocks.prop_foldBlocks
        , section $ H.ignoreOnWindows "CliQueries" Cardano.Testnet.Test.Cli.Queries.hprop_cli_queries
        ]
      ]
    , T.testGroup "SubmitApi"
      [ T.testGroup "Babbage"
        [ section $ H.ignoreOnWindows "transaction" Cardano.Testnet.Test.SubmitApi.Babbage.Transaction.hprop_transaction
        ]
      ]
    ]

shouldRunConcurrently :: IO Bool
shouldRunConcurrently = (== Just "1") <$> E.lookupEnv "CONCURRENT_TESTS"

ingredients :: [T.Ingredient]
ingredients = T.defaultIngredients

main :: IO ()
main = do
  Crypto.cryptoInit

  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  args <- E.getArgs

  E.withArgs args $ tests >>= T.defaultMainWithIngredients ingredients
