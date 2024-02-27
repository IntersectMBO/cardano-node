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
import qualified Cardano.Testnet.Test.LedgerEvents.Gov.ProposeNewConstitutionSPO as LedgerEvents
import qualified Cardano.Testnet.Test.LedgerEvents.SanityCheck as LedgerEvents
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
tests = pure $ sequentialTestGroup "test/Spec.hs"
  [ sequentialTestGroup "Spec"
      [ sequentialTestGroup "Ledger Events"
          [ H.ignoreOnWindows "Sanity Check" LedgerEvents.hprop_ledger_events_sanity_check
          -- TODO: Replace foldBlocks with checkLedgerStateCondition
          , T.testGroup "Governance"
              -- FIXME Those tests are flaky
              [ -- H.ignoreOnMacAndWindows "ProposeAndRatifyNewConstitution" LedgerEvents.hprop_ledger_events_propose_new_constitution
                -- , H.ignoreOnWindows "InfoAction" LedgerEvents.hprop_ledger_events_info_action
                H.ignoreOnWindows "ProposeNewConstitutionSPO" LedgerEvents.hprop_ledger_events_propose_new_constitution_spo
              , H.ignoreOnWindows "DRepRetirement" DRepRetirement.hprop_drep_retirement
              ]
          , T.testGroup "Plutus"
              [ H.ignoreOnWindows "PlutusV3" Cardano.Testnet.Test.Cli.Conway.Plutus.hprop_plutus_v3]
          ]
      , sequentialTestGroup "CLI"
        [ H.ignoreOnWindows "Shutdown" Cardano.Testnet.Test.Node.Shutdown.hprop_shutdown
        -- ShutdownOnSigint fails on Mac with
        -- "Log file: /private/tmp/tmp.JqcjW7sLKS/kes-period-info-2-test-30c2d0d8eb042a37/logs/test-spo.stdout.log had no logs indicating the relevant node has minted blocks."
        , H.ignoreOnMacAndWindows "ShutdownOnSigint" Cardano.Testnet.Test.Node.Shutdown.hprop_shutdownOnSigint
        -- ShutdownOnSlotSynced FAILS Still. The node times out and it seems the "shutdown-on-slot-synced" flag does nothing
        -- , H.ignoreOnWindows "ShutdownOnSlotSynced" Cardano.Testnet.Test.Node.Shutdown.hprop_shutdownOnSlotSynced
        , sequentialTestGroup "Babbage"
            -- TODO: Babbage --next leadership schedule still fails. Once this fix is propagated to the cli (https://github.com/input-output-hk/cardano-api/pull/274)
            -- this should remedy. Double check and make sure we have re-enabled it and remove this comment.
            [ H.ignoreOnMacAndWindows "leadership-schedule" Cardano.Testnet.Test.Cli.Babbage.LeadershipSchedule.hprop_leadershipSchedule -- FAILS
            , H.ignoreOnWindows "stake-snapshot" Cardano.Testnet.Test.Cli.Babbage.StakeSnapshot.hprop_stakeSnapshot
            , H.ignoreOnWindows "transaction" Cardano.Testnet.Test.Cli.Babbage.Transaction.hprop_transaction
            ]
        -- TODO: Conway -  Re-enable when create-staked is working in conway again
        --, sequentialTestGroup "Conway"
        --  [ H.ignoreOnWindows "stake-snapshot" Cardano.Testnet.Test.Cli.Conway.StakeSnapshot.hprop_stakeSnapshot
        --  ]
          -- Ignored on Windows due to <stdout>: cosmmitBuffer: invalid argument (invalid character)
          -- as a result of the kes-period-info output to stdout.
        , H.ignoreOnWindows "kes-period-info" Cardano.Testnet.Test.Cli.KesPeriodInfo.hprop_kes_period_info
        , H.ignoreOnWindows "query-slot-number" Cardano.Testnet.Test.Cli.QuerySlotNumber.hprop_querySlotNumber
        , H.ignoreOnWindows "foldBlocks receives ledger state" Cardano.Testnet.Test.FoldBlocks.prop_foldBlocks
        ]

      ]
  , sequentialTestGroup "SubmitApi"
      [ sequentialTestGroup "Babbage"
          [ H.ignoreOnWindows "transaction" Cardano.Testnet.Test.SubmitApi.Babbage.Transaction.hprop_transaction
          ]
      ]
  ]

-- FIXME Right now when running tests concurrently it makes tests flaky and sometimes stuck
sequentialTestGroup :: T.TestName -> [TestTree] -> TestTree
sequentialTestGroup name = T.sequentialTestGroup name T.AllFinish

ingredients :: [T.Ingredient]
ingredients = T.defaultIngredients

main :: IO ()
main = do
  Crypto.cryptoInit

  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  args <- E.getArgs

  E.withArgs args $ tests >>= T.defaultMainWithIngredients ingredients
