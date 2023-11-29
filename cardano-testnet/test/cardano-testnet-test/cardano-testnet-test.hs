{-# LANGUAGE FlexibleInstances #-}

module Main
  ( main
  ) where

import qualified Cardano.Crypto.Init as Crypto
import qualified Cardano.Testnet.Test.Cli.Babbage.LeadershipSchedule
import qualified Cardano.Testnet.Test.Cli.Babbage.StakeSnapshot
import qualified Cardano.Testnet.Test.Cli.Babbage.Transaction
import qualified Cardano.Testnet.Test.Cli.KesPeriodInfo
import qualified Cardano.Testnet.Test.Cli.QuerySlotNumber
import qualified Cardano.Testnet.Test.FoldBlocks
import qualified Cardano.Testnet.Test.Node.LedgerEvents.Governance.ProposeNewConstitution as LedgerEvents
import qualified Cardano.Testnet.Test.Node.LedgerEvents.SanityCheck
import qualified Cardano.Testnet.Test.Node.Shutdown
import qualified Cardano.Testnet.Test.SubmitApi.Babbage.Transaction

import           Prelude

import qualified System.Environment as E
import           System.IO (BufferMode (LineBuffering), hSetBuffering, hSetEncoding, stdout, utf8)

import qualified Test.Tasty as T
import           Test.Tasty (TestTree)
import qualified Test.Tasty.Ingredients as T

import qualified Testnet.Property.Run as H

tests :: IO TestTree
tests = pure $ T.testGroup "test/Spec.hs"
  [ T.testGroup "Spec"
      [ T.testGroup "CLI"
        [ H.ignoreOnWindows "Shutdown" Cardano.Testnet.Test.Node.Shutdown.hprop_shutdown
        , H.ignoreOnWindows "LedgerEvents" Cardano.Testnet.Test.Node.LedgerEvents.SanityCheck.hprop_ledger_events_sanity_check
        , H.ignoreOnWindows "Governance.ProposeNewConstitution" LedgerEvents.hprop_ledger_events_propose_new_constitution
        , H.ignoreOnWindows "ShutdownOnSigint" Cardano.Testnet.Test.Node.Shutdown.hprop_shutdownOnSigint
        -- ShutdownOnSlotSynced FAILS Still. The node times out and it seems the "shutdown-on-slot-synced" flag does nothing
        -- , H.ignoreOnWindows "ShutdownOnSlotSynced" Cardano.Testnet.Test.Node.Shutdown.hprop_shutdownOnSlotSynced
        , T.testGroup "Babbage"
            -- TODO: Babbage --next leadership schedule still fails. Once this fix is propagated to the cli (https://github.com/input-output-hk/cardano-api/pull/274)
            -- this should remedy. Double check and make sure we have re-enabled it and remove this comment.
            [ H.ignoreOnMacAndWindows "leadership-schedule" Cardano.Testnet.Test.Cli.Babbage.LeadershipSchedule.hprop_leadershipSchedule -- FAILS
            , H.ignoreOnWindows "stake-snapshot" Cardano.Testnet.Test.Cli.Babbage.StakeSnapshot.hprop_stakeSnapshot
            , H.ignoreOnWindows "transaction" Cardano.Testnet.Test.Cli.Babbage.Transaction.hprop_transaction
            ]
        -- TODO: Conway -  Re-enable when create-staked is working in conway again
        --, T.testGroup "Conway"
        --  [ H.ignoreOnWindows "stake-snapshot" Cardano.Testnet.Test.Cli.Conway.StakeSnapshot.hprop_stakeSnapshot
        --  ]
          -- Ignored on Windows due to <stdout>: cosmmitBuffer: invalid argument (invalid character)
          -- as a result of the kes-period-info output to stdout.
        , H.ignoreOnWindows "kes-period-info" Cardano.Testnet.Test.Cli.KesPeriodInfo.hprop_kes_period_info
        , H.ignoreOnWindows "query-slot-number" Cardano.Testnet.Test.Cli.QuerySlotNumber.hprop_querySlotNumber
        , H.ignoreOnWindows "foldBlocks receives ledger state" Cardano.Testnet.Test.FoldBlocks.prop_foldBlocks
        ]
      ]
  , T.testGroup "SubmitApi"
      [ T.testGroup "Babbage"
          [ H.ignoreOnWindows "transaction" Cardano.Testnet.Test.SubmitApi.Babbage.Transaction.hprop_transaction
          ]
      ]
  ]

ingredients :: [T.Ingredient]
ingredients = T.defaultIngredients

main :: IO ()
main = do
  Crypto.cryptoInit

  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  args <- E.getArgs

  E.withArgs args $ tests >>= T.defaultMainWithIngredients ingredients
