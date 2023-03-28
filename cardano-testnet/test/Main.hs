{-# LANGUAGE FlexibleInstances #-}

module Main
  ( main
  ) where

import           Prelude
import           Test.Tasty (TestTree)

import qualified System.Environment as E
import qualified Test.Tasty as T
import qualified Test.Tasty.Ingredients as T

--import qualified Test.Cli.Alonzo.LeadershipSchedule
import qualified Test.Cli.Babbage.LeadershipSchedule
import qualified Test.Cli.Babbage.StakeSnapshot
import qualified Test.Cli.KesPeriodInfo
import qualified Test.FoldBlocks
import qualified Test.Node.Shutdown
import qualified Test.ShutdownOnSlotSynced

import           Testnet.Util.Ignore as H

tests :: IO TestTree
tests = pure $ T.testGroup "test/Spec.hs"
  [ T.testGroup "Spec"
    [ H.ignoreOnWindows "Shutdown" Test.Node.Shutdown.hprop_shutdown
    , H.ignoreOnWindows "ShutdownOnSlotSynced" Test.ShutdownOnSlotSynced.hprop_shutdownOnSlotSynced
    -- TODO: This is failing. Disabling until we can figure out why
    -- , T.testGroup "Alonzo"
    --   [ H.ignoreOnMacAndWindows "leadership-schedule" Test.Cli.Alonzo.LeadershipSchedule.hprop_leadershipSchedule
    --   ]
    , T.testGroup "Babbage"
      [ H.ignoreOnWindows "leadership-schedule" Test.Cli.Babbage.LeadershipSchedule.hprop_leadershipSchedule
      , H.ignoreOnWindows "stake-snapshot" Test.Cli.Babbage.StakeSnapshot.hprop_stakeSnapshot
      ]
      -- Ignored on Windows due to <stdout>: commitBuffer: invalid argument (invalid character)
      -- as a result of the kes-period-info output to stdout.
      -- TODO: Babbage temporarily ignored due to broken protocol-state query
    , H.disabled "kes-period-info" Test.Cli.KesPeriodInfo.hprop_kesPeriodInfo
    ]
  , H.ignoreOnWindows "foldBlocks receives ledger state" Test.FoldBlocks.prop_foldBlocks
  ]

ingredients :: [T.Ingredient]
ingredients = T.defaultIngredients

main :: IO ()
main = do
  args <- E.getArgs

  E.withArgs args $ tests >>= T.defaultMainWithIngredients ingredients
