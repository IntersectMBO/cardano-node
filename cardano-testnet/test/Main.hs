{-# LANGUAGE FlexibleInstances #-}

module Main
  ( main
  ) where

import           Prelude
import           Test.Tasty (TestTree)

import qualified Spec.Cli.Alonzo.LeadershipSchedule
import qualified Spec.Cli.Babbage.LeadershipSchedule
import qualified Spec.Cli.KesPeriodInfo
import qualified Spec.FoldBlocks
import qualified Spec.Node.Shutdown
import qualified Spec.ShutdownOnSlotSynced
import qualified System.Environment as E
import qualified Test.Tasty as T
import qualified Test.Tasty.Ingredients as T
import qualified Test.Util as H

tests :: IO TestTree
tests = pure $ T.testGroup "test/Spec.hs"
  [ T.testGroup "Spec"
    [  H.ignoreOnWindows "Shutdown" Spec.Node.Shutdown.hprop_shutdown
    , H.ignoreOnWindows "ShutdownOnSlotSynced" Spec.ShutdownOnSlotSynced.hprop_shutdownOnSlotSynced
    , T.testGroup "Alonzo"
      [ H.ignoreOnMacAndWindows "leadership-schedule" Spec.Cli.Alonzo.LeadershipSchedule.hprop_leadershipSchedule
      ]
    , T.testGroup "Babbage"
      [ H.ignoreOnMacAndWindows "leadership-schedule" Spec.Cli.Babbage.LeadershipSchedule.hprop_leadershipSchedule
      ]
      -- Ignored on Windows due to <stdout>: commitBuffer: invalid argument (invalid character)
      -- as a result of the kes-period-info output to stdout.
      -- TODO: Babbage temporarily ignored due to broken protocol-state query
    , H.disabled "kes-period-info" Spec.Cli.KesPeriodInfo.hprop_kes_period_info
    ]
  , Spec.FoldBlocks.tests
  ]

ingredients :: [T.Ingredient]
ingredients = T.defaultIngredients

main :: IO ()
main = do
  args <- E.getArgs

  E.withArgs args $ tests >>= T.defaultMainWithIngredients ingredients
