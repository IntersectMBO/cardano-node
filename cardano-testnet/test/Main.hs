{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main
  ( main
  ) where

import Prelude
import Spec.Cli.KesPeriodInfo qualified
import Spec.Node.Shutdown qualified
import Spec.ShutdownOnSlotSynced qualified
import System.Environment qualified as E
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Ingredients qualified as T
import Test.Util qualified as H

tests :: IO TestTree
tests = pure $ T.testGroup "test/Spec.hs"
  [ T.testGroup "Spec"
    [ H.ignoreOnWindows "Spec.Node.Shutdown" "Spec.Node.Shutdown" Spec.Node.Shutdown.hprop_shutdown
    , H.ignoreOnWindows "Spec.ShutdownOnSlotSynced" "Spec.ShutdownOnSlotSynced" Spec.ShutdownOnSlotSynced.hprop_shutdownOnSlotSynced
      -- Ignored on Windows due to <stdout>: commitBuffer: invalid argument (invalid character)
      -- as a result of the kes-period-info output to stdout.
      , H.ignoreOnWindows "Spec.Cli.KesPeriodInfo" "Spec.Cli.KesPeriodInfo" Spec.Cli.KesPeriodInfo.hprop_kes_period_info
    ]
  ]

ingredients :: [T.Ingredient]
ingredients = T.defaultIngredients

main :: IO ()
main = do
  args <- E.getArgs

  E.withArgs args $ tests >>= T.defaultMainWithIngredients ingredients
