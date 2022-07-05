{-# LANGUAGE FlexibleInstances #-}

module Main
  ( main
  ) where

import           Prelude
-- import qualified Spec.Cli.KesPeriodInfo
import qualified Spec.Node.Shutdown
import qualified Spec.ShutdownOnSlotSynced
import qualified System.Environment as E
import           Test.Tasty (TestTree)
import qualified Test.Tasty as T
import qualified Test.Tasty.Ingredients as T
import qualified Test.Util as H

tests :: IO TestTree
tests = pure $ T.testGroup "test/Spec.hs"
  [ T.testGroup "Spec"
    [ H.ignoreOnWindows "Shutdown" Spec.Node.Shutdown.hprop_shutdown
    , H.ignoreOnWindows "ShutdownOnSlotSynced" Spec.ShutdownOnSlotSynced.hprop_shutdownOnSlotSynced
      -- Ignored on Windows due to <stdout>: commitBuffer: invalid argument (invalid character)
      -- as a result of the kes-period-info output to stdout.
      -- TODO: Babbage temporarily ignored due to broken protocol-state query
      -- H.ignoreOnWindows "kes-period-info" Spec.Cli.KesPeriodInfo.hprop_kes_period_info
    ]
  ]

ingredients :: [T.Ingredient]
ingredients = T.defaultIngredients

main :: IO ()
main = do
  args <- E.getArgs

  E.withArgs args $ tests >>= T.defaultMainWithIngredients ingredients
