{-# LANGUAGE FlexibleInstances #-}

module Main
  ( main
  ) where

import           Prelude

import           Hedgehog (Property, property, success)
import           Hedgehog.Extras.Stock.OS (isWin32)
import qualified System.Environment as E
import           Test.Tasty (TestTree)
import qualified Test.Tasty as T
import qualified Test.Tasty.Hedgehog as H
import qualified Test.Tasty.Ingredients as T

import qualified Spec.Plutus.Direct.ScriptContextEquality
import qualified Spec.Plutus.Direct.ScriptContextEqualityMint
import qualified Spec.Plutus.Direct.TxInLockingPlutus
import qualified Spec.Plutus.Script.TxInLockingPlutus
import qualified Spec.Plutus.SubmitApi.TxInLockingPlutus
import qualified Spec.Shutdown

tests :: IO TestTree
tests = do
  pure $ T.testGroup "test/Spec.hs"
    [ T.testGroup "Spec"
      [ H.testProperty "Spec.Plutus.Direct.TxInLockingPlutus" Spec.Plutus.Direct.TxInLockingPlutus.hprop_plutus
      -- This hangs on Windows for an unknown reason
      , ignoreOnWindows "Spec.Plutus.Script.TxInLockingPlutus" Spec.Plutus.Script.TxInLockingPlutus.hprop_plutus
      , H.testProperty "Spec.Plutus.SubmitApi.TxInLockingPlutus" Spec.Plutus.SubmitApi.TxInLockingPlutus.hprop_plutus
      , ignoreOnWindows "Spec.Plutus.Direct.ScriptContextEquality"  Spec.Plutus.Direct.ScriptContextEquality.hprop_plutus_script_context_equality
      , ignoreOnWindows "Spec.Plutus.Direct.ScriptContextEqualityMint" Spec.Plutus.Direct.ScriptContextEqualityMint.hprop_plutus_script_context_mint_equality
        -- There is a blocking call on Windows that prevents graceful shutdown and we currently aren't testing the shutdown IPC flag.
      , H.testProperty "Spec.Shutdown" Spec.Shutdown.hprop_shutdown
      ]
    ]

ignoreOnWindows :: String -> Property -> TestTree
ignoreOnWindows pName prop =
  if isWin32
  then H.testProperty ("Property not tested on Windows: " ++ pName) $ property success
  else H.testProperty pName prop


ingredients :: [T.Ingredient]
ingredients = T.defaultIngredients

main :: IO ()
main = do
  args <- E.getArgs

  E.withArgs args $ tests >>= T.defaultMainWithIngredients ingredients
