{-# LANGUAGE FlexibleInstances #-}

module Main
  ( main
  ) where

import           Prelude

import           Test.Tasty (TestTree)

-- TODO: Move to plutus-apps
-- import qualified Spec.Plutus.Direct.CertifyingAndWithdrawingPlutus
-- import qualified Spec.Plutus.Direct.ScriptContextEquality
-- import qualified Spec.Plutus.Direct.ScriptContextEqualityMint
-- import qualified Spec.Plutus.Direct.TxInLockingPlutus
-- import qualified Spec.Plutus.Script.TxInLockingPlutus
-- import qualified Spec.Plutus.SubmitApi.TxInLockingPlutus
import qualified Spec.Shutdown
import qualified System.Environment as E
import qualified Test.Tasty as T
import qualified Test.Tasty.Ingredients as T
-- import qualified Test.Util as H
import qualified Test.Tasty.Hedgehog as H

tests :: IO TestTree
tests = do
  pure $ T.testGroup "test/Spec.hs"
    [ T.testGroup "Spec"
      [ -- TODO: Move to plutus-apps
        -- Fails to meet deadline on MacOS for an unknown reason
     --    H.ignoreOnMacAndWindows "Plutus.Direct.CertifyingAndWithdrawingPlutus" Spec.Plutus.Direct.CertifyingAndWithdrawingPlutus.hprop_plutus_certifying_withdrawing
     --  , H.testProperty "Plutus.Direct.TxInLockingPlutus" Spec.Plutus.Direct.TxInLockingPlutus.hprop_plutus
     --    -- This hangs on Windows for an unknown reason
     --  , H.ignoreOnWindows "Plutus.Script.TxInLockingPlutus" Spec.Plutus.Script.TxInLockingPlutus.hprop_plutus
     --  , H.testProperty "Plutus.SubmitApi.TxInLockingPlutus" Spec.Plutus.SubmitApi.TxInLockingPlutus.hprop_plutus
     --  , H.ignoreOnWindows "Plutus.Direct.ScriptContextEquality"  Spec.Plutus.Direct.ScriptContextEquality.hprop_plutus_script_context_equality
     --  , H.ignoreOnWindows "Plutus.Direct.ScriptContextEqualityMint" Spec.Plutus.Direct.ScriptContextEqualityMint.hprop_plutus_script_context_mint_equality
        -- There is a blocking call on Windows that prevents graceful shutdown and we currently aren't testing the shutdown IPC flag.
        H.testProperty "Shutdown" Spec.Shutdown.hprop_shutdown
      ]
    ]

ingredients :: [T.Ingredient]
ingredients = T.defaultIngredients

main :: IO ()
main = do
  args <- E.getArgs

  E.withArgs args $ tests >>= T.defaultMainWithIngredients ingredients
