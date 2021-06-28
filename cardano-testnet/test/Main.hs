{-# LANGUAGE FlexibleInstances #-}

module Main
  ( main
  ) where

import Data.Function ((&))
import Prelude

import qualified Data.List as L
import qualified System.Environment as E
import qualified System.IO.Unsafe as IO
import qualified Test.Tasty as T
import qualified Test.Tasty.ExpectedFailure as T
import qualified Test.Tasty.Ingredients as T
import qualified Test.Tasty.Hedgehog as H

import qualified Spec.Plutus

inNix :: Bool
inNix = IO.unsafePerformIO $ do
  L.elem "NIX_STORE" . fmap fst <$> E.getEnvironment

ignoreInNix :: T.TestTree -> T.TestTree
ignoreInNix tree = if inNix
  then T.ignoreTestBecause "Disabled in Nix" tree
  else tree

tests :: IO T.TestTree
tests = do
  pure $ T.testGroup "test/Spec.hs"
    [ T.testGroup "Spec"
      [ H.testProperty "Plutus" Spec.Plutus.hprop_plutus & ignoreInNix
      ]
    ]

ingredients :: [T.Ingredient]
ingredients = T.defaultIngredients

main :: IO ()
main = do
  args <- E.getArgs

  E.withArgs args $ tests >>= T.defaultMainWithIngredients ingredients
