{-# LANGUAGE FlexibleInstances #-}

module Main
  ( main
  ) where

import Prelude

import qualified System.Environment as E
import qualified Test.Tasty as T
import qualified Test.Tasty.Ingredients as T
import qualified Test.Tasty.Hedgehog as H

import qualified Spec.Plutus

tests :: IO T.TestTree
tests = do
  pure $ T.testGroup "test/Spec.hs"
    [ T.testGroup "Spec"
      [ H.testProperty "Plutus" Spec.Plutus.hprop_plutus
      ]
    ]

ingredients :: [T.Ingredient]
ingredients = T.defaultIngredients

main :: IO ()
main = do
  args <- E.getArgs

  E.withArgs args $ tests >>= T.defaultMainWithIngredients ingredients
