{-# LANGUAGE FlexibleInstances #-}

module Main
  ( main
  ) where

import           Data.String
import           Prelude
import           System.IO (BufferMode (LineBuffering), hSetBuffering, hSetEncoding, stdout, utf8)
import           Test.Tasty (TestTree)

import qualified System.Environment as E
import qualified Test.Tasty as T
import qualified Test.Tasty.Hedgehog as H
import qualified Test.Tasty.Ingredients as T

import qualified Test.Golden.Testnet.Config
import qualified Test.Golden.Testnet.Help

tests :: IO TestTree
tests = pure $ T.testGroup "Golden tests"
  [ H.testPropertyNamed "golden_DefaultConfig" (fromString "golden_DefaultConfig") Test.Golden.Testnet.Config.goldenDefaultConfigYaml
  , H.testPropertyNamed "golden_HelpAll" (fromString "golden_HelpAll") Test.Golden.Testnet.Help.golden_HelpAll
  , H.testPropertyNamed "golden_HelpCmds" (fromString "golden_HelpCmds") Test.Golden.Testnet.Help.golden_HelpCmds
  ]

ingredients :: [T.Ingredient]
ingredients = T.defaultIngredients

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  args <- E.getArgs

  E.withArgs args $ tests >>= T.defaultMainWithIngredients ingredients
