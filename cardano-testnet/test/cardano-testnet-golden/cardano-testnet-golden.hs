{-# LANGUAGE FlexibleInstances #-}

module Main
  ( main
  ) where

import qualified Cardano.Crypto.Init as Crypto
import qualified Cardano.Testnet.Test.Golden.Config
import qualified Cardano.Testnet.Test.Golden.Help
import qualified Cardano.Testnet.Test.Golden.Manifest
import qualified Cardano.Testnet.Test.Golden.Status

import           Prelude

import           Data.String
import qualified System.Environment as E
import           System.IO (BufferMode (LineBuffering), hSetBuffering, hSetEncoding, stdout, utf8)

import           Testnet.Property.Run (ignoreOnWindows)

import qualified Test.Tasty as T
import           Test.Tasty (TestTree)
import qualified Test.Tasty.Hedgehog as H
import qualified Test.Tasty.Ingredients as T

tests :: IO TestTree
tests = pure $ T.testGroup "Golden tests"
  [ H.testPropertyNamed "golden_DefaultConfig" (fromString "golden_DefaultConfig") Cardano.Testnet.Test.Golden.Config.goldenDefaultConfigYaml
  , H.testPropertyNamed "golden_HelpAll" (fromString "golden_HelpAll") Cardano.Testnet.Test.Golden.Help.golden_HelpAll
  , H.testPropertyNamed "golden_HelpCmds" (fromString "golden_HelpCmds") Cardano.Testnet.Test.Golden.Help.golden_HelpCmds
  , ignoreOnWindows "golden_HelpReadme" Cardano.Testnet.Test.Golden.Help.golden_HelpReadme
  , H.testPropertyNamed "golden_VersionCmd" (fromString "golden_VersionCmd") Cardano.Testnet.Test.Golden.Help.golden_VersionCmd
  , H.testPropertyNamed "golden_Manifest" (fromString "golden_Manifest") Cardano.Testnet.Test.Golden.Manifest.golden_Manifest
  , H.testPropertyNamed "golden_StatusJson" (fromString "golden_StatusJson") Cardano.Testnet.Test.Golden.Status.golden_StatusJson
  , H.testPropertyNamed "golden_StatusTable" (fromString "golden_StatusTable") Cardano.Testnet.Test.Golden.Status.golden_StatusTable
  , H.testPropertyNamed "golden_StatusCommand" (fromString "golden_StatusCommand") Cardano.Testnet.Test.Golden.Status.golden_StatusCommand
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
