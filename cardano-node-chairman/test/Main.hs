{-# LANGUAGE FlexibleInstances #-}

module Main
  ( main
  ) where

import qualified Cardano.Crypto.Init as Crypto

import           Prelude

import qualified System.Environment as E
import           System.IO (BufferMode (LineBuffering), hSetBuffering, hSetEncoding, stdout, utf8)

import           Testnet.Property.Run (ignoreOnWindows)

import qualified Test.Tasty as T
import qualified Test.Tasty.Ingredients as T

import qualified Spec.Chairman.Cardano
import qualified Spec.Network

tests :: IO T.TestTree
tests = do
  let t0 = ignoreOnWindows "isPortOpen False" Spec.Network.hprop_isPortOpen_False
  let t1 = ignoreOnWindows "isPortOpen True"  Spec.Network.hprop_isPortOpen_True
  let t2 = ignoreOnWindows "chairman"         Spec.Chairman.Cardano.hprop_chairman

  pure $ T.testGroup "test/Spec.hs"
    [ T.testGroup "Spec"
      [ T.testGroup "Chairman"
        [ T.testGroup "Cardano" [t2]
        ]
      , T.testGroup "Network" [t0, t1]
      ]
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
