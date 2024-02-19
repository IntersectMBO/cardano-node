{-# LANGUAGE FlexibleInstances #-}

module Main
  ( main
  ) where

import qualified Cardano.Crypto.Init as Crypto

import           Prelude

import           Data.String (IsString (..))
import qualified System.Environment as E
import           System.IO (BufferMode (LineBuffering), hSetBuffering, hSetEncoding, stdout, utf8)

import qualified Test.Tasty as T
import qualified Test.Tasty.Hedgehog as H
import qualified Test.Tasty.Ingredients as T

import qualified Spec.Network

tests :: IO T.TestTree
tests = do
  let t0 = H.testPropertyNamed "isPortOpen False" (fromString "isPortOpen False") Spec.Network.hprop_isPortOpen_False
  let t1 = H.testPropertyNamed "isPortOpen True"  (fromString "isPortOpen True" ) Spec.Network.hprop_isPortOpen_True
  -- TODO: Conway broken in conway
  -- let t2 = H.testPropertyNamed "chairman"         (fromString "chairman"        ) Spec.Chairman.Cardano.hprop_chairman

  pure $ T.testGroup "test/Spec.hs"
    [ T.testGroup "Spec"
      [ T.testGroup "Chairman"
        [ T.testGroup "Cardano" [] -- [t2]
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
