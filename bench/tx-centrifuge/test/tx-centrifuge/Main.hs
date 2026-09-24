{-# LANGUAGE ImportQualifiedPost #-}

module Main where

-----------
-- tasty --
-----------
import Test.Tasty qualified as Tasty
---------------------
-- tx-centrifuge --
---------------------
import Test.TxCentrifuge.TxTest qualified as TxTest

main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "tx-centrifuge"
  [ TxTest.txTests
  ]
