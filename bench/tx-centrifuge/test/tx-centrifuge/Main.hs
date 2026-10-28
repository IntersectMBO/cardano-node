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
import Test.TxCentrifuge.SupervisionTest qualified as SupervisionTest
import Test.TxCentrifuge.SizeTest qualified as SizeTest

main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "tx-centrifuge"
  [ TxTest.txTests
  , SupervisionTest.tests
  , SizeTest.tests
  ]
