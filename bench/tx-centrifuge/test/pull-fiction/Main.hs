{-# LANGUAGE ImportQualifiedPost #-}

module Main where

-----------
-- tasty --
-----------
import Test.Tasty qualified as Tasty
import Test.Tasty.Runners qualified as Tasty
---------------------
-- pull-fiction --
---------------------
import Test.PullFiction.GeneratorTest qualified as GeneratorTest
import Test.PullFiction.Harness qualified as Harness
import Test.PullFiction.PipelineTest qualified as PipelineTest

main :: IO ()
main = do
  dur <- Harness.getDuration
  Tasty.defaultMain
    $ Tasty.localOption (Tasty.NumThreads 1)
    $ Tasty.testGroup "pull-fiction"
      [ GeneratorTest.generatorTests dur
      , PipelineTest.pipelineTests dur
      ]
