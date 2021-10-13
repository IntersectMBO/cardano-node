module Main (main) where

import           Test.Tasty

import qualified Test.DataPoint.Forward.Protocol.Tests as Protocol
import qualified Test.DataPoint.Forward.Demo.Tests as Demo

main :: IO ()
main = defaultMain $ testGroup "datapoint-forward"
  [ Protocol.tests
  , Demo.tests
  ]
