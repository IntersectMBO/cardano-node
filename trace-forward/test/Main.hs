module Main
  ( main
  ) where

import           Test.Tasty

import qualified Test.Trace.Forward.Protocol.DataPoint.Tests as DataPoint
import qualified Test.Trace.Forward.Protocol.TraceObject.Tests as TraceObject

main :: IO ()
main = defaultMain $
  testGroup "trace-forward"
    [ DataPoint.tests
    , TraceObject.tests
    ]
