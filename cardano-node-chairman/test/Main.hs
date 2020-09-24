module Main
  ( main
  ) where

import           Control.Monad
import           System.IO (IO)

import qualified Test.Cardano.Node.Chairman.Shelley

main :: IO ()
main = void Test.Cardano.Node.Chairman.Shelley.tests
