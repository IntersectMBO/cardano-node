module Main
  ( main
  ) where

import           Hedgehog.Main (defaultMain)
import           System.IO (IO)

import qualified Test.Cardano.Node.Chairman.Byron
import qualified Test.Cardano.Node.Chairman.Shelley
import qualified Test.Common.NetworkSpec

main :: IO ()
main = defaultMain
    [ Test.Cardano.Node.Chairman.Byron.tests
    , Test.Cardano.Node.Chairman.Shelley.tests
    , Test.Common.NetworkSpec.tests
    ]
