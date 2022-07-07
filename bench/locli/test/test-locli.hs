import           Cardano.Prelude

import           Hedgehog.Main (defaultMain)

import qualified Test.Analysis.CDF

main :: IO ()
main =
  defaultMain
    [ Test.Analysis.CDF.tests
    ]
