import           Cardano.Prelude

import           Hedgehog.Main (defaultMain)

import qualified Test.Analysis.CDF
import qualified Test.Unlog.Org

main :: IO ()
main =
  defaultMain
    [ Test.Analysis.CDF.tests
    , Test.Unlog.Org.tests
    ]
