import           Cardano.Prelude

import           Hedgehog.Main (defaultMain)

import qualified Test.Analysis.CDF
import qualified Test.Unlog.LogObjectDB

main :: IO ()
main =
  defaultMain
    [ Test.Analysis.CDF.tests
    , Test.Unlog.LogObjectDB.tests
    ]
