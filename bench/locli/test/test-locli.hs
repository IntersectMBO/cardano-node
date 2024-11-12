import           Cardano.Prelude

import           Hedgehog.Main (defaultMain)

import qualified Test.Analysis.CDF
import qualified Test.Unlog.Org
import qualified Test.Unlog.LogObjectDB

main :: IO ()
main =
  defaultMain
    [ Test.Analysis.CDF.tests
    , Test.Unlog.Org.tests
    , Test.Unlog.LogObjectDB.tests
    ]
