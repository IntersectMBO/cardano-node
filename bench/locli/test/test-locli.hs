import           Cardano.Prelude

import           Hedgehog.Main (defaultMain)

import qualified Test.Analysis.BlockProp
import qualified Test.Analysis.CDF
import qualified Test.Analysis.Compat
import qualified Test.Analysis.MachPerf
import qualified Test.Analysis.Metrics
import qualified Test.Report
import qualified Test.Typst
import qualified Test.Unlog.LogObjectDB

main :: IO ()
main =
  defaultMain
    [ Test.Analysis.BlockProp.tests
    , Test.Analysis.CDF.tests
    , Test.Analysis.Compat.tests
    , Test.Analysis.MachPerf.tests
    , Test.Analysis.Metrics.tests
    , Test.Report.tests
    , Test.Typst.tests
    , Test.Unlog.LogObjectDB.tests
    ]
