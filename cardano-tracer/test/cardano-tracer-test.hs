import           System.Environment (setEnv, unsetEnv)
import           Test.Tasty

import qualified Cardano.Tracer.Test.Logs.Tests as Logs
import qualified Cardano.Tracer.Test.DataPoint.Tests as DataPoint
import qualified Cardano.Tracer.Test.Restart.Tests as Restart
import qualified Cardano.Tracer.Test.Queue.Tests as Queue

main :: IO ()
main = do
  setEnv tastyNumThreads "1" -- For sequential running of tests (because of Windows).
  defaultMain $ testGroup "cardano-tracer"
    [ Logs.tests
    , DataPoint.tests
    , Restart.tests
    , Queue.tests
    ]
  unsetEnv tastyNumThreads
 where
  tastyNumThreads = "TASTY_NUM_THREADS"
