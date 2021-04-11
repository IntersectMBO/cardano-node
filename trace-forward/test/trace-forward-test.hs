import           Test.Tasty

import qualified Trace.Forward.Test.Demo as Test.Demo
import qualified Trace.Forward.Test.Codec as Test.Codec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "trace-forward"
    [ Test.Demo.tests
    , Test.Codec.tests
    ]
