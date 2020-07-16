
import           Cardano.Prelude

import           Hedgehog.Main (defaultMain)
import           System.IO (BufferMode (..))
import qualified System.IO as IO

import qualified Test.CLI.Shelley.Tests
import qualified Test.ITN
import qualified Test.Pioneers.Exercise1
import qualified Test.Pioneers.Exercise2
import qualified Test.Pioneers.Exercise3
import qualified Test.Pioneers.Exercise4

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout LineBuffering
  IO.hSetBuffering IO.stderr LineBuffering

  defaultMain [ Test.CLI.Shelley.Tests.keyTests
              , Test.CLI.Shelley.Tests.certificateTests
              , Test.CLI.Shelley.Tests.metaDatatests
              , Test.CLI.Shelley.Tests.txTests

              , Test.Pioneers.Exercise1.tests
              , Test.Pioneers.Exercise2.tests
              , Test.Pioneers.Exercise3.tests
              , Test.Pioneers.Exercise4.tests

              , Test.ITN.tests
              ]
