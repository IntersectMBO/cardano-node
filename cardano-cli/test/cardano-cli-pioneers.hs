
import           Cardano.Prelude

import           Hedgehog.Main (defaultMain)
import           System.IO (BufferMode (..))
import qualified System.IO as IO

import qualified Test.CLI.Shelley.TextEnvelope.Golden.Tests
import qualified Test.Pioneers.Exercise1
import qualified Test.Pioneers.Exercise2
import qualified Test.Pioneers.Exercise3
import qualified Test.Pioneers.Exercise4
import qualified Test.ITN

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout LineBuffering
  IO.hSetBuffering IO.stderr LineBuffering

  defaultMain [ Test.CLI.Shelley.TextEnvelope.Golden.Tests.tests
              , Test.Pioneers.Exercise1.tests
              , Test.Pioneers.Exercise2.tests
              , Test.Pioneers.Exercise3.tests
              , Test.Pioneers.Exercise4.tests
              , Test.ITN.tests
              ]
