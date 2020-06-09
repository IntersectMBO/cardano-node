
import           Cardano.Prelude

import           Hedgehog.Main (defaultMain)
import           System.IO (BufferMode (..))
import qualified System.IO as IO

import qualified Test.Pioneers.Exercise1
import qualified Test.Pioneers.Exercise2

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout LineBuffering
  IO.hSetBuffering IO.stderr LineBuffering

  defaultMain [ Test.Pioneers.Exercise1.tests
              , Test.Pioneers.Exercise2.tests
              ]
