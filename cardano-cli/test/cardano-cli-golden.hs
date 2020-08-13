import           Cardano.Prelude

import           Hedgehog.Main (defaultMain)

import qualified Test.Golden.Shelley

main :: IO ()
main =
  defaultMain
    [ Test.Golden.Shelley.keyTests
    , Test.Golden.Shelley.certificateTests
    , Test.Golden.Shelley.metaDatatests
    , Test.Golden.Shelley.multiSigTests
    , Test.Golden.Shelley.txTests
    ]
