import           Cardano.Prelude

import           Hedgehog.Main (defaultMain)
import           Test.PlutusExample.Plutus
import           Test.PlutusExample.ScriptData

main :: IO ()
main =
  defaultMain
    [ Test.PlutusExample.Plutus.tests
    , Test.PlutusExample.ScriptData.tests
    ]
