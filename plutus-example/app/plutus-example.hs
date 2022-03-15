
import           Prelude

import           Cardano.Api

import           System.Directory
import           System.FilePath.Posix ((</>))

import           Cardano.PlutusExample.AlwaysFails (alwaysFailsScript)
import           Cardano.PlutusExample.AlwaysSucceeds (alwaysSucceedsScript)
import           Cardano.PlutusExample.CustomDatumRedeemerGuess
import           Cardano.PlutusExample.DatumRedeemerGuess (guessScript, guessScriptStake)
import           Cardano.PlutusExample.MintingScript (apiExamplePlutusMintingScript)
import           Cardano.PlutusExample.ScriptContextChecker
import           Cardano.PlutusExample.Sum (sumScript)
import           Cardano.PlutusExample.Loop (loopScript)

main :: IO ()
main = do
  let dir = "generated-plutus-scripts"
  createDirectory dir

  _ <- writeFileTextEnvelope (dir </> "always-fails.plutus") Nothing alwaysFailsScript
  _ <- writeFileTextEnvelope (dir </> "always-succeeds-spending.plutus") Nothing alwaysSucceedsScript
  _ <- writeFileTextEnvelope (dir </> "guess-42-datum-42-txin.plutus") Nothing guessScript
  _ <- writeFileTextEnvelope (dir </> "guess-42-stake.plutus") Nothing guessScriptStake
  _ <- writeFileTextEnvelope (dir </> "custom-guess-42-datum-42.plutus") Nothing customGuessScript
  _ <- writeFileTextEnvelope (dir </> "anyone-can-mint.plutus") Nothing apiExamplePlutusMintingScript
  _ <- writeFileTextEnvelope (dir </> "sum.plutus") Nothing sumScript
  _ <- writeFileTextEnvelope (dir </> "loop.plutus") Nothing loopScript
  _ <- writeFileTextEnvelope (dir </> "context-equivalance-test.plutus") Nothing scriptContextCheckScript
  _ <- writeFileTextEnvelope (dir </> "minting-context-equivalance-test.plutus") Nothing customApiExamplePlutusMintingScript
  return ()
