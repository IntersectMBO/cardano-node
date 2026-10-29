{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations -Wno-missed-specialisations #-}
module PlutusV4Test
  ( plutusV4Tests
  ) where

import           Prelude

import           Cardano.Api (ExecutionUnits (..), ScriptData (ScriptDataNumber), ScriptInAnyLang,
                   unsafeHashableScriptData)
import           Cardano.Benchmarking.Script.Aeson (readProtocolParametersFile)
import           Cardano.TxGenerator.ProtocolParameters (ProtocolParameters)
import           Cardano.TxGenerator.Setup.Plutus (preExecutePlutusScript, readPlutusScript)
import           Cardano.TxGenerator.Types (TxGenError)

import           System.FilePath ((</>))
import           Test.Tasty
import           Test.Tasty.HUnit


-- | LoopV4 counts its redeemer down to 1,000,000, and fails below it. Only a
-- redeemer read from the right field of the V4 script context, which is
-- encoded as a list, makes it succeed for some values and fail for others.
plutusV4Tests :: TestTree
plutusV4Tests = testGroup "Plutus V4"
  [ testCase "LoopV4 reads the redeemer from the V4 script context" $ do
      (pparams, script) <- loopV4
      ExecutionUnits steps0 memory0 <- expectRight $ run pparams script 1_000_000
      ExecutionUnits steps1 memory1 <- expectRight $ run pparams script 1_000_100
      assertBool ("100 loops cost no more than none: " ++ show ((steps0, memory0), (steps1, memory1))) $
        steps1 > steps0 && memory1 > memory0
      case run pparams script 999_999 of
        Left _  -> pure ()
        Right u -> assertFailure $ "a redeemer below 1,000,000 succeeded: " ++ show u
  ]

loopV4 :: IO (ProtocolParameters, ScriptInAnyLang)
loopV4 = do
  pparams <- readProtocolParametersFile ("data" </> "protocol-parameters-v12.json")
  script <- readPlutusScript (Right $ "scripts-fallback" </> "LoopV4.plutus") >>= either (assertFailure . show) (pure . fst)
  pure (pparams, script)

run :: ProtocolParameters -> ScriptInAnyLang -> Integer -> Either TxGenError ExecutionUnits
run pparams script n =
  preExecutePlutusScript pparams script (ScriptDataNumber 0) (unsafeHashableScriptData $ ScriptDataNumber n)

expectRight :: Show e => Either e a -> IO a
expectRight = either (assertFailure . show) pure
