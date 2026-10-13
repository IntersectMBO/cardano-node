{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations -Wno-missed-specialisations #-}
module ScriptVariantsTest
  ( scriptVariantsTests
  ) where

import           Prelude

import           Cardano.Api (ExecutionUnits (..), PlutusScript (PlutusScriptSerialised),
                   Script (PlutusScript), ScriptData (ScriptDataNumber), ScriptInAnyLang (..),
                   getScriptData, hashScript, unsafeHashableScriptData)
import           Cardano.Benchmarking.Script.Aeson (readProtocolParametersFile)
import           Cardano.TxGenerator.PlutusContext (readScriptData, scriptDataModifyNumber)
import           Cardano.TxGenerator.Setup.Plutus (preExecutePlutusScript, readPlutusScript)
import           Cardano.TxGenerator.ScriptVariants

import           Control.Monad (forM_, void)
import           Control.Monad.Except (runExcept)
import           Data.Maybe (fromMaybe)
import           Data.Set qualified as Set
import           System.FilePath ((<.>), (</>))
import           Test.Tasty
import           Test.Tasty.HUnit

import           PlutusCore.DeBruijn (FreeVariableError, fakeNameDeBruijn)
import           PlutusCore.Quote (runQuoteT)
import           PlutusLedgerApi.Common (uncheckedDeserialiseUPLC)
import           UntypedPlutusCore qualified as UPLC


-- | Every script shipped with the generator, with its redeemer file.
bundledScripts :: [String]
bundledScripts =
  [ "Loop", "Loop2024", "LoopV3"
  , "EcdsaSecp256k1Loop", "EcdsaSecp256k1LoopV3"
  , "SchnorrSecp256k1Loop", "SchnorrSecp256k1LoopV3"
  , "HashOntoG2AndAdd", "MultiScalarMulG1", "Ripemd160", "ExpModInteger"
  ]

scriptVariantsTests :: TestTree
scriptVariantsTests = testGroup "script variants"
  [ testGroup name
      [ testCase "variants have distinct hashes, the same size, and reach the target size" $ do
          base <- loadScript name
          forM_ [ScriptSizeMinimal, ScriptSizeFixed 4_096, ScriptSizeFixed 12_000] $ \size -> do
            variants <- expectRight $ scriptVariants size 20 base
            let hashes = map scriptHashOf variants
                sizes  = map sizeOf variants
            Set.size (Set.fromList $ scriptHashOf base : hashes) @?= 21
            Set.size (Set.fromList sizes) @?= 1
            case size of
              ScriptSizeFixed target -> do
                assertBool ("smaller than the target: " ++ show sizes) $ head sizes >= target
                assertBool ("too far above the target: " ++ show sizes) $ head sizes <= target + 8
              ScriptSizeMinimal ->
                assertBool "not just above the base script" $ head sizes - sizeOf base <= 16

      , testCase "a single variant is the same as in the list of variants" $ do
          base <- loadScript name
          variants <- expectRight $ scriptVariants (ScriptSizeFixed 4_096) 10 base
          variant7 <- expectRight $ scriptVariant (ScriptSizeFixed 4_096) base 7
          scriptHashOf variant7 @?= scriptHashOf (variants !! 7)

      , testCase "variants are well scoped" $ do
          base <- loadScript name
          variants <- expectRight $ scriptVariants (ScriptSizeFixed 4_096) 3 base
          forM_ variants $ \variant -> case wellScoped variant of
            Left err -> assertFailure $ "free variable: " ++ show err
            Right () -> pure ()

      , testCase "variants run like the base script, at one and the same cost" $ do
          base <- loadScript name
          pparams <- readProtocolParametersFile ("data" </> "protocol-parameters-v11.json")
          redeemerFile <- either (assertFailure . show) pure =<< readScriptData ("data" </> name <.> "redeemer.json")
          -- The loop counter as the generator's automatic budgeting sets it before running a script.
          let redeemer = unsafeHashableScriptData $ scriptDataModifyNumber (const 1_000_000) $ getScriptData redeemerFile
          variants <- expectRight $ scriptVariants (ScriptSizeFixed 4_096) 5 base
          let run script = preExecutePlutusScript pparams script (ScriptDataNumber 0) redeemer
          baseUnits <- expectRight $ run base
          variantUnits <- mapM (expectRight . run) variants
          Set.size (Set.fromList $ map unitsOf variantUnits) @?= 1
          let ExecutionUnits steps memory = head variantUnits
              ExecutionUnits baseSteps baseMemory = baseUnits
          assertBool ("variant cheaper than the base script: " ++ show (variantUnits, baseUnits)) $
            steps >= baseSteps && memory >= baseMemory
          assertBool ("variant overhead too high: " ++ show (variantUnits, baseUnits)) $
            steps - baseSteps <= 1_000_000 && memory - baseMemory <= 1_000
      ]
  | name <- bundledScripts
  ]
 where
  unitsOf (ExecutionUnits s m) = (s, m)

loadScript :: String -> IO ScriptInAnyLang
loadScript name =
  readPlutusScript (Right $ "scripts-fallback" </> name <.> "plutus") >>= either (assertFailure . show) (pure . fst)

expectRight :: Show e => Either e a -> IO a
expectRight = either (assertFailure . show) pure

scriptHashOf :: ScriptInAnyLang -> String
scriptHashOf (ScriptInAnyLang _ script) = show $ hashScript script

sizeOf :: ScriptInAnyLang -> Int
sizeOf = fromMaybe (error "not a Plutus script") . plutusScriptSize

-- | Converting de Bruijn indices back to names fails on any free variable.
wellScoped :: ScriptInAnyLang -> Either FreeVariableError ()
wellScoped (ScriptInAnyLang _ (PlutusScript _ (PlutusScriptSerialised bytes))) =
  let UPLC.Program _ _ term = uncheckedDeserialiseUPLC bytes
  in void $ runExcept (runQuoteT $ UPLC.unDeBruijnTerm $ UPLC.termMapNames fakeNameDeBruijn term)
wellScoped _ = Right ()
