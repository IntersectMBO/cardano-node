{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified TraceSchemaGen.ApplySchemaOverrides as Apply
import qualified TraceSchemaGen.CheckOverrideCoverage as Coverage
import qualified TraceSchemaGen.ValidateTraceSchemas as Validate
import Control.Exception (catch)
import Data.List (sort)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "trace-schema-gen"
    [ applySchemaOverridesTests
    , checkOverrideCoverageTests
    , validateTraceSchemasTests
    ]

applySchemaOverridesTests :: TestTree
applySchemaOverridesTests =
  testGroup "ApplySchemaOverrides"
    [ testCase "overrideRelToTarget maps nested override file" $ do
        actual <- Apply.overrideRelToTarget "messages/Foo/Bar.schema.override.json"
        actual @?= "messages/Foo/Bar.schema.json"
    , testCase "findDestructiveOps reports replacement and deletion" $ do
        let target = A.object ["keep" A..= ("x" :: String), "nested" A..= A.object ["gone" A..= (1 :: Int)]]
        let patch = A.object ["keep" A..= (2 :: Int), "nested" A..= A.object ["gone" A..= A.Null]]
        Apply.findDestructiveOps target patch
          @?= ["keep: field replacement", "nested.gone: field deletion"]
    , testCase "findDestructiveOps allows identical values" $ do
        let target = A.object ["keep" A..= ("x" :: String), "nested" A..= A.object ["same" A..= (1 :: Int)]]
        let patch = A.object ["keep" A..= ("x" :: String), "nested" A..= A.object ["same" A..= (1 :: Int)]]
        Apply.findDestructiveOps target patch @?= []
    , testCase "processOverride applies additive patch in temp tree" $
        withSystemTempDirectory "trace-schema-gen" $ \root -> do
          let target = root </> "messages" </> "Example.schema.json"
          let override = root </> "overrides" </> "messages" </> "Example.schema.override.json"
          createDirectoryIfMissing True (root </> "messages")
          createDirectoryIfMissing True (root </> "overrides" </> "messages")
          BL.writeFile target (A.encode (A.object ["existing" A..= ("value" :: String)]))
          BL.writeFile override (A.encode (A.object ["patch" A..= A.object ["added" A..= (1 :: Int)]]))

          let cfg = Apply.defaultConfig {Apply.cfgRoot = root}
          (writtenTarget, changed) <- Apply.processOverride cfg override

          writtenTarget @?= target
          assertBool "expected target to change" changed

          decoded <- A.eitherDecodeFileStrict' target
          decoded @?= Right (A.object ["existing" A..= ("value" :: String), "added" A..= (1 :: Int)])
    , testCase "processOverride rejects destructive patch by default" $
        withSystemTempDirectory "trace-schema-gen" $ \root -> do
          let target = root </> "messages" </> "Example.schema.json"
          let override = root </> "overrides" </> "messages" </> "Example.schema.override.json"
          createDirectoryIfMissing True (root </> "messages")
          createDirectoryIfMissing True (root </> "overrides" </> "messages")
          BL.writeFile target (A.encode (A.object ["existing" A..= ("value" :: String)]))
          BL.writeFile override (A.encode (A.object ["patch" A..= A.object ["existing" A..= (1 :: Int)]]))

          let cfg = Apply.defaultConfig {Apply.cfgRoot = root}
          result <- catch (Apply.processOverride cfg override >> pure Nothing) exitCodeToMaybe
          assertBool "expected processOverride to exit with failure" $
            case result of
              Just (ExitFailure _) -> True
              _ -> False
    ]

checkOverrideCoverageTests :: TestTree
checkOverrideCoverageTests =
  testGroup "CheckOverrideCoverage"
    [ testCase "isGeneratedJson matches schema outputs only" $ do
        assertBool "messages schema should match" $
          Coverage.isGeneratedJson "bench/trace-schemas/messages/Foo.schema.json"
        assertBool "types schema should match" $
          Coverage.isGeneratedJson "bench/trace-schemas/types/Foo.schema.json"
        assertBool "override file should not match" $
          not (Coverage.isGeneratedJson "bench/trace-schemas/overrides/messages/Foo.schema.override.json")
    , testCase "generatedToOverride maps generated schema path" $ do
        Coverage.generatedToOverride "bench/trace-schemas/messages/Foo.schema.json"
          @?= "bench/trace-schemas/overrides/messages/Foo.schema.override.json"
    ]

validateTraceSchemasTests :: TestTree
validateTraceSchemasTests =
  testGroup "ValidateTraceSchemas"
    [ testCase "listJsonFilesRecursive finds nested json files only" $
        withSystemTempDirectory "trace-schema-gen" $ \root -> do
          let nested = root </> "nested"
          createDirectoryIfMissing True nested
          writeFile (root </> "one.json") "{}"
          writeFile (nested </> "two.json") "{}"
          writeFile (nested </> "skip.txt") "nope"

          files <- sort <$> Validate.listJsonFilesRecursive root
          files @?= sort [root </> "one.json", nested </> "two.json"]
    , testCase "validatorArgs prepend nix check-jsonschema invocation" $ do
        Validate.validatorArgs ["--schemafile", "meta.schema.json", "msg.schema.json"]
          @?= ["run", "nixpkgs#check-jsonschema", "--", "--schemafile", "meta.schema.json", "msg.schema.json"]
    , testCase "checkInputs accepts existing files and directories" $
        withSystemTempDirectory "trace-schema-gen" $ \root -> do
          let meta = root </> "meta.schema.json"
          let messages = root </> "messages"
          writeFile meta "{}"
          createDirectoryIfMissing True messages
          Validate.checkInputs Validate.defaultConfig
            { Validate.cfgMetaSchema = meta
            , Validate.cfgMessagesDir = messages
            }
    , testCase "checkInputs fails on missing inputs" $
        withSystemTempDirectory "trace-schema-gen" $ \root -> do
          let cfg =
                Validate.defaultConfig
                  { Validate.cfgMetaSchema = root </> "missing.schema.json"
                  , Validate.cfgMessagesDir = root </> "missing-dir"
                  }
          result <- catch (Validate.checkInputs cfg >> pure Nothing) exitCodeToMaybe
          assertBool "expected checkInputs to exit with failure" $
            case result of
              Just (ExitFailure _) -> True
              _ -> False
    ]

exitCodeToMaybe :: ExitCode -> IO (Maybe ExitCode)
exitCodeToMaybe code = pure (Just code)
