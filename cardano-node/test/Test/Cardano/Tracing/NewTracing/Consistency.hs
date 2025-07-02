{-# LANGUAGE FlexibleContexts #-}


-- | Check namespace consistencies agains configurations
module Test.Cardano.Tracing.NewTracing.Consistency (tests) where

import           Cardano.Node.Tracing.Consistency (checkNodeTraceConfiguration)

import           Control.Monad.IO.Class (MonadIO)
import           Data.Text
import qualified System.Directory as IO
import           System.FilePath ((</>))

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Process as H
import           Hedgehog.Internal.Property (PropertyName (PropertyName))

tests :: MonadIO m => m Bool
tests = do
  H.checkSequential
      $ H.Group "Configuration Consistency tests"
      $ Prelude.map test
            [ ( []
              -- This file name should reference the current standard config with new tracing
              , configSubdir
              , "mainnet-config.json"
              )
              ,
              (  []
              , testSubdir
              , "goodConfig.yaml"
              )
            , (  [ "Config namespace error: Illegal namespace ChainDB.CopyToImmutableDBEvent2.CopiedBlockToImmutableDB"
                  , "Config namespace error: Illegal namespace SubscriptionDNS"
                  ]
              , testSubdir
              , "badConfig.yaml"
              )
            ]
  where
    test (actualValue, subDir, goldenBaseName) =
        (PropertyName goldenBaseName, goldenTestJSON subDir actualValue goldenBaseName)

goldenTestJSON :: SubdirSelection -> [Text] -> FilePath -> Property
goldenTestJSON subDir expectedOutcome goldenFileBaseName =
  H.withTests 1 $ H.withShrinks 0 $ H.property $ do
    base          <- resolveDir
    goldenFp      <- H.note $ base </> goldenFileBaseName
    actualValue   <- H.evalIO $ checkNodeTraceConfiguration goldenFp
    actualValue H.=== expectedOutcome
  where
    resolveDir = case subDir of
      ExternalSubdir d -> do
        base <- H.evalIO . IO.canonicalizePath =<< H.getProjectBase
        pure $ base </> d
      InternalSubdir d ->
        pure d

data SubdirSelection =
    InternalSubdir  FilePath
  | ExternalSubdir  FilePath

testSubdir, configSubdir :: SubdirSelection
testSubdir    = InternalSubdir "test/Test/Cardano/Tracing/NewTracing/data"
configSubdir  = ExternalSubdir $ "configuration" </> "cardano"
