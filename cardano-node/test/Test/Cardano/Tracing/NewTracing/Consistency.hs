{-# LANGUAGE FlexibleContexts #-}


-- | Check namespace consistencies agains configurations
module Test.Cardano.Tracing.NewTracing.Consistency (tests) where

import           Cardano.Node.Tracing.Consistency (checkNodeTraceConfiguration)

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text
import           System.Directory (canonicalizePath)
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
            [ (  []
                  -- This file name shoud reference the current standard config with new tracing
              , "mainnet-config-new-tracing.json"
              , configPrefix)
            , (  []
              , "goodConfig.yaml"
              , testPrefix)
            , (  [ "Config namespace error: Illegal namespace ChainDB.CopyToImmutableDBEvent2.CopiedBlockToImmutableDB"
                  , "Config namespace error: Illegal namespace SubscriptionDNS"
                  ]
              , "badConfig.yaml"
              , testPrefix)
            ]
  where
    test (actualValue, goldenBaseName, prefix) =
        (PropertyName goldenBaseName, goldenTestJSON actualValue goldenBaseName prefix)


goldenTestJSON :: [Text] -> FilePath -> (FilePath -> IO FilePath) -> Property
goldenTestJSON expectedOutcome goldenFileBaseName prefixFunc =
    H.withTests 1 $ H.withShrinks 0 $ H.property $ do
      basePath <- H.getProjectBase
      prefixPath <- liftIO $ prefixFunc basePath
      goldenFp    <- H.note $ prefixPath </> goldenFileBaseName
      actualValue <- liftIO $ checkNodeTraceConfiguration goldenFp
      actualValue H.=== expectedOutcome


configPrefix :: FilePath -> IO FilePath
configPrefix projectBase = do
  base <- canonicalizePath projectBase
  return $ base </> "configuration/cardano"

testPrefix :: FilePath ->  IO FilePath
testPrefix _ = pure "test/Test/Cardano/Tracing/NewTracing/data/"
