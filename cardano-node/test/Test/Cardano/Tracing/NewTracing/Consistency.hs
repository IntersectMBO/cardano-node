{-# LANGUAGE FlexibleContexts #-}


-- | Check namespace consistencies agains configurations
module Test.Cardano.Tracing.NewTracing.Consistency (tests) where

import           Cardano.Node.Tracing.Consistency (checkNodeTraceConfiguration)

import           Control.Monad.IO.Class (MonadIO, liftIO)
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
        (PropertyName goldenBaseName, goldenTestJSON actualValue subDir goldenBaseName)

goldenTestJSON :: [Text] -> SubdirSelection -> FilePath -> Property
goldenTestJSON expectedOutcome subDir goldenFileBaseName =
    H.withTests 1 $ H.withShrinks 0 $ H.property $ do
      base        <- resolveDir subDir
      goldenFp    <- H.note $ base </> goldenFileBaseName
      actualValue <- liftIO $ checkNodeTraceConfiguration goldenFp
      actualValue H.=== expectedOutcome
  where
      resolveDir (ExternalSubdir d) = do
        base <- H.evalIO . IO.canonicalizePath =<< H.getProjectBase
        pure $ base </> d
      resolveDir (InternalSubdir d) =
        pure d

data SubdirSelection =
    InternalSubdir  FilePath
  | ExternalSubdir  FilePath

testSubdir, configSubdir :: SubdirSelection
testSubdir    = InternalSubdir "test/Test/Cardano/Tracing/NewTracing/data"
configSubdir  = ExternalSubdir "configuration/cardano"
