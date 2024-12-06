-- | Check namespace consistencies agains configurations
module Test.Cardano.Tracing.NewTracing.Consistency (tests) where

import           Cardano.Node.Tracing.Consistency (checkNodeTraceConfiguration)

import           Control.Monad.IO.Class (liftIO)
import           Data.Text

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H.Base
import           Hedgehog.Internal.Property (PropertyName (PropertyName))

tests :: IO Bool
tests = H.checkSequential
      $ H.Group "Configuration Consistency tests"
      $ test
     <$> [ (  []
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


goldenTestJSON :: [Text] -> FilePath -> FilePath -> Property
goldenTestJSON expectedOutcome goldenFileBaseName prefix =
    H.withTests 1 $ H.withShrinks 0 $ H.property $ do
      goldenFp    <- H.Base.note $ prefix <> goldenFileBaseName
      actualValue <- liftIO $ checkNodeTraceConfiguration goldenFp
      actualValue H.=== expectedOutcome


configPrefix :: FilePath
configPrefix = "../configuration/cardano/"

testPrefix :: FilePath
testPrefix = "test/Test/Cardano/Tracing/NewTracing/data/"
