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
           , "goodConfig.yaml")
         , (  [ "Config namespace error: Illegal namespace ChainDB.CopyToImmutableDBEvent2.CopiedBlockToImmutableDB"
              , "Config namespace error: Illegal namespace SubscriptionDNS"
              ]
           , "badConfig.yaml")
           -- TODO: add mainnet config as good
         ]
  where
    test (actualValue, goldenBaseName) =
        (PropertyName goldenBaseName, goldenTestJSON actualValue goldenBaseName)


goldenTestJSON :: [Text] -> FilePath -> Property
goldenTestJSON expectedOutcome goldenFileBaseName =
    H.withTests 1 $ H.withShrinks 0 $ H.property $ do
      goldenFp    <- H.Base.note $ addPrefix goldenFileBaseName
      actualValue <- liftIO $ checkNodeTraceConfiguration goldenFp
      actualValue H.=== expectedOutcome


-- | NB: this function is only used in 'goldenTestJSON' but it is defined at the
-- top level so that we can refer to it in the documentation of this module.
addPrefix :: FilePath -> FilePath
addPrefix fname = "test/Test/Cardano/Tracing/NewTracing/data/" <> fname
