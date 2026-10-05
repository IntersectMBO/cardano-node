{-# LANGUAGE FlexibleContexts #-}


-- | Check namespace consistencies agains configurations
module Test.Cardano.Tracing.NewTracing.Consistency (tests) where

import           Cardano.Node.Tracing.Consistency (checkNodeTraceConfigurationWith)
import           Cardano.Node.Tracing.Documentation (DocRun (..), docTracersFirstPhase)

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.Directory as IO
import           System.FilePath ((</>))

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Process as H
import           Hedgehog.Internal.Property (PropertyName (PropertyName))

import           Hermod.Tracing.DocuGenerator (docuResultsToNamespaces)



tests :: MonadIO m => m Bool
tests = do
  -- The documentation pass is comparatively expensive; run it once and share
  -- the result between all properties.
  run <- liftIO $ docTracersFirstPhase Nothing
  H.checkSequential
      $ H.Group "Configuration Consistency tests"
      $ Prelude.map (test run)
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
                 ]
              , testSubdir
              , "badConfig.yaml"
              )
            ]
        <> [ ( PropertyName "namespace inventory matches documented tracers"
             , prop_namespaceInventory run
             )
           , ( PropertyName "bench/trace-schemas/newNamespaces.txt is current"
             , prop_namespaceListCurrent run
             )
           ]
  where
    test run (actualValue, subDir, goldenBaseName) =
        (PropertyName goldenBaseName, goldenTestJSON run subDir actualValue goldenBaseName)

goldenTestJSON :: DocRun -> SubdirSelection -> [Text] -> FilePath -> Property
goldenTestJSON run subDir expectedOutcome goldenFileBaseName =
  H.withTests 1 $ H.withShrinks 0 $ H.property $ do
    base          <- resolveDir
    goldenFp      <- H.note $ base </> goldenFileBaseName
    actualValue   <- H.evalIO $ checkNodeTraceConfigurationWith run goldenFp
    actualValue H.=== expectedOutcome
  where
    resolveDir = case subDir of
      ExternalSubdir d -> do
        base <- projectBase
        pure $ base </> d
      InternalSubdir d ->
        pure d

projectBase :: (H.MonadTest m, MonadIO m) => m FilePath
projectBase = H.evalIO . IO.canonicalizePath =<< H.getProjectBase

-- | Namespaces (or namespace prefixes) that are documented but not part of
-- the configuration consistency check. Every declared tracer is both, unless
-- it is declared otherwise in "Cardano.Node.Tracing.Tracers"; the property
-- fails on stale entries.
knownUnchecked :: [Text]
knownUnchecked = []

-- | Namespaces (or namespace prefixes) that are checked but not documented:
-- tracers declared @undocumented@ in "Cardano.Node.Tracing.Tracers" because
-- their 'MetaTrace' instances in ouroboros-network lack severities. Remove
-- entries as the instances get fixed; the property fails on stale entries.
knownUndocumented :: [Text]
knownUndocumented =
  [ "Net.DNS"
  , "Net.Mux.Local.Bearer"
  , "Net.Mux.Remote.Bearer"
  ]

-- | Compare the namespace inventory of the configuration consistency check
-- against the namespaces of the documented tracers. Metrics are not part of
-- the comparison; datapoint namespaces are.
--
-- Returns @(documentedButNotChecked, checkedButNotDocumented)@, both sorted.
namespaceInventoryDiff :: DocRun -> ([T.Text], [T.Text])
namespaceInventoryDiff run =
    ( Set.toAscList (documented `Set.difference` checked)
    , Set.toAscList (checked `Set.difference` documented) )
  where
    documented = Set.fromList (T.lines (docuResultsToNamespaces (drDocTracer run)))
    checked    = Set.fromList
                   [ T.intercalate "." (outer <> inner)
                   | (outer, inner) <- drNamespaces run ]

-- | Both inventories now come from the one declaration of the tracers, so
-- they differ exactly by the tracers declared as undocumented or unchecked
-- there; this property keeps those exceptions explicit and checks that
-- hermod's documentation of a tracer lists the same namespaces as its
-- 'allNamespaces'.
prop_namespaceInventory :: DocRun -> Property
prop_namespaceInventory run =
  H.withTests 1 $ H.withShrinks 0 $ H.property $ do
    let (documentedNotChecked, checkedNotDocumented) = namespaceInventoryDiff run
    H.annotate "Namespaces documented but unknown to the consistency check:"
    withoutKnown knownUnchecked documentedNotChecked H.=== []
    H.annotate "Namespaces in the consistency check but never documented \
               \(a tracer declared undocumented in Cardano.Node.Tracing.Tracers?):"
    withoutKnown knownUndocumented checkedNotDocumented H.=== []
    H.annotate "Stale allowlist entries no longer matching any discrepancy \
               \(delete them from this test):"
    staleEntries knownUnchecked documentedNotChecked
      <> staleEntries knownUndocumented checkedNotDocumented H.=== []
  where
    covers :: Text -> Text -> Bool
    covers prefix ns = prefix == ns || (prefix <> ".") `T.isPrefixOf` ns
    withoutKnown :: [Text] -> [Text] -> [Text]
    withoutKnown allowed =
      Prelude.filter (\ns -> not (Prelude.any (`covers` ns) allowed))
    staleEntries :: [Text] -> [Text] -> [Text]
    staleEntries allowed diffs =
      Prelude.filter (\prefix -> not (Prelude.any (covers prefix) diffs)) allowed

-- | The tracked namespace list the trace schemas are generated from must be
-- the one the current tracers produce.
prop_namespaceListCurrent :: DocRun -> Property
prop_namespaceListCurrent run =
  H.withTests 1 $ H.withShrinks 0 $ H.property $ do
    base <- projectBase
    let listFp = base </> "bench" </> "trace-schemas" </> "newNamespaces.txt"
    tracked <- H.evalIO $ T.readFile listFp
    H.annotate "Regenerate with: cardano-node trace-documentation \
               \--config configuration/cardano/mainnet-config.yaml \
               \--output-file /dev/null \
               \--output-namespace-list bench/trace-schemas/newNamespaces.txt"
    T.lines tracked H.=== T.lines (docuResultsToNamespaces (drDocTracer run))

data SubdirSelection =
    InternalSubdir  FilePath
  | ExternalSubdir  FilePath

testSubdir, configSubdir :: SubdirSelection
testSubdir    = InternalSubdir "test/Test/Cardano/Tracing/NewTracing/data"
configSubdir  = ExternalSubdir $ "configuration" </> "cardano"
