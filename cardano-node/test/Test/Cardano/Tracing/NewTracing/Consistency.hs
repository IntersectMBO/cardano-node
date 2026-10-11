{-# LANGUAGE FlexibleContexts #-}


-- | Check namespace consistencies agains configurations
module Test.Cardano.Tracing.NewTracing.Consistency (tests) where

import           Cardano.Node.Tracing.Consistency (DocTracer, checkNodeTraceConfigurationWith, getAllNamespaces)
import           Cardano.Node.Tracing.Documentation (docTracersFirstPhase)

import           Cardano.Logging.DocuGenerator (docuResultsToNamespaces)

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as Set

import qualified System.Directory as IO
import           System.FilePath ((</>))

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Process as H
import           Hedgehog.Internal.Property (PropertyName (PropertyName))



tests :: MonadIO m => m Bool
tests = do
  -- The documentation pass is comparatively expensive; run it once and share
  -- the result between all properties.
  (docTracer, _) <- liftIO $ docTracersFirstPhase Nothing
  H.checkSequential
      $ H.Group "Configuration Consistency tests"
      $ Prelude.map (test docTracer)
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
             , prop_namespaceInventory docTracer
             )
           ]
  where
    test docTracer (actualValue, subDir, goldenBaseName) =
        (PropertyName goldenBaseName, goldenTestJSON docTracer subDir actualValue goldenBaseName)

goldenTestJSON :: DocTracer -> SubdirSelection -> [Text] -> FilePath -> Property
goldenTestJSON docTracer subDir expectedOutcome goldenFileBaseName =
  H.withTests 1 $ H.withShrinks 0 $ H.property $ do
    base          <- resolveDir
    goldenFp      <- H.note $ base </> goldenFileBaseName
    actualValue   <- H.evalIO $ checkNodeTraceConfigurationWith docTracer goldenFp
    actualValue H.=== expectedOutcome
  where
    resolveDir = case subDir of
      ExternalSubdir d -> do
        base <- H.evalIO . IO.canonicalizePath =<< H.getProjectBase
        pure $ base </> d
      InternalSubdir d ->
        pure d

-- | Namespaces (or namespace prefixes) documented by the doc tracers in
-- "Cardano.Node.Tracing.Documentation" but missing from 'getAllNamespaces'.
-- Pre-existing debt tracked in the follow-up to
-- <https://github.com/IntersectMBO/cardano-node/issues/6667 #6667>.
-- Remove entries as they get fixed; the property fails on stale entries.
knownUnchecked :: [Text]
knownUnchecked =
  [ "Forge.ThreadStats"
  , "NodeInfo"
  , "NodeStartupInfo"
  , "Reflection"
  ]

-- | Namespaces (or namespace prefixes) in 'getAllNamespaces' that no doc
-- tracer documents.  Pre-existing debt tracked in the follow-up to
-- <https://github.com/IntersectMBO/cardano-node/issues/6667 #6667>.
-- Remove entries as they get fixed; the property fails on stale entries.
knownUndocumented :: [Text]
knownUndocumented =
  [ "Net.DNSResolver"
  , "Net.Handshake.Local"
  , "Net.Handshake.Remote"
  , "Net.Mux.Local.Bearer"
  , "Net.Mux.Local.Channel"
  , "Net.Mux.Remote.Bearer"
  , "Net.Mux.Remote.Channel"
  ]

-- | Compare the namespace inventory of the configuration consistency check
-- ('getAllNamespaces') against the namespaces of the documented tracers
-- (the 'DocTracer' produced by 'docTracersFirstPhase').  Metrics are not part
-- of the comparison; datapoint namespaces are.
--
-- Returns @(documentedButNotChecked, checkedButNotDocumented)@, both sorted.
-- Both lists are empty exactly when the two hand-written tracer inventories
-- in "Cardano.Node.Tracing.Documentation" and this module agree.
namespaceInventoryDiff :: DocTracer -> ([T.Text], [T.Text])
namespaceInventoryDiff dt =
    ( Set.toAscList (documented `Set.difference` checked)
    , Set.toAscList (checked `Set.difference` documented) )
  where
    documented = Set.fromList (T.lines (docuResultsToNamespaces dt))
    checked    = Set.fromList
                   [ T.intercalate "." (outer <> inner)
                   | (outer, inner) <- getAllNamespaces ]

-- | The namespace inventory of the configuration consistency check
-- ('getAllNamespaces') must agree with the namespaces of the documented
-- tracers ('docTracersFirstPhase').  Both are written out by hand and are not
-- type-checked against each other, so they can drift apart silently
-- (issue#6667); this property catches any divergence between the two.
prop_namespaceInventory :: DocTracer -> Property
prop_namespaceInventory docTracer =
  H.withTests 1 $ H.withShrinks 0 $ H.property $ do
    let (documentedNotChecked, checkedNotDocumented) = namespaceInventoryDiff docTracer
    H.annotate "Namespaces documented by a doc tracer but unknown to getAllNamespaces \
               \(add an entry in Cardano.Node.Tracing.Consistency.getAllNamespaces):"
    withoutKnown knownUnchecked documentedNotChecked H.=== []
    H.annotate "Namespaces in getAllNamespaces but never documented \
               \(add a documentTracer block in Cardano.Node.Tracing.Documentation \
               \and regenerate the trace schemas):"
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

data SubdirSelection =
    InternalSubdir  FilePath
  | ExternalSubdir  FilePath

testSubdir, configSubdir :: SubdirSelection
testSubdir    = InternalSubdir "test/Test/Cardano/Tracing/NewTracing/data"
configSubdir  = ExternalSubdir $ "configuration" </> "cardano"
