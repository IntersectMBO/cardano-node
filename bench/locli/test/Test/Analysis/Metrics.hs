{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Consistency of the metric definitions: every field a report or analysis selects
--   by name has a definition, and every CDF of the cluster performance analysis is
--   defined as a metric. A mismatch otherwise only shows at runtime, during analysis.
module Test.Analysis.Metrics where

import           Cardano.Prelude

import qualified Data.Set as Set
import qualified Data.Text as T

import           Cardano.Analysis.API
import           Cardano.Report (fieldsWarnIfZero, fieldsZeroIsUnreported)
import           Cardano.Util (I)

import           Hedgehog


machPerfIds, blockPropIds, summaryIds :: Set FieldName
machPerfIds  = Set.fromList $ cdfFields @MachPerf @(CDF I) <&> FieldName . fId
blockPropIds = Set.fromList $ cdfFields @BlockProp @(CDF I) <&> FieldName . fId
summaryIds   = Set.fromList $ timelineFields @SummaryOne <&> FieldName . fId

undefinedIn :: Set FieldName -> [FieldName] -> [Text]
undefinedIn defined fs = unFieldName <$> filter (`Set.notMember` defined) fs

prop_selected_fields_are_defined = withTests 1 $ property $ do
  undefinedIn summaryIds   sumFieldsReport       === []
  undefinedIn machPerfIds  mtFieldsReport        === []
  undefinedIn machPerfIds  mtFieldsSnapshots     === []
  undefinedIn machPerfIds  mtFieldsTxProcessing  === []
  undefinedIn machPerfIds  fieldsWarnIfZero      === []
  undefinedIn machPerfIds  fieldsZeroIsUnreported === []
  undefinedIn blockPropIds bpFieldsControl       === []
  undefinedIn blockPropIds bpFieldsForger        === []
  undefinedIn blockPropIds bpFieldsPeers         === []
  undefinedIn blockPropIds bpFieldsEndToEnd      === []
  undefinedIn blockPropIds bpFieldsEndToEndBrief === []

-- | Derived from the record itself: a newly added CDF can't go without a definition.
prop_machPerf_cdfs_are_defined = withTests 1 $ property $
  Set.fromList (undefinedIn machPerfIds machPerfCDFs) === Set.fromList (unFieldName <$> unreported)
 where
   machPerfCDFs = FieldName <$> filter ("cdf" `T.isPrefixOf`) (recordSelectors (Proxy @(MachPerf I)))

-- | CDFs kept in the analysis results (JSON) on purpose, without a metric definition, so in
--   no report or data dictionary.
--   cdfHostSlots: slots analysed per node; it differs between nodes if one node's timeline
--   covers less of the run (late start, early stop), or its slots are filtered differently.
--   cdfLeads: leadership wins per slot, per node; with dense pools (several forger credentials
--   per node) the per-node rates differ by the credentials held.
unreported :: [FieldName]
unreported = [ "cdfHostSlots", "cdfLeads" ]

tests :: IO Bool
tests =
  checkSequential $$discover
