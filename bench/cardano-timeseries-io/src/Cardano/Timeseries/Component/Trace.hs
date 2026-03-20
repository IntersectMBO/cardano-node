{-# LANGUAGE OverloadedStrings #-}

module Cardano.Timeseries.Component.Trace(TimeseriesTrace(..)) where

import           Cardano.Logging (LogFormatting (..), SeverityS (..))
import           Cardano.Logging.Types (MetaTrace (..), Namespace (..))
import           Cardano.Timeseries.API
import           Cardano.Timeseries.Component.Types

import           Data.Aeson (toJSON)
import           Data.Aeson.Key (fromText)
import           Data.Aeson.KeyMap (singleton)
import           Data.Aeson.Types ((.=))
import           Data.Text (Text)
import           Data.Word (Word64)

data TimeseriesTrace = TimeseriesTraceCreate (Maybe TimeseriesConfig)
                     | TimeseriesTraceReconfigure (Maybe TimeseriesConfig)
                     | TimeseriesTraceInsert
                         Text -- ^ Origin key
                         Text -- ^ Origin value
                         Timestamp
                         [(MetricIdentifier, Double)] -- ^ Payload
                     | TimeseriesTraceIssueExecute QueryId Text
                     | TimeseriesTraceYieldExecute QueryId (Either ExecutionError Value)
                     -- COMMENT: (@russoul) shall we count the number of pruned entries?
                     | TimeseriesTracePrune
                        Word64 -- ^ retention (ms)
                        deriving (Show)

instance LogFormatting TimeseriesTrace where
  forMachine _ (TimeseriesTraceCreate cfg) =
    singleton "cfg" (toJSON cfg)
  forMachine _ (TimeseriesTraceReconfigure cfg) =
    singleton "cfg" (toJSON cfg)
  forMachine _ (TimeseriesTraceInsert originKey originValue t batch) = mconcat
    [
      fromText originKey .= originValue
    ,
      "timestamp" .= t
    ,
      "batch" .= batch
    ]
  forMachine _ (TimeseriesTraceIssueExecute queryId queryText) = mconcat
    [
      "query_id" .= queryId
    ,
      "query_text" .= queryText
    ]
  forMachine _ (TimeseriesTraceYieldExecute queryId result) = mconcat
    [
      "query_id" .= queryId
    ,
      "query_result" .= either asText showT result
    ]
  forMachine _ (TimeseriesTracePrune retMs) =
    singleton "retention_millis" (toJSON retMs)

instance MetaTrace TimeseriesTrace where
  allNamespaces =
    [
      Namespace [] ["Timeseries", "Create"]
    , Namespace [] ["Timeseries", "Reconfigure"]
    , Namespace [] ["Timeseries", "Insert"]
    , Namespace [] ["Timeseries", "IssueExecute"]
    , Namespace [] ["Timeseries", "YieldExecute"]
    , Namespace [] ["Timeseries", "Prune"]
    ]

  namespaceFor TimeseriesTraceCreate{}       = Namespace [] ["Timeseries", "Create"]
  namespaceFor TimeseriesTraceReconfigure{}  = Namespace [] ["Timeseries", "Reconfigure"]
  namespaceFor TimeseriesTraceInsert{}       = Namespace [] ["Timeseries", "Insert"]
  namespaceFor TimeseriesTraceIssueExecute{} = Namespace [] ["Timeseries", "IssueExecute"]
  namespaceFor TimeseriesTraceYieldExecute{} = Namespace [] ["Timeseries", "YieldExecute"]
  namespaceFor TimeseriesTracePrune{}        = Namespace [] ["Timeseries", "Prune"]

  severityFor (Namespace [] ["Timeseries", "Create"])        _ = Just Info
  severityFor (Namespace [] ["Timeseries", "Reconfigure"])   _ = Just Info
  severityFor (Namespace [] ["Timeseries", "Insert"])        _ = Just Debug -- That one clogs up the traces, hence lower severity
  severityFor (Namespace [] ["Timeseries", "IssueExecute"])  _ = Just Info
  severityFor (Namespace [] ["Timeseries", "YieldExecute"])  _ = Just Info
  severityFor (Namespace [] ["Timeseries", "Prune"])         _ = Just Info
  severityFor _                                              _ = Nothing

  documentFor (Namespace [] ["Timeseries", "Create"])       = Just "A timeseries handle has been created."
  documentFor (Namespace [] ["Timeseries", "Reconfigure"])  = Just "The timeseries handle has been reconfigured."
  documentFor (Namespace [] ["Timeseries", "Insert"])       = Just "A batch of metrics from one node has been inserted to the store."
  documentFor (Namespace [] ["Timeseries", "IssueExecute"]) = Just "The timeseries query has been issued."
  documentFor (Namespace [] ["Timeseries", "YieldExecute"]) = Just "The timeseries query has yielded a result."
  documentFor (Namespace [] ["Timeseries", "Prune"])        = Just "The timeseries store has been pruned."
  documentFor _                                             = Nothing
