{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Node.Tracing.Tracers.Rpc () where

import           Cardano.Api.Pretty

import           Cardano.Logging
import           Cardano.Rpc.Server (TraceRpc (..), TraceRpcQuery (..), TraceRpcSubmit (..),
                   TraceSpanEvent (..))

import           Data.Aeson (Object, ToJSON, ToJSONKey, Value (..), object, toJSON, toJSONList,
                   (.=))

instance LogFormatting TraceRpc where
  forMachine _dtal tr =
    mconcat $
      ("reason" .= prettyShow tr)
        : case tr of
          TraceRpcFatalError _ -> ["kind" .= String "FatalError"]
          TraceRpcError _ -> ["kind" .= String "Error"]
          TraceRpcQuery queryTrace ->
            ["kind" .= String "QueryService"]
              <> case queryTrace of
                TraceRpcQueryParamsSpan s ->
                  [ "queryName" .= String "ReadParams"
                  , spanToObject s
                  ]
                TraceRpcQueryReadUtxosSpan s ->
                  [ "queryName" .= String "ReadUtxos"
                  , spanToObject s
                  ]
          TraceRpcSubmit submitTrace ->
            ["kind" .= String "SubmitService"]
              <> case submitTrace of
                TraceRpcSubmitN2cConnectionError _ -> []
                TraceRpcSubmitTxDecodingError _ -> []
                TraceRpcSubmitTxValidationError _ -> []
                TraceRpcSubmitSpan s -> [spanToObject s]

  forHuman = docToText . pretty

  asMetrics = \case
    -- metrics for each rpc request
    -- query names here are taken from UTXORPC spec: https://utxorpc.org/query/intro/#operations
    TraceRpcQuery (TraceRpcQueryParamsSpan (SpanBegin _)) -> [CounterM "rpc.request.QueryService.ReadParams" Nothing]
    TraceRpcQuery (TraceRpcQueryReadUtxosSpan (SpanBegin _)) -> [CounterM "rpc.request.QueryService.ReadUtxos" Nothing]
    TraceRpcSubmit (TraceRpcSubmitSpan (SpanBegin _)) -> [CounterM "rpc.request.SubmitService.SubmitTx" Nothing]
    _ -> []

instance MetaTrace TraceRpc where
  namespaceFor =
    Namespace [] . \case
      TraceRpcFatalError _ -> ["FatalError"]
      TraceRpcError _ -> ["Error"]
      TraceRpcQuery queryTrace ->
        "QueryService"
          : case queryTrace of
            TraceRpcQueryParamsSpan _ -> ["ReadParams", "Span"]
            TraceRpcQueryReadUtxosSpan _ -> ["ReadUtxos", "Span"]
      TraceRpcSubmit submitTrace ->
        "SubmitService"
          : case submitTrace of
            TraceRpcSubmitN2cConnectionError _ -> ["N2cConnectionError"]
            TraceRpcSubmitTxDecodingError _ -> ["TxDecodingError"]
            TraceRpcSubmitTxValidationError _ -> ["TxValidationError"]
            TraceRpcSubmitSpan _ -> ["Span"]

  severityFor (Namespace _ nsInner) _ = case nsInner of
    ["FatalError"] -> Just Error -- RPC server startup errors
    ["Error"] -> Just Debug -- those are normal operation errors, like request errors, hide them by default
    ["QueryService", "ReadParams", "Span"] -> Just Debug
    ["QueryService", "ReadUtxos", "Span"] -> Just Debug
    ["SubmitService", "SubmitTx", "Span"] -> Just Debug
    ["SubmitService", "N2cConnectionError"] -> Just Warning -- this is a more serious error, this shouldn't happen
    ["SubmitService", "TxDecodingError"] -> Just Debug -- request error
    ["SubmitService", "TxValidationError"] -> Just Debug -- request error
    _ -> Nothing

  documentFor (Namespace _ nsInner) = case nsInner of
    ["FatalError"] -> Just ""
    ["Error"] -> Just ""
    ["QueryService", "ReadParams", "Span"] -> Just ""
    ["QueryService", "ReadUtxos", "Span"] -> Just ""
    ["SubmitService", "SubmitTx", "Span"] -> Just ""
    ["SubmitService", "N2cConnectionError"] -> Just ""
    ["SubmitService", "TxDecodingError"] -> Just ""
    ["SubmitService", "TxValidationError"] -> Just ""
    _ -> Nothing

  metricsDocFor (Namespace _ nsInner) = case nsInner of
    ["QueryService", "ReadParams", "Span"] -> [("rpc.request.QueryService.ReadParams", "<some docstring; can be empty>")]
    ["QueryService", "ReadUtxos", "Span"] -> [("rpc.request.QueryService.ReadUtxos", "<some docstring; can be empty>")]
    ["SubmitService", "SubmitTx", "Span"] -> [("rpc.request.SubmitService.SubmitTx", "<some docstring; can be empty>")]
    _ -> []

  allNamespaces =
    Namespace []
      <$> [ ["FatalError"]
          , ["Error"]
          , ["QueryService", "ReadParams", "Span"]
          , ["QueryService", "ReadParams", "Span"]
          , ["SubmitService", "SubmitTx", "Span"]
          , ["SubmitService", "N2cConnectionError"]
          , ["SubmitService", "TxDecodingError"]
          , ["SubmitService", "TxValidationError"]
          ]

-- helper functions

spanToObject :: TraceSpanEvent -> Object
spanToObject =
  mconcat . \case
    SpanBegin spanId -> ["span" .= String "begin", "spanId" .= spanId]
    SpanEnd spanId -> ["span" .= String "end", "spanId" .= spanId]
