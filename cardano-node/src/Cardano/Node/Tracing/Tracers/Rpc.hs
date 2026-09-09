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

import           Cardano.Logging hiding (nsInner)
import           Cardano.Rpc.Server (TraceRpc (..), TraceRpcNodeKernelAccess (..),
                   TraceRpcQuery (..), TraceRpcSubmit (..), TraceRpcSync (..), TraceSpanEvent (..))
import           Cardano.Rpc.Server.Config ()

import           Data.Aeson (Object, Value (..), (.=))

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
                TraceRpcQuerySearchUtxosSpan s ->
                  [ "queryName" .= String "SearchUtxos"
                  , spanToObject s
                  ]
                TraceRpcQueryReadGenesisSpan s ->
                  [ "queryName" .= String "ReadGenesis"
                  , spanToObject s
                  ]
          TraceRpcSubmit submitTrace ->
            ["kind" .= String "SubmitService"]
              <> case submitTrace of
                TraceRpcSubmitN2cConnectionError _ -> []
                TraceRpcSubmitTxDecodingError _ -> []
                TraceRpcSubmitTxValidationError _ -> []
                TraceRpcSubmitSpan s -> [spanToObject s]
                TraceRpcEvalTxDecodingError _ -> []
                TraceRpcEvalTxSpan s -> [spanToObject s]
          TraceRpcSync syncTrace ->
            ["kind" .= String "SyncService"]
              <> case syncTrace of
                TraceRpcFetchBlockSpan s -> [spanToObject s]
                TraceRpcFetchBlockNotFound _ -> []
                TraceRpcNodeKernelAccessUnavailable -> []
                TraceRpcReadTipSpan s -> [spanToObject s]
                TraceRpcFollowTipSpan s -> [spanToObject s]
          TraceRpcNodeKernelAccess nodeKernelAccessTrace ->
            ["kind" .= String "NodeKernelAccess"]
              <> case nodeKernelAccessTrace of
                TraceRpcUnsupportedBlockType blockType -> ["blockType" .= String blockType]
          TraceRpcServerListening endpoint ->
            [ "kind" .= String "ServerListening"
            , "endpoint" .= docToText (pretty endpoint)
            ]

  forHuman = docToText . pretty

  asMetrics = \case
    -- metrics for each rpc request
    -- query names here are taken from UTXORPC spec: https://utxorpc.org/query/intro/#operations
    TraceRpcQuery (TraceRpcQueryParamsSpan (SpanBegin _)) -> [CounterM "rpc.request.QueryService.ReadParams" Nothing]
    TraceRpcQuery (TraceRpcQueryReadUtxosSpan (SpanBegin _)) -> [CounterM "rpc.request.QueryService.ReadUtxos" Nothing]
    TraceRpcQuery (TraceRpcQuerySearchUtxosSpan (SpanBegin _)) -> [CounterM "rpc.request.QueryService.SearchUtxos" Nothing]
    TraceRpcQuery (TraceRpcQueryReadGenesisSpan (SpanBegin _)) -> [CounterM "rpc.request.QueryService.ReadGenesis" Nothing]
    TraceRpcSubmit (TraceRpcSubmitSpan (SpanBegin _)) -> [CounterM "rpc.request.SubmitService.SubmitTx" Nothing]
    TraceRpcSubmit (TraceRpcEvalTxSpan (SpanBegin _)) -> [CounterM "rpc.request.SubmitService.EvalTx" Nothing]
    TraceRpcSync (TraceRpcFetchBlockSpan (SpanBegin _)) -> [CounterM "rpc.request.SyncService.FetchBlock" Nothing]
    TraceRpcSync (TraceRpcReadTipSpan (SpanBegin _)) -> [CounterM "rpc.request.SyncService.ReadTip" Nothing]
    TraceRpcSync (TraceRpcFollowTipSpan (SpanBegin _)) -> [CounterM "rpc.request.SyncService.FollowTip" Nothing]
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
            TraceRpcQuerySearchUtxosSpan _ -> ["SearchUtxos", "Span"]
            TraceRpcQueryReadGenesisSpan _ -> ["ReadGenesis", "Span"]
      TraceRpcSubmit submitTrace ->
        "SubmitService"
          : case submitTrace of
            TraceRpcSubmitN2cConnectionError _ -> ["N2cConnectionError"]
            TraceRpcSubmitTxDecodingError _ -> ["TxDecodingError"]
            TraceRpcSubmitTxValidationError _ -> ["TxValidationError"]
            TraceRpcSubmitSpan _ -> ["SubmitTx", "Span"]
            TraceRpcEvalTxDecodingError _ -> ["EvalTxDecodingError"]
            TraceRpcEvalTxSpan _ -> ["EvalTx", "Span"]
      TraceRpcSync syncTrace ->
        "SyncService"
          : case syncTrace of
            TraceRpcFetchBlockSpan _ -> ["FetchBlock", "Span"]
            TraceRpcFetchBlockNotFound _ -> ["FetchBlockNotFound"]
            TraceRpcNodeKernelAccessUnavailable -> ["NodeKernelAccessUnavailable"]
            TraceRpcReadTipSpan _ -> ["ReadTip", "Span"]
            TraceRpcFollowTipSpan _ -> ["FollowTip", "Span"]
      TraceRpcNodeKernelAccess nodeKernelAccessTrace ->
        "NodeKernelAccess"
          : case nodeKernelAccessTrace of
            TraceRpcUnsupportedBlockType _ -> ["UnsupportedBlockType"]
      TraceRpcServerListening _ -> ["ServerListening"]

  severityFor (Namespace _ nsInner) _ = case nsInner of
    ["FatalError"] -> Just Error -- RPC server startup errors
    ["Error"] -> Just Debug -- those are normal operation errors, like request errors, hide them by default
    ["QueryService", "ReadParams", "Span"] -> Just Debug
    ["QueryService", "ReadUtxos", "Span"] -> Just Debug
    ["QueryService", "SearchUtxos", "Span"] -> Just Debug
    ["QueryService", "ReadGenesis", "Span"] -> Just Debug
    ["SubmitService", "SubmitTx", "Span"] -> Just Debug
    ["SubmitService", "EvalTx", "Span"] -> Just Debug
    ["SubmitService", "N2cConnectionError"] -> Just Warning -- this is a more serious error, this shouldn't happen
    ["SubmitService", "TxDecodingError"] -> Just Debug -- request error
    ["SubmitService", "TxValidationError"] -> Just Debug -- request error
    ["SubmitService", "EvalTxDecodingError"] -> Just Debug -- request error
    ["SyncService", "FetchBlock", "Span"] -> Just Debug
    ["SyncService", "FetchBlockNotFound"] -> Just Debug
    ["SyncService", "NodeKernelAccessUnavailable"] -> Just Warning
    ["SyncService", "ReadTip", "Span"] -> Just Debug
    ["SyncService", "FollowTip", "Span"] -> Just Debug
    ["NodeKernelAccess", "UnsupportedBlockType"] -> Just Warning
    ["ServerListening"] -> Just Notice -- one-off startup event, must be visible with default config
    _ -> Nothing

  documentFor (Namespace _ nsInner) = case nsInner of
    ["FatalError"] -> Just "RPC startup critical error."
    ["Error"] -> Just "Normal operation errors such as request errors. Those are not harmful to the RPC server itself."
    ["QueryService", "ReadParams", "Span"] -> Just "Span for the ReadParams UTXORPC method."
    ["QueryService", "ReadUtxos", "Span"] -> Just "Span for the ReadUtxos UTXORPC method."
    ["QueryService", "SearchUtxos", "Span"] -> Just "Span for the SearchUtxos UTXORPC method."
    ["QueryService", "ReadGenesis", "Span"] -> Just "Span for the ReadGenesis UTXORPC method."
    ["SubmitService", "SubmitTx", "Span"] -> Just "Span for the SubmitTx UTXORPC method."
    ["SubmitService", "EvalTx", "Span"] -> Just "Span for the EvalTx UTXORPC method."
    ["SubmitService", "N2cConnectionError"] ->
      Just
        "Node connection error. This should not happen, as this means that there is an issue in cardano-rpc configuration."
    ["SubmitService", "TxDecodingError"] -> Just "A regular request error, when submitted transaction decoding fails."
    ["SubmitService", "TxValidationError"] -> Just "A regular request error, when submitted transaction is invalid."
    ["SubmitService", "EvalTxDecodingError"] -> Just "A regular request error, when evalTx transaction decoding fails."
    ["SyncService", "FetchBlock", "Span"] -> Just "Span for the FetchBlock SyncService method."
    ["SyncService", "FetchBlockNotFound"] -> Just "Requested block was not found in ChainDB."
    ["SyncService", "NodeKernelAccessUnavailable"] -> Just "Node kernel access not yet initialised. The node is still starting up."
    ["SyncService", "ReadTip", "Span"] -> Just "Span for the ReadTip SyncService method."
    ["SyncService", "FollowTip", "Span"] -> Just "Span for the FollowTip SyncService method."
    ["NodeKernelAccess", "UnsupportedBlockType"] -> Just "The block type is not supported by the RPC server."
    ["ServerListening"] -> Just "RPC server is starting to listen on the configured endpoint."
    _ -> Nothing

  metricsDocFor (Namespace _ nsInner) = case nsInner of
    ["QueryService", "ReadParams", "Span"] ->
      [("rpc.request.QueryService.ReadParams", "Span for the ReadParams UTXORPC method.")]
    ["QueryService", "ReadUtxos", "Span"] ->
      [("rpc.request.QueryService.ReadUtxos", "Span for the ReadUtxos UTXORPC method.")]
    ["QueryService", "SearchUtxos", "Span"] ->
      [("rpc.request.QueryService.SearchUtxos", "Span for the SearchUtxos UTXORPC method.")]
    ["QueryService", "ReadGenesis", "Span"] ->
      [("rpc.request.QueryService.ReadGenesis", "Span for the ReadGenesis UTXORPC method.")]
    ["SubmitService", "SubmitTx", "Span"] ->
      [("rpc.request.SubmitService.SubmitTx", "Span for the SubmitTx UTXORPC method.")]
    ["SubmitService", "EvalTx", "Span"] ->
      [("rpc.request.SubmitService.EvalTx", "Span for the EvalTx UTXORPC method.")]
    ["SyncService", "FetchBlock", "Span"] ->
      [("rpc.request.SyncService.FetchBlock", "Span for the FetchBlock SyncService method.")]
    ["SyncService", "ReadTip", "Span"] ->
      [("rpc.request.SyncService.ReadTip", "Span for the ReadTip SyncService method.")]
    ["SyncService", "FollowTip", "Span"] ->
      [("rpc.request.SyncService.FollowTip", "Span for the FollowTip SyncService method.")]
    _ -> []

  allNamespaces =
    Namespace []
      <$> [ ["FatalError"]
          , ["Error"]
          , ["QueryService", "ReadParams", "Span"]
          , ["QueryService", "ReadUtxos", "Span"]
          , ["QueryService", "SearchUtxos", "Span"]
          , ["SubmitService", "SubmitTx", "Span"]
          , ["SubmitService", "EvalTx", "Span"]
          , ["SubmitService", "N2cConnectionError"]
          , ["SubmitService", "TxDecodingError"]
          , ["SubmitService", "TxValidationError"]
          , ["SubmitService", "EvalTxDecodingError"]
          , ["SyncService", "FetchBlock", "Span"]
          , ["SyncService", "FetchBlockNotFound"]
          , ["SyncService", "NodeKernelAccessUnavailable"]
          , ["SyncService", "ReadTip", "Span"]
          , ["SyncService", "FollowTip", "Span"]
          , ["QueryService", "ReadGenesis", "Span"]
          , ["NodeKernelAccess", "UnsupportedBlockType"]
          , ["ServerListening"]
          ]

-- helper functions

spanToObject :: TraceSpanEvent -> Object
spanToObject =
  mconcat . \case
    SpanBegin spanId -> ["span" .= String "begin", "spanId" .= spanId]
    SpanEnd spanId -> ["span" .= String "end", "spanId" .= spanId]
