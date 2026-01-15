{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
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
  forMachine _dtal tr = mconcat $
    ( "reason" .= prettyShow tr ) :
    case tr of
      TraceRpcTestTrace _ -> [ "kind" .= String "TestTrace" ]
      TraceRpcFatalError _ -> [ "kind" .= String "FatalError" ]
      TraceRpcError _ -> [ "kind" .= String "Error" ]

      TraceRpcQuery queryTrace -> [ "kind" .= String "Query" ]
        <> case queryTrace of
          TraceRpcQueryParamsSpan s ->
            [ "queryName" .= String "ProtocolParameters"
            , spanToObject s
            ]

      TraceRpcSubmit submitTrace -> [ "kind" .= String "Submit" ]
        <> case submitTrace of
          TraceRpcSubmitTxDecodingFailure i _ -> [ "txIndex" .= show i ]
          TraceRpcSubmitN2cConnectionError _ -> [ ]
          TraceRpcSubmitTxValidationError i _ -> [ "txIndex" .= show i ]
          TraceRpcSubmitSpan s -> [spanToObject s]

  forHuman = docToText . pretty

instance MetaTrace TraceRpc where
  namespaceFor = Namespace [] . \case
    TraceRpcTestTrace _ -> ["TestTrace"]
    TraceRpcFatalError _ -> ["FatalError"]
    TraceRpcError _ -> ["Error"]

    TraceRpcQuery queryTrace ->
      "Query" :
        case queryTrace of
          TraceRpcQueryParamsSpan _ -> ["ProtocolParameters", "Span"]

    TraceRpcSubmit submitTrace ->
      "Submit" :
        case submitTrace of
          TraceRpcSubmitTxDecodingFailure _ _ -> ["TxDecodingFailure"]
          TraceRpcSubmitN2cConnectionError _ -> ["N2cConnectionError"]
          TraceRpcSubmitTxValidationError _ _ -> ["TxValidationError"]
          TraceRpcSubmitSpan _ -> ["Span"]

  severityFor (Namespace _ ["TestTrace"]) _ = Just Critical
  severityFor (Namespace _ ["FatalError"]) _ = Just Critical
  severityFor (Namespace _ ["Error"]) _ = Just Error
  severityFor (Namespace _ ["Query", "ProtocolParameters", "Span"]) _ = Just Warning
  severityFor (Namespace _ ["Submit", "Span"]) _ = Just Warning
  severityFor (Namespace _ ["Submit", "TxDecodingFailure"]) _ = Just Warning
  severityFor (Namespace _ ["Submit", "N2cConnectionError"]) _ = Just Warning
  severityFor (Namespace _ ["Submit", "TxValidationError"]) _ = Just Warning
  severityFor _ _ = Nothing

  documentFor (Namespace _ ["TestTrace"]) = Just ""
  documentFor (Namespace _ ["FatalError"]) = Just ""
  documentFor (Namespace _ ["Error"]) = Just ""
  documentFor (Namespace _ ["Query", "ProtocolParameters", "Span"]) = Just ""
  documentFor (Namespace _ ["Submit", "Span"]) = Just ""
  documentFor (Namespace _ ["Submit", "TxDecodingFailure"]) = Just ""
  documentFor (Namespace _ ["Submit", "N2cConnectionError"]) = Just ""
  documentFor (Namespace _ ["Submit", "TxValidationError"]) = Just ""
  documentFor _ = Nothing

  allNamespaces =
    [ Namespace [] ["TestTrace"]
    , Namespace [] ["FatalError"]
    , Namespace [] ["Error"]
    , Namespace [] ["Query", "ProtocolParameters", "Span"]
    , Namespace [] ["Submit", "Span"]
    , Namespace [] ["Submit", "TxDecodingFailure"]
    , Namespace [] ["Submit", "N2cConnectionError"]
    , Namespace [] ["Submit", "TxValidationError"]
    ]


-- helper functions

spanToObject :: TraceSpanEvent -> Object
spanToObject = \case
  SpanBegin -> "span" .= String "begin"
  SpanEnd -> "span" .= String "end"


