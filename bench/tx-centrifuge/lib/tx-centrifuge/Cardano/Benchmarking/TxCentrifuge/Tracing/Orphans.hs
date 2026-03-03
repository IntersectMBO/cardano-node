{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

--------------------------------------------------------------------------------

-- | Orphan 'LogFormatting' and 'MetaTrace' instances copied from
-- @cardano-node@ (@NodeToNode.hs@ and @NodeToClient.hs@) so that
-- trace-dispatcher can format TxSubmission2, KeepAlive, and
-- TraceSendRecv messages.
module Cardano.Benchmarking.TxCentrifuge.Tracing.Orphans
  () where

--------------------------------------------------------------------------------

-----------
-- aeson --
-----------
import Data.Aeson (Value (String), (.=))
----------
-- text --
----------
import Data.Text (pack)
--------------------------
-- ouroboros-network --
--------------------------
-- First two using same qualified as "typed-protocol" imports below.
-- This is two import "NodeToClient.hs" `TraceSendMsg` instances unmmodified.
import Ouroboros.Network.Driver.Simple qualified as Simple
import Ouroboros.Network.Driver.Stateful qualified as Stateful
import Ouroboros.Network.Protocol.KeepAlive.Type qualified as KA
import Ouroboros.Network.Protocol.TxSubmission2.Type qualified as STX
----------------------
-- trace-dispatcher --
----------------------
-- We prefer the qualified import above but used to copy instances unmmodified.
import Cardano.Logging
  ( LogFormatting (..)
  , MetaTrace (..)
  , Namespace (..)
  , SeverityS (..)
  , nsCast
  , nsPrependInner
  )
---------------------
-- typed-protocols --
---------------------
-- First one to copy unmodified the instance definition of `TxSubmissionNode2`.
import Network.TypedProtocol.Codec (AnyMessage (AnyMessageAndAgency))
import Network.TypedProtocol.Codec qualified as Simple
import Network.TypedProtocol.Stateful.Codec qualified as Stateful

-- Copied instances: from cardano-node NodeToClient.hs
--------------------------------------------------------------------------------
-- Driver Simple.
--------------------------------------------------------------------------------

instance LogFormatting (Simple.AnyMessage ps)
      => LogFormatting (Simple.TraceSendRecv ps) where
  forMachine dtal (Simple.TraceSendMsg m) = mconcat
    [ "kind" .= String "Send" , "msg" .= forMachine dtal m ]
  forMachine dtal (Simple.TraceRecvMsg m) = mconcat
    [ "kind" .= String "Recv" , "msg" .= forMachine dtal m ]

  forHuman (Simple.TraceSendMsg m) = "Send: " <> forHuman m
  forHuman (Simple.TraceRecvMsg m) = "Receive: " <> forHuman m

  asMetrics (Simple.TraceSendMsg m) = asMetrics m
  asMetrics (Simple.TraceRecvMsg m) = asMetrics m

instance LogFormatting (Stateful.AnyMessage ps f)
      => LogFormatting (Stateful.TraceSendRecv ps f) where
  forMachine dtal (Stateful.TraceSendMsg m) = mconcat
    [ "kind" .= String "Send" , "msg" .= forMachine dtal m ]
  forMachine dtal (Stateful.TraceRecvMsg m) = mconcat
    [ "kind" .= String "Recv" , "msg" .= forMachine dtal m ]

  forHuman (Stateful.TraceSendMsg m) = "Send: " <> forHuman m
  forHuman (Stateful.TraceRecvMsg m) = "Receive: " <> forHuman m

  asMetrics (Stateful.TraceSendMsg m) = asMetrics m
  asMetrics (Stateful.TraceRecvMsg m) = asMetrics m

instance MetaTrace (Simple.AnyMessage ps) =>
            MetaTrace (Simple.TraceSendRecv ps) where
  namespaceFor (Simple.TraceSendMsg msg) =
    nsPrependInner "Send" (namespaceFor msg)
  namespaceFor (Simple.TraceRecvMsg msg) =
    nsPrependInner "Receive" (namespaceFor msg)

  severityFor (Namespace out ("Send" : tl)) (Just (Simple.TraceSendMsg msg)) =
    severityFor (Namespace out tl) (Just msg)
  severityFor (Namespace out ("Send" : tl)) Nothing =
    severityFor (Namespace out tl :: Namespace (Simple.AnyMessage ps)) Nothing
  severityFor (Namespace out ("Receive" : tl)) (Just (Simple.TraceSendMsg msg)) =
    severityFor (Namespace out tl) (Just msg)
  severityFor (Namespace out ("Receive" : tl)) Nothing =
    severityFor (Namespace out tl :: Namespace (Simple.AnyMessage ps)) Nothing
  severityFor _ _ = Nothing

  privacyFor (Namespace out ("Send" : tl)) (Just (Simple.TraceSendMsg msg)) =
    privacyFor (Namespace out tl) (Just msg)
  privacyFor (Namespace out ("Send" : tl)) Nothing =
    privacyFor (Namespace out tl :: Namespace (Simple.AnyMessage ps)) Nothing
  privacyFor (Namespace out ("Receive" : tl)) (Just (Simple.TraceSendMsg msg)) =
    privacyFor (Namespace out tl) (Just msg)
  privacyFor (Namespace out ("Receive" : tl)) Nothing =
    privacyFor (Namespace out tl :: Namespace (Simple.AnyMessage ps)) Nothing
  privacyFor _ _ = Nothing

  detailsFor (Namespace out ("Send" : tl)) (Just (Simple.TraceSendMsg msg)) =
    detailsFor (Namespace out tl) (Just msg)
  detailsFor (Namespace out ("Send" : tl)) Nothing =
    detailsFor (Namespace out tl :: Namespace (Simple.AnyMessage ps)) Nothing
  detailsFor (Namespace out ("Receive" : tl)) (Just (Simple.TraceSendMsg msg)) =
    detailsFor (Namespace out tl) (Just msg)
  detailsFor (Namespace out ("Receive" : tl)) Nothing =
    detailsFor (Namespace out tl :: Namespace (Simple.AnyMessage ps)) Nothing
  detailsFor _ _ = Nothing

  metricsDocFor (Namespace out ("Send" : tl)) =
    metricsDocFor (nsCast (Namespace out tl) :: Namespace (Simple.AnyMessage ps))
  metricsDocFor (Namespace out ("Receive" : tl)) =
    metricsDocFor (nsCast (Namespace out tl) :: Namespace (Simple.AnyMessage ps))
  metricsDocFor _ = []

  documentFor (Namespace out ("Send" : tl)) =
    documentFor (nsCast (Namespace out tl) :: Namespace (Simple.AnyMessage ps))
  documentFor (Namespace out ("Receive" : tl)) =
    documentFor (nsCast (Namespace out tl) :: Namespace (Simple.AnyMessage ps))
  documentFor _ = Nothing

  allNamespaces =
    let cn = allNamespaces :: [Namespace (Simple.AnyMessage ps)]
    in fmap (nsPrependInner "Send") cn ++ fmap (nsPrependInner "Receive") cn

instance MetaTrace (Stateful.AnyMessage ps f) =>
            MetaTrace (Stateful.TraceSendRecv ps f) where
  namespaceFor (Stateful.TraceSendMsg msg) =
    nsPrependInner "Send" (namespaceFor msg)
  namespaceFor (Stateful.TraceRecvMsg msg) =
    nsPrependInner "Receive" (namespaceFor msg)

  severityFor (Namespace out ("Send" : tl)) (Just (Stateful.TraceSendMsg msg)) =
    severityFor (Namespace out tl) (Just msg)
  severityFor (Namespace out ("Send" : tl)) Nothing =
    severityFor (Namespace out tl :: Namespace (Stateful.AnyMessage ps f)) Nothing

  severityFor (Namespace out ("Receive" : tl)) (Just (Stateful.TraceSendMsg msg)) =
    severityFor (Namespace out tl) (Just msg)
  severityFor (Namespace out ("Receive" : tl)) Nothing =
    severityFor (Namespace out tl :: Namespace (Stateful.AnyMessage ps f)) Nothing
  severityFor _ _ = Nothing

  privacyFor (Namespace out ("Send" : tl)) (Just (Stateful.TraceSendMsg msg)) =
    privacyFor (Namespace out tl) (Just msg)
  privacyFor (Namespace out ("Send" : tl)) Nothing =
    privacyFor (Namespace out tl :: Namespace (Stateful.AnyMessage ps f)) Nothing
  privacyFor (Namespace out ("Receive" : tl)) (Just (Stateful.TraceSendMsg msg)) =
    privacyFor (Namespace out tl) (Just msg)
  privacyFor (Namespace out ("Receive" : tl)) Nothing =
    privacyFor (Namespace out tl :: Namespace (Stateful.AnyMessage ps f)) Nothing
  privacyFor _ _ = Nothing

  detailsFor (Namespace out ("Send" : tl)) (Just (Stateful.TraceSendMsg msg)) =
    detailsFor (Namespace out tl) (Just msg)
  detailsFor (Namespace out ("Send" : tl)) Nothing =
    detailsFor (Namespace out tl :: Namespace (Stateful.AnyMessage ps f)) Nothing
  detailsFor (Namespace out ("Receive" : tl)) (Just (Stateful.TraceSendMsg msg)) =
    detailsFor (Namespace out tl) (Just msg)
  detailsFor (Namespace out ("Receive" : tl)) Nothing =
    detailsFor (Namespace out tl :: Namespace (Stateful.AnyMessage ps f)) Nothing
  detailsFor _ _ = Nothing

  metricsDocFor (Namespace out ("Send" : tl)) =
    metricsDocFor (nsCast (Namespace out tl) :: Namespace (Stateful.AnyMessage ps f))
  metricsDocFor (Namespace out ("Receive" : tl)) =
    metricsDocFor (nsCast (Namespace out tl) :: Namespace (Stateful.AnyMessage ps f))
  metricsDocFor _ = []

  documentFor (Namespace out ("Send" : tl)) =
    documentFor (nsCast (Namespace out tl) :: Namespace (Stateful.AnyMessage ps f))
  documentFor (Namespace out ("Receive" : tl)) =
    documentFor (nsCast (Namespace out tl) :: Namespace (Stateful.AnyMessage ps f))
  documentFor _ = Nothing

  allNamespaces =
    let cn = allNamespaces :: [Namespace (Stateful.AnyMessage ps f)]
    in fmap (nsPrependInner "Send") cn ++ fmap (nsPrependInner "Receive") cn

-- Copied instances: from cardano-node NodeToNode.hs
--------------------------------------------------------------------------------
-- TxSubmissionNode2 Tracer
--------------------------------------------------------------------------------

instance (Show txid, Show tx)
      => LogFormatting (AnyMessage (STX.TxSubmission2 txid tx)) where
  forMachine _dtal (AnyMessageAndAgency stok STX.MsgInit) =
    mconcat
      [ "kind" .= String "MsgInit"
      , "agency" .= String (pack $ show stok)
      ]
  forMachine _dtal (AnyMessageAndAgency stok (STX.MsgRequestTxs txids)) =
    mconcat
      [ "kind" .= String "MsgRequestTxs"
      , "agency" .= String (pack $ show stok)
      , "txIds" .= String (pack $ show txids)
      ]
  forMachine _dtal (AnyMessageAndAgency stok (STX.MsgReplyTxs txs)) =
    mconcat
      [ "kind" .= String "MsgReplyTxs"
      , "agency" .= String (pack $ show stok)
      , "txs" .= String (pack $ show txs)
      ]
  forMachine _dtal (AnyMessageAndAgency stok STX.MsgRequestTxIds {}) =
    mconcat
      [ "kind" .= String "MsgRequestTxIds"
      , "agency" .= String (pack $ show stok)
      ]
  forMachine _dtal (AnyMessageAndAgency stok (STX.MsgReplyTxIds _)) =
    mconcat
      [ "kind" .= String "MsgReplyTxIds"
      , "agency" .= String (pack $ show stok)
      ]
  forMachine _dtal (AnyMessageAndAgency stok STX.MsgDone) =
    mconcat
      [ "kind" .= String "MsgDone"
      , "agency" .= String (pack $ show stok)
      ]

instance MetaTrace (AnyMessage (STX.TxSubmission2 txid tx)) where
    namespaceFor (AnyMessageAndAgency _stok STX.MsgInit {}) =
      Namespace [] ["MsgInit"]
    namespaceFor (AnyMessageAndAgency _stok STX.MsgRequestTxs {}) =
      Namespace [] ["RequestTxIds"]
    namespaceFor (AnyMessageAndAgency _stok STX.MsgReplyTxs {}) =
      Namespace [] ["ReplyTxIds"]
    namespaceFor (AnyMessageAndAgency _stok STX.MsgRequestTxIds {}) =
      Namespace [] ["RequestTxs"]
    namespaceFor (AnyMessageAndAgency _stok STX.MsgReplyTxIds {}) =
      Namespace [] ["ReplyTxs"]
    namespaceFor (AnyMessageAndAgency _stok STX.MsgDone {}) =
      Namespace [] ["Done"]

    severityFor (Namespace _ ["MsgInit"]) _ = Just Debug
    severityFor (Namespace _ ["RequestTxIds"]) _ = Just Debug
    severityFor (Namespace _ ["ReplyTxIds"]) _ = Just Debug
    severityFor (Namespace _ ["RequestTxs"]) _ = Just Debug
    severityFor (Namespace _ ["ReplyTxs"]) _ = Just Debug
    severityFor (Namespace _ ["Done"]) _ = Just Debug
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["MsgInit"]) = Just
        "Client side hello message."
    documentFor (Namespace _ ["RequestTxIds"]) = Just $ mconcat
      [ "Request a non-empty list of transaction identifiers from the client, "
      , "and confirm a number of outstanding transaction identifiers. "
      , "\n "
      , "With 'TokBlocking' this is a a blocking operation: the response will "
      , "always have at least one transaction identifier, and it does not expect "
      , "a prompt response: there is no timeout. This covers the case when there "
      , "is nothing else to do but wait. For example this covers leaf nodes that "
      , "rarely, if ever, create and submit a transaction. "
      , "\n "
      , "With 'TokNonBlocking' this is a non-blocking operation: the response "
      , "may be an empty list and this does expect a prompt response. This "
      , "covers high throughput use cases where we wish to pipeline, by "
      , "interleaving requests for additional transaction identifiers with "
      , "requests for transactions, which requires these requests not block. "
      , "\n "
      , "The request gives the maximum number of transaction identifiers that "
      , "can be accepted in the response. This must be greater than zero in the "
      , "'TokBlocking' case. In the 'TokNonBlocking' case either the numbers "
      , "acknowledged or the number requested must be non-zero. In either case, "
      , "the number requested must not put the total outstanding over the fixed "
      , "protocol limit. "
      , "\n"
      , "The request also gives the number of outstanding transaction "
      , "identifiers that can now be acknowledged. The actual transactions "
      , "to acknowledge are known to the peer based on the FIFO order in which "
      , "they were provided. "
      , "\n "
      , "There is no choice about when to use the blocking case versus the "
      , "non-blocking case, it depends on whether there are any remaining "
      , "unacknowledged transactions (after taking into account the ones "
      , "acknowledged in this message): "
      , "\n "
      , "* The blocking case must be used when there are zero remaining "
      , "  unacknowledged transactions. "
      , "\n "
      , "* The non-blocking case must be used when there are non-zero remaining "
      , "  unacknowledged transactions."
      ]
    documentFor (Namespace _ ["ReplyTxIds"]) = Just $ mconcat
      [ "Reply with a list of transaction identifiers for available "
      , "transactions, along with the size of each transaction. "
      , "\n "
      , "The list must not be longer than the maximum number requested. "
      , "\n "
      , "In the 'StTxIds' 'StBlocking' state the list must be non-empty while "
      , "in the 'StTxIds' 'StNonBlocking' state the list may be empty. "
      , "\n "
      , "These transactions are added to the notional FIFO of outstanding "
      , "transaction identifiers for the protocol. "
      , "\n "
      , "The order in which these transaction identifiers are returned must be "
      , "the order in which they are submitted to the mempool, to preserve "
      , "dependent transactions."
      ]
    documentFor (Namespace _ ["RequestTxs"]) = Just $ mconcat
      [ "Request one or more transactions corresponding to the given  "
      , "transaction identifiers.  "
      , "\n "
      , "While it is the responsibility of the replying peer to keep within "
      , "pipelining in-flight limits, the sender must also cooperate by keeping "
      , "the total requested across all in-flight requests within the limits. "
      , "\n"
      , "It is an error to ask for transaction identifiers that were not "
      , "previously announced (via 'MsgReplyTxIds'). "
      , "\n"
      , "It is an error to ask for transaction identifiers that are not "
      , "outstanding or that were already asked for."
      ]
    documentFor (Namespace _ ["ReplyTxs"]) = Just $ mconcat
      [ "Reply with the requested transactions, or implicitly discard."
      , "\n"
      , "Transactions can become invalid between the time the transaction "
      , "identifier was sent and the transaction being requested. Invalid "
      , "(including committed) transactions do not need to be sent."
      , "\n"
      , "Any transaction identifiers requested but not provided in this reply "
      , "should be considered as if this peer had never announced them. (Note "
      , "that this is no guarantee that the transaction is invalid, it may still "
      , "be valid and available from another peer)."
      ]
    documentFor (Namespace _ ["Done"]) = Just $ mconcat
      [ "Termination message, initiated by the client when the server is "
      , "making a blocking call for more transaction identifiers."
      ]
    documentFor _ = Nothing

    allNamespaces = [
        Namespace [] ["MsgInit"]
      , Namespace [] ["RequestTxIds"]
      , Namespace [] ["ReplyTxIds"]
      , Namespace [] ["RequestTxs"]
      , Namespace [] ["ReplyTxs"]
      , Namespace [] ["Done"]
      ]

-- Copied instances: from cardano-node NodeToNode.hs
--------------------------------------------------------------------------------
-- KeepAlive Tracer
--------------------------------------------------------------------------------

instance LogFormatting (AnyMessage KA.KeepAlive) where
  forMachine _dtal (AnyMessageAndAgency stok KA.MsgKeepAlive {}) =
    mconcat
      [ "kind" .= String "KeepAlive"
      , "agency" .= String (pack $ show stok)
      ]
  forMachine _dtal (AnyMessageAndAgency stok KA.MsgKeepAliveResponse {}) =
    mconcat
      [ "kind" .= String "KeepAliveResponse"
      , "agency" .= String (pack $ show stok)
      ]
  forMachine _dtal (AnyMessageAndAgency stok KA.MsgDone) =
    mconcat
      [ "kind" .= String "Done"
      , "agency" .= String (pack $ show stok)
      ]

instance MetaTrace (AnyMessage KA.KeepAlive) where
    namespaceFor (AnyMessageAndAgency _stok KA.MsgKeepAlive {}) =
      Namespace [] ["KeepAlive"]
    namespaceFor (AnyMessageAndAgency _stok KA.MsgKeepAliveResponse {}) =
      Namespace [] ["KeepAliveResponse"]
    namespaceFor (AnyMessageAndAgency _stok KA.MsgDone) =
      Namespace [] ["Done"]

    severityFor (Namespace _ ["KeepAlive"]) _ = Just Debug
    severityFor (Namespace _ ["KeepAliveResponse"]) _ = Just Debug
    severityFor (Namespace _ ["Done"]) _ = Just Debug
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["KeepAlive"]) = Just
        "Client side message to keep the connection alive."
    documentFor (Namespace _ ["KeepAliveResponse"]) = Just $ mconcat
      [ "Server side response to a previous client KeepAlive message."
      ]
    documentFor (Namespace _ ["Done"]) = Just $ mconcat
      [ "Termination message, initiated by the client."
      ]
    documentFor _ = Nothing

    allNamespaces = [
        Namespace [] ["KeepAlive"]
      , Namespace [] ["KeepAliveResponse"]
      , Namespace [] ["Done"]
      ]
