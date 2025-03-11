{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Node.Tracing.Tracers.NodeToClient () where

import           Cardano.Logging
import           Ouroboros.Consensus.Ledger.Query (Query)
import qualified Ouroboros.Network.Driver.Simple as Simple
import qualified Ouroboros.Network.Driver.Stateful as Stateful
import           Ouroboros.Network.Protocol.ChainSync.Type as ChainSync
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as LSQ
import qualified Ouroboros.Network.Protocol.LocalTxMonitor.Type as LTM
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type as LTS

import           Data.Aeson (Value (String), (.=))
import           Data.Text (Text, pack)
import qualified Network.TypedProtocol.Codec as Simple
import qualified Network.TypedProtocol.Stateful.Codec as Stateful

{-# ANN module ("HLint: ignore Redundant bracket" :: Text) #-}

instance LogFormatting (Simple.AnyMessage ps)
      => LogFormatting (Simple.TraceSendRecv ps) where
  forMachine dtal (Simple.TraceSendMsg m) = mconcat
    [ "kind" .= String "Send" , "msg" .= forMachine dtal m ]
  forMachine dtal (Simple.TraceRecvMsg m) = mconcat
    [ "kind" .= String "Recv" , "msg" .= forMachine dtal m ]

  forHuman (Simple.TraceSendMsg m) = "Send: " <> forHumanOrMachine m
  forHuman (Simple.TraceRecvMsg m) = "Receive: " <> forHumanOrMachine m

  asMetrics (Simple.TraceSendMsg m) = asMetrics m
  asMetrics (Simple.TraceRecvMsg m) = asMetrics m

instance LogFormatting (Stateful.AnyMessage ps f)
      => LogFormatting (Stateful.TraceSendRecv ps f) where
  forMachine dtal (Stateful.TraceSendMsg m) = mconcat
    [ "kind" .= String "Send" , "msg" .= forMachine dtal m ]
  forMachine dtal (Stateful.TraceRecvMsg m) = mconcat
    [ "kind" .= String "Recv" , "msg" .= forMachine dtal m ]

  forHuman (Stateful.TraceSendMsg m) = "Send: " <> forHumanOrMachine m
  forHuman (Stateful.TraceRecvMsg m) = "Receive: " <> forHumanOrMachine m

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


-- --------------------------------------------------------------------------------
-- -- TChainSync Tracer
-- --------------------------------------------------------------------------------

instance LogFormatting (Simple.AnyMessage (ChainSync blk pt tip)) where
   forMachine _dtal (Simple.AnyMessageAndAgency stok ChainSync.MsgRequestNext{}) =
     mconcat [ "kind" .= String "MsgRequestNext"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (Simple.AnyMessageAndAgency stok ChainSync.MsgAwaitReply{}) =
     mconcat [ "kind" .= String "MsgAwaitReply"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (Simple.AnyMessageAndAgency stok ChainSync.MsgRollForward{}) =
     mconcat [ "kind" .= String "MsgRollForward"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (Simple.AnyMessageAndAgency stok ChainSync.MsgRollBackward{}) =
     mconcat [ "kind" .= String "MsgRollBackward"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (Simple.AnyMessageAndAgency stok ChainSync.MsgFindIntersect{}) =
     mconcat [ "kind" .= String "MsgFindIntersect"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (Simple.AnyMessageAndAgency stok ChainSync.MsgIntersectFound{}) =
     mconcat [ "kind" .= String "MsgIntersectFound"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (Simple.AnyMessageAndAgency stok ChainSync.MsgIntersectNotFound{}) =
     mconcat [ "kind" .= String "MsgIntersectNotFound"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (Simple.AnyMessageAndAgency stok ChainSync.MsgDone{}) =
     mconcat [ "kind" .= String "MsgDone"
              , "agency" .= String (pack $ show stok)
              ]

instance MetaTrace (Simple.AnyMessage (ChainSync blk pt tip)) where
    namespaceFor (Simple.AnyMessageAndAgency _agency (MsgRequestNext {})) =
      Namespace [] ["RequestNext"]
    namespaceFor (Simple.AnyMessageAndAgency _agency (MsgAwaitReply {})) =
      Namespace [] ["AwaitReply"]
    namespaceFor (Simple.AnyMessageAndAgency _agency (MsgRollForward {})) =
      Namespace [] ["RollForward"]
    namespaceFor (Simple.AnyMessageAndAgency _agency (MsgRollBackward {})) =
      Namespace [] ["RollBackward"]
    namespaceFor (Simple.AnyMessageAndAgency _agency (MsgFindIntersect {})) =
      Namespace [] ["FindIntersect"]
    namespaceFor (Simple.AnyMessageAndAgency _agency (MsgIntersectFound {})) =
      Namespace [] ["IntersectFound"]
    namespaceFor (Simple.AnyMessageAndAgency _agency (MsgIntersectNotFound {})) =
      Namespace [] ["IntersectNotFound"]
    namespaceFor (Simple.AnyMessageAndAgency _agency (MsgDone {})) =
      Namespace [] ["Done"]

    severityFor (Namespace _ ["RequestNext"]) _ = Just Info
    severityFor (Namespace _ ["AwaitReply"]) _ = Just Info
    severityFor (Namespace _ ["RollForward"]) _ = Just Info
    severityFor (Namespace _ ["RollBackward"]) _ = Just Info
    severityFor (Namespace _ ["FindIntersect"]) _ = Just Info
    severityFor (Namespace _ ["IntersectFound"]) _ = Just Info
    severityFor (Namespace _ ["IntersectNotFound"]) _ = Just Info
    severityFor (Namespace _ ["Done"]) _ = Just Info
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["RequestNext"]) = Just $ mconcat
      [ "Request the next update from the producer. The response can be a roll "
      , "forward, a roll back or wait."
      ]
    documentFor (Namespace _ ["AwaitReply"]) = Just $ mconcat
      [ "Acknowledge the request but require the consumer to wait for the next "
      , "update. This means that the consumer is synced with the producer, and "
      , "the producer is waiting for its own chain state to change."
      ]
    documentFor (Namespace _ ["RollForward"]) = Just $ mconcat
      [ "Tell the consumer to extend their chain with the given header. "
      , "\n "
      , "The message also tells the consumer about the head point of the producer."
      ]
    documentFor (Namespace _ ["RollBackward"]) = Just $ mconcat
      [ "Tell the consumer to roll back to a given point on their chain. "
      , "\n "
      , "The message also tells the consumer about the head point of the producer."
      ]
    documentFor (Namespace _ ["FindIntersect"]) = Just $ mconcat
      [ "Ask the producer to try to find an improved intersection point between "
      , "the consumer and producer's chains. The consumer sends a sequence of "
      , "points and it is up to the producer to find the first intersection point "
      , "on its chain and send it back to the consumer."
      ]
    documentFor (Namespace _ ["IntersectFound"]) = Just $ mconcat
      [ "The reply to the consumer about an intersection found. "
      , "The consumer can decide weather to send more points. "
      , "\n "
      , "The message also tells the consumer about the head point of the producer."
      ]
    documentFor (Namespace _ ["IntersectNotFound"]) = Just $ mconcat
      [ "The reply to the consumer that no intersection was found: none of the "
      , "points the consumer supplied are on the producer chain. "
      , "\n "
      , "The message also tells the consumer about the head point of the producer."
      ]
    documentFor (Namespace _ ["Done"]) = Just $ mconcat
      [ "We have to explain to the framework what our states mean, in terms of "
      , "which party has agency in each state. "
      , "\n "
      , "Idle states are where it is for the client to send a message, "
      , "busy states are where the server is expected to send a reply."
      ]
    documentFor _ = Nothing

    allNamespaces = [
        Namespace [] ["RequestNext"]
      , Namespace [] ["AwaitReply"]
      , Namespace [] ["RollForward"]
      , Namespace [] ["RollBackward"]
      , Namespace [] ["FindIntersect"]
      , Namespace [] ["IntersectFound"]
      , Namespace [] ["IntersectNotFound"]
      , Namespace [] ["Done"]
      ]


--------------------------------------------------------------------------------
-- LocalTxMonitor Tracer
--------------------------------------------------------------------------------

instance LogFormatting (Simple.AnyMessage (LTM.LocalTxMonitor txid tx slotNo)) where
  forMachine _dtal (Simple.AnyMessageAndAgency stok LTM.MsgAcquire {}) =
    mconcat [ "kind" .= String "MsgAcquire"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (Simple.AnyMessageAndAgency stok LTM.MsgAcquired {}) =
    mconcat [ "kind" .= String "MsgAcquired"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (Simple.AnyMessageAndAgency stok LTM.MsgAwaitAcquire {}) =
    mconcat [ "kind" .= String "MsgAwaitAcquire"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (Simple.AnyMessageAndAgency stok LTM.MsgNextTx {}) =
    mconcat [ "kind" .= String "MsgNextTx"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (Simple.AnyMessageAndAgency stok LTM.MsgReplyNextTx {}) =
    mconcat [ "kind" .= String "MsgReplyNextTx"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (Simple.AnyMessageAndAgency stok LTM.MsgHasTx {}) =
    mconcat [ "kind" .= String "MsgHasTx"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (Simple.AnyMessageAndAgency stok LTM.MsgReplyHasTx {}) =
    mconcat [ "kind" .= String "MsgReplyHasTx"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (Simple.AnyMessageAndAgency stok LTM.MsgGetSizes {}) =
    mconcat [ "kind" .= String "MsgGetSizes"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (Simple.AnyMessageAndAgency stok LTM.MsgReplyGetSizes {}) =
    mconcat [ "kind" .= String "MsgReplyGetSizes"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (Simple.AnyMessageAndAgency stok LTM.MsgRelease {}) =
    mconcat [ "kind" .= String "MsgRelease"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (Simple.AnyMessageAndAgency stok LTM.MsgDone {}) =
    mconcat [ "kind" .= String "MsgDone"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (Simple.AnyMessageAndAgency stok LTM.MsgGetMeasures {}) =
    mconcat [ "kind" .= String "MsgGetMeasures"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (Simple.AnyMessageAndAgency stok LTM.MsgReplyGetMeasures {}) =
    mconcat [ "kind" .= String "MsgReplyGetMeasures"
             , "agency" .= String (pack $ show stok)
             ]

instance MetaTrace (Simple.AnyMessage (LTM.LocalTxMonitor txid tx slotNo)) where
    namespaceFor (Simple.AnyMessageAndAgency _agency LTM.MsgAcquire {}) =
      Namespace [] ["Acquire"]
    namespaceFor (Simple.AnyMessageAndAgency _agency LTM.MsgAcquired {}) =
      Namespace [] ["Acquired"]
    namespaceFor (Simple.AnyMessageAndAgency _agency LTM.MsgAwaitAcquire {}) =
      Namespace [] ["AwaitAcquire"]
    namespaceFor (Simple.AnyMessageAndAgency _agency LTM.MsgNextTx {}) =
      Namespace [] ["NextTx"]
    namespaceFor (Simple.AnyMessageAndAgency _agency LTM.MsgReplyNextTx {}) =
      Namespace [] ["ReplyNextTx"]
    namespaceFor (Simple.AnyMessageAndAgency _agency LTM.MsgHasTx {}) =
      Namespace [] ["HasTx"]
    namespaceFor (Simple.AnyMessageAndAgency _agency LTM.MsgReplyHasTx {}) =
      Namespace [] ["ReplyHasTx"]
    namespaceFor (Simple.AnyMessageAndAgency _agency LTM.MsgGetSizes {}) =
      Namespace [] ["GetSizes"]
    namespaceFor (Simple.AnyMessageAndAgency _agency LTM.MsgReplyGetSizes {}) =
      Namespace [] ["ReplyGetSizes"]
    namespaceFor (Simple.AnyMessageAndAgency _agency LTM.MsgRelease {}) =
      Namespace [] ["Release"]
    namespaceFor (Simple.AnyMessageAndAgency _agency LTM.MsgDone {}) =
      Namespace [] ["Done"]
    namespaceFor (Simple.AnyMessageAndAgency _agency LTM.MsgGetMeasures {}) =
      Namespace [] ["GetMeasures"]
    namespaceFor (Simple.AnyMessageAndAgency _agency LTM.MsgReplyGetMeasures {}) =
      Namespace [] ["ReplyGetMeasures"]

    severityFor (Namespace _ ["Acquire"]) _ = Just Info
    severityFor (Namespace _ ["Acquired"]) _ = Just Info
    severityFor (Namespace _ ["AwaitAcquire"]) _ = Just Info
    severityFor (Namespace _ ["NextTx"]) _ = Just Info
    severityFor (Namespace _ ["ReplyNextTx"]) _ = Just Info
    severityFor (Namespace _ ["HasTx"]) _ = Just Info
    severityFor (Namespace _ ["ReplyHasTx"]) _ = Just Info
    severityFor (Namespace _ ["GetSizes"]) _ = Just Info
    severityFor (Namespace _ ["ReplyGetSizes"]) _ = Just Info
    severityFor (Namespace _ ["Release"]) _ = Just Info
    severityFor (Namespace _ ["Done"]) _ = Just Info
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["Acquire"]) = Just
      ""
    documentFor (Namespace _ ["Acquired"]) = Just
      ""
    documentFor (Namespace _ ["AwaitAcquire"]) = Just
      ""
    documentFor (Namespace _ ["NextTx"]) = Just
      ""
    documentFor (Namespace _ ["ReplyNextTx"]) = Just
      ""
    documentFor (Namespace _ ["HasTx"]) = Just
      ""
    documentFor (Namespace _ ["ReplyHasTx"]) = Just
      ""
    documentFor (Namespace _ ["GetSizes"]) = Just
      ""
    documentFor (Namespace _ ["ReplyGetSizes"]) = Just
      ""
    documentFor (Namespace _ ["Release"]) = Just
      ""
    documentFor (Namespace _ ["Done"]) = Just
      ""
    documentFor _ = Nothing

    allNamespaces = [
        Namespace [] ["Acquire"]
      , Namespace [] ["Acquired"]
      , Namespace [] ["AwaitAcquire"]
      , Namespace [] ["NextTx"]
      , Namespace [] ["ReplyNextTx"]
      , Namespace [] ["HasTx"]
      , Namespace [] ["ReplyHasTx"]
      , Namespace [] ["GetSizes"]
      , Namespace [] ["ReplyGetSizes"]
      , Namespace [] ["Release"]
      , Namespace [] ["Done"]
      ]
--------------------------------------------------------------------------------
-- LocalTxSubmission Tracer
--------------------------------------------------------------------------------

instance LogFormatting (Simple.AnyMessage (LTS.LocalTxSubmission tx err)) where
  forMachine _dtal (Simple.AnyMessageAndAgency stok LTS.MsgSubmitTx{}) =
    mconcat [ "kind" .= String "MsgSubmitTx"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (Simple.AnyMessageAndAgency stok LTS.MsgAcceptTx{}) =
    mconcat [ "kind" .= String "MsgAcceptTx"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (Simple.AnyMessageAndAgency stok LTS.MsgRejectTx{}) =
    mconcat [ "kind" .= String "MsgRejectTx"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (Simple.AnyMessageAndAgency stok LTS.MsgDone{}) =
    mconcat [ "kind" .= String "MsgDone"
             , "agency" .= String (pack $ show stok)
             ]

instance MetaTrace (Simple.AnyMessage (LTS.LocalTxSubmission tx err)) where
    namespaceFor (Simple.AnyMessageAndAgency _agency LTS.MsgSubmitTx{}) =
      Namespace [] ["SubmitTx"]
    namespaceFor (Simple.AnyMessageAndAgency _agency LTS.MsgAcceptTx{}) =
      Namespace [] ["AcceptTx"]
    namespaceFor (Simple.AnyMessageAndAgency _agency LTS.MsgRejectTx{}) =
      Namespace [] ["RejectTx"]
    namespaceFor (Simple.AnyMessageAndAgency _agency LTS.MsgDone{}) =
      Namespace [] ["Done"]

    severityFor (Namespace _ ["SubmitTx"]) _ = Just Info
    severityFor (Namespace _ ["AcceptTx"]) _ = Just Info
    severityFor (Namespace _ ["RejectTx"]) _ = Just Info
    severityFor (Namespace _ ["Done"]) _ = Just Info
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["SubmitTx"]) = Just
      "The client submits a single transaction and waits a reply."
    documentFor (Namespace _ ["AcceptTx"]) = Just
      "The server can reply to inform the client that it has accepted the \
        \transaction."
    documentFor (Namespace _ ["RejectTx"]) = Just
      "The server can reply to inform the client that it has rejected the \
        \transaction. A reason for the rejection is included."
    documentFor (Namespace _ ["Done"]) = Just
      "The client can terminate the protocol."
    documentFor _ = Nothing

    allNamespaces = [
        Namespace [] ["SubmitTx"]
      , Namespace [] ["AcceptTx"]
      , Namespace [] ["RejectTx"]
      , Namespace [] ["Done"]
      ]
--------------------------------------------------------------------------------
-- TStateQuery Tracer
--------------------------------------------------------------------------------

instance (forall result. Show (Query blk result))
      => LogFormatting (Simple.AnyMessage (LSQ.LocalStateQuery blk pt (Query blk))) where
  forMachine _dtal (Simple.AnyMessageAndAgency stok LSQ.MsgAcquire{}) =
    mconcat [ "kind" .= String "MsgAcquire"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (Simple.AnyMessageAndAgency stok LSQ.MsgAcquired{}) =
    mconcat [ "kind" .= String "MsgAcquired"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (Simple.AnyMessageAndAgency stok LSQ.MsgFailure{}) =
    mconcat [ "kind" .= String "MsgFailure"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (Simple.AnyMessageAndAgency stok LSQ.MsgQuery{}) =
    mconcat [ "kind" .= String "MsgQuery"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (Simple.AnyMessageAndAgency stok LSQ.MsgResult{}) =
    mconcat [ "kind" .= String "MsgResult"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (Simple.AnyMessageAndAgency stok LSQ.MsgRelease{}) =
    mconcat [ "kind" .= String "MsgRelease"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (Simple.AnyMessageAndAgency stok LSQ.MsgReAcquire{}) =
    mconcat [ "kind" .= String "MsgReAcquire"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (Simple.AnyMessageAndAgency stok LSQ.MsgDone{}) =
    mconcat [ "kind" .= String "MsgDone"
             , "agency" .= String (pack $ show stok)
             ]

instance (forall result. Show (Query blk result))
      => LogFormatting (Stateful.AnyMessage (LSQ.LocalStateQuery blk pt (Query blk)) f) where
  forMachine _dtal (Stateful.AnyMessageAndAgency stok _ LSQ.MsgAcquire{}) =
    mconcat [ "kind" .= String "MsgAcquire"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (Stateful.AnyMessageAndAgency stok _ LSQ.MsgAcquired{}) =
    mconcat [ "kind" .= String "MsgAcquired"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (Stateful.AnyMessageAndAgency stok _ LSQ.MsgFailure{}) =
    mconcat [ "kind" .= String "MsgFailure"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (Stateful.AnyMessageAndAgency stok _ LSQ.MsgQuery{}) =
    mconcat [ "kind" .= String "MsgQuery"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (Stateful.AnyMessageAndAgency stok _ LSQ.MsgResult{}) =
    mconcat [ "kind" .= String "MsgResult"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (Stateful.AnyMessageAndAgency stok _ LSQ.MsgRelease{}) =
    mconcat [ "kind" .= String "MsgRelease"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (Stateful.AnyMessageAndAgency stok _ LSQ.MsgReAcquire{}) =
    mconcat [ "kind" .= String "MsgReAcquire"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (Stateful.AnyMessageAndAgency stok _ LSQ.MsgDone{}) =
    mconcat [ "kind" .= String "MsgDone"
             , "agency" .= String (pack $ show stok)
             ]

instance MetaTrace (Simple.AnyMessage (LSQ.LocalStateQuery blk pt (Query blk))) where
    namespaceFor (Simple.AnyMessageAndAgency _agency LSQ.MsgAcquire{}) =
      Namespace [] ["Acquire"]
    namespaceFor (Simple.AnyMessageAndAgency _agency LSQ.MsgAcquired{}) =
      Namespace [] ["Acquired"]
    namespaceFor (Simple.AnyMessageAndAgency _agency LSQ.MsgFailure{}) =
      Namespace [] ["Failure"]
    namespaceFor (Simple.AnyMessageAndAgency _agency LSQ.MsgQuery{}) =
      Namespace [] ["Query"]
    namespaceFor (Simple.AnyMessageAndAgency _agency LSQ.MsgResult{}) =
      Namespace [] ["Result"]
    namespaceFor (Simple.AnyMessageAndAgency _agency LSQ.MsgRelease{}) =
      Namespace [] ["Release"]
    namespaceFor (Simple.AnyMessageAndAgency _agency LSQ.MsgReAcquire{}) =
      Namespace [] ["ReAcquire"]
    namespaceFor (Simple.AnyMessageAndAgency _agency LSQ.MsgDone{}) =
      Namespace [] ["Done"]

    severityFor (Namespace _ ["Acquire"]) _ = Just Info
    severityFor (Namespace _ ["Acquired"]) _ = Just Info
    severityFor (Namespace _ ["Failure"]) _ = Just Warning
    severityFor (Namespace _ ["Query"]) _ = Just Info
    severityFor (Namespace _ ["Result"]) _ = Just Info
    severityFor (Namespace _ ["Release"]) _ = Just Info
    severityFor (Namespace _ ["ReAcquire"]) _ = Just Info
    severityFor (Namespace _ ["Done"]) _ = Just Info
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["Acquire"]) = Just $ mconcat
      [ "The client requests that the state as of a particular recent point on "
      , "the server's chain (within K of the tip) be made available to query, "
      , "and waits for confirmation or failure. "
      , "\n "
      , "From 'NodeToClient_V8' onwards if the point is not specified, current tip "
      , "will be acquired.  For previous versions of the protocol 'point' must be "
      , "given."
      ]
    documentFor (Namespace _ ["Acquired"]) = Just
      "The server can confirm that it has the state at the requested point."
    documentFor (Namespace _ ["Failure"]) = Just $ mconcat
      [ "The server can report that it cannot obtain the state for the "
      , "requested point."
      ]
    documentFor (Namespace _ ["Query"]) = Just
      "The client can perform queries on the current acquired state."
    documentFor (Namespace _ ["Result"]) = Just
      "The server must reply with the queries."
    documentFor (Namespace _ ["Release"]) = Just $ mconcat
      [ "The client can instruct the server to release the state. This lets "
      , "the server free resources."
      ]
    documentFor (Namespace _ ["ReAcquire"]) = Just $ mconcat
      [ "This is like 'MsgAcquire' but for when the client already has a "
      , "state. By moving to another state directly without a 'MsgRelease' it "
      , "enables optimisations on the server side (e.g. moving to the state for "
      , "the immediate next block). "
      , "\n "
      , "Note that failure to re-acquire is equivalent to 'MsgRelease', "
      , "rather than keeping the exiting acquired state. "
      , "\n "
      , "From 'NodeToClient_V8' onwards if the point is not specified, current tip "
      , "will be acquired.  For previous versions of the protocol 'point' must be "
      , "given."
      ]
    documentFor (Namespace _ ["Done"]) = Just
      "The client can terminate the protocol."
    documentFor _ = Nothing

    allNamespaces = [
        Namespace [] ["Acquire"]
      , Namespace [] ["Acquired"]
      , Namespace [] ["Failure"]
      , Namespace [] ["Query"]
      , Namespace [] ["Result"]
      , Namespace [] ["Release"]
      , Namespace [] ["ReAcquire"]
      , Namespace [] ["Done"]
      ]

instance MetaTrace (Stateful.AnyMessage (LSQ.LocalStateQuery blk pt (Query blk)) f) where
    namespaceFor (Stateful.AnyMessageAndAgency _agency _ LSQ.MsgAcquire{}) =
      Namespace [] ["Acquire"]
    namespaceFor (Stateful.AnyMessageAndAgency _agency _ LSQ.MsgAcquired{}) =
      Namespace [] ["Acquired"]
    namespaceFor (Stateful.AnyMessageAndAgency _agency _ LSQ.MsgFailure{}) =
      Namespace [] ["Failure"]
    namespaceFor (Stateful.AnyMessageAndAgency _agency _ LSQ.MsgQuery{}) =
      Namespace [] ["Query"]
    namespaceFor (Stateful.AnyMessageAndAgency _agency _ LSQ.MsgResult{}) =
      Namespace [] ["Result"]
    namespaceFor (Stateful.AnyMessageAndAgency _agency _ LSQ.MsgRelease{}) =
      Namespace [] ["Release"]
    namespaceFor (Stateful.AnyMessageAndAgency _agency _ LSQ.MsgReAcquire{}) =
      Namespace [] ["ReAcquire"]
    namespaceFor (Stateful.AnyMessageAndAgency _agency _ LSQ.MsgDone{}) =
      Namespace [] ["Done"]

    severityFor (Namespace _ ["Acquire"]) _ = Just Info
    severityFor (Namespace _ ["Acquired"]) _ = Just Info
    severityFor (Namespace _ ["Failure"]) _ = Just Warning
    severityFor (Namespace _ ["Query"]) _ = Just Info
    severityFor (Namespace _ ["Result"]) _ = Just Info
    severityFor (Namespace _ ["Release"]) _ = Just Info
    severityFor (Namespace _ ["ReAcquire"]) _ = Just Info
    severityFor (Namespace _ ["Done"]) _ = Just Info
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["Acquire"]) = Just $ mconcat
      [ "The client requests that the state as of a particular recent point on "
      , "the server's chain (within K of the tip) be made available to query, "
      , "and waits for confirmation or failure. "
      , "\n "
      , "From 'NodeToClient_V8' onwards if the point is not specified, current tip "
      , "will be acquired.  For previous versions of the protocol 'point' must be "
      , "given."
      ]
    documentFor (Namespace _ ["Acquired"]) = Just
      "The server can confirm that it has the state at the requested point."
    documentFor (Namespace _ ["Failure"]) = Just $ mconcat
      [ "The server can report that it cannot obtain the state for the "
      , "requested point."
      ]
    documentFor (Namespace _ ["Query"]) = Just
      "The client can perform queries on the current acquired state."
    documentFor (Namespace _ ["Result"]) = Just
      "The server must reply with the queries."
    documentFor (Namespace _ ["Release"]) = Just $ mconcat
      [ "The client can instruct the server to release the state. This lets "
      , "the server free resources."
      ]
    documentFor (Namespace _ ["ReAcquire"]) = Just $ mconcat
      [ "This is like 'MsgAcquire' but for when the client already has a "
      , "state. By moving to another state directly without a 'MsgRelease' it "
      , "enables optimisations on the server side (e.g. moving to the state for "
      , "the immediate next block). "
      , "\n "
      , "Note that failure to re-acquire is equivalent to 'MsgRelease', "
      , "rather than keeping the exiting acquired state. "
      , "\n "
      , "From 'NodeToClient_V8' onwards if the point is not specified, current tip "
      , "will be acquired.  For previous versions of the protocol 'point' must be "
      , "given."
      ]
    documentFor (Namespace _ ["Done"]) = Just
      "The client can terminate the protocol."
    documentFor _ = Nothing

    allNamespaces = [
        Namespace [] ["Acquire"]
      , Namespace [] ["Acquired"]
      , Namespace [] ["Failure"]
      , Namespace [] ["Query"]
      , Namespace [] ["Result"]
      , Namespace [] ["Release"]
      , Namespace [] ["ReAcquire"]
      , Namespace [] ["Done"]
      ]
