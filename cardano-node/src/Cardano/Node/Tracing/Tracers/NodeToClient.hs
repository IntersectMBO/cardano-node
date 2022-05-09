{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Node.Tracing.Tracers.NodeToClient
  ( severityTChainSync
  , namesForTChainSync
  , docTChainSyncNodeToClient
  , docTChainSyncNodeToNode
  , docTChainSyncNodeToNodeSerisalised

  , severityTTxMonitor
  , namesForTTxMonitor
  , docTTxMonitor

  , severityTTxSubmission
  , namesForTTxSubmission
  , docTTxSubmission

  , severityTStateQuery
  , namesForTStateQuery
  , docTStateQuery
  ) where

import           Cardano.Logging
import           Cardano.Prelude hiding (Show, show)
import           Data.Aeson (Value (String), (.=))
import           Network.TypedProtocol.Codec (AnyMessage (..))
import           Text.Show

import           Cardano.Slotting.Slot (SlotNo)

import           Network.Mux.Trace (TraceLabelPeer (..))

import           Ouroboros.Consensus.Ledger.Query (Query)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx, GenTxId)

import           Ouroboros.Network.Block (Point, Serialised, Tip)
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.Driver.Simple (TraceSendRecv (..))
import           Ouroboros.Network.Protocol.ChainSync.Type as ChainSync
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as LSQ
import qualified Ouroboros.Network.Protocol.LocalTxMonitor.Type as LTM
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type as LTS

import           Cardano.Node.Tracing.Tracers.NodeToNode (formatMessageWithAgency)


instance LogFormatting (AnyMessage ps)
      => LogFormatting (TraceSendRecv ps) where
  forMachine dtal (TraceSendMsg m) = mconcat
    [ "kind" .= String "Send" , "msg" .= forMachine dtal m ]
  forMachine dtal (TraceRecvMsg m) = mconcat
    [ "kind" .= String "Recv" , "msg" .= forMachine dtal m ]

  forHuman (TraceSendMsg m) = "Send: " <> forHuman m
  forHuman (TraceRecvMsg m) = "Receive: " <> forHuman m

  asMetrics (TraceSendMsg m) = asMetrics m
  asMetrics (TraceRecvMsg m) = asMetrics m


--------------------------------------------------------------------------------
-- TChainSync Tracer
--------------------------------------------------------------------------------

severityTChainSync :: BlockFetch.TraceLabelPeer peer (TraceSendRecv
    (ChainSync (Serialised blk) (Point blk) (Tip blk))) -> SeverityS
severityTChainSync (BlockFetch.TraceLabelPeer _ v) = severityTChainSync' v
  where
    severityTChainSync' (TraceSendMsg msg) = severityTChainSync'' msg
    severityTChainSync' (TraceRecvMsg msg) = severityTChainSync'' msg

    severityTChainSync'' (AnyMessage msg) = severityTChainSync''' msg

    severityTChainSync''' :: Message
                                     (ChainSync header point tip) from to
                                   -> SeverityS
    severityTChainSync''' MsgRequestNext {}       = Info
    severityTChainSync''' MsgAwaitReply {}        = Info
    severityTChainSync''' MsgRollForward {}       = Info
    severityTChainSync''' MsgRollBackward {}      = Info
    severityTChainSync''' MsgFindIntersect {}     = Info
    severityTChainSync''' MsgIntersectFound {}    = Info
    severityTChainSync''' MsgIntersectNotFound {} = Info
    severityTChainSync''' MsgDone {}              = Info

namesForTChainSync :: BlockFetch.TraceLabelPeer peer (TraceSendRecv
    (ChainSync (Serialised blk) (Point blk) (Tip blk))) -> [Text]
namesForTChainSync (BlockFetch.TraceLabelPeer _ v) = namesTChainSync v
  where

    namesTChainSync (TraceSendMsg msg) = "Send" : namesTChainSync' msg
    namesTChainSync (TraceRecvMsg msg) = "Receive" : namesTChainSync' msg

    namesTChainSync' (AnyMessage msg) = namesTChainSync'' msg

    namesTChainSync'' :: Message (ChainSync header point tip) from to
                               -> [Text]
    namesTChainSync'' MsgRequestNext {}       = ["RequestNext"]
    namesTChainSync'' MsgAwaitReply {}        = ["AwaitReply"]
    namesTChainSync'' MsgRollForward {}       = ["RollForward"]
    namesTChainSync'' MsgRollBackward {}      = ["RollBackward"]
    namesTChainSync'' MsgFindIntersect {}     = ["FindIntersect"]
    namesTChainSync'' MsgIntersectFound {}    = ["IntersectFound"]
    namesTChainSync'' MsgIntersectNotFound {} = ["IntersectNotFound"]
    namesTChainSync'' MsgDone {}              = ["Done"]


instance ( forall (st :: ChainSync blk pt tip) (st' :: ChainSync blk pt tip).
             Show (Message (ChainSync blk pt tip) st st'))
      => LogFormatting (AnyMessage (ChainSync blk pt tip)) where
   forMachine dtal (AnyMessage msg@ChainSync.MsgRequestNext {}) =
     formatMessageWithAgency dtal msg "MsgRequestNext"
   forMachine dtal (AnyMessage msg@ChainSync.MsgAwaitReply{}) =
     formatMessageWithAgency dtal msg "MsgAwaitReply"
   forMachine dtal (AnyMessage msg@ChainSync.MsgRollForward{}) =
     formatMessageWithAgency dtal msg "MsgRollForward"
   forMachine dtal (AnyMessage msg@ChainSync.MsgRollBackward{}) =
     formatMessageWithAgency dtal msg "MsgRollBackward"
   forMachine dtal (AnyMessage msg@ChainSync.MsgFindIntersect{}) =
     formatMessageWithAgency dtal msg "MsgFindIntersect"
   forMachine dtal (AnyMessage msg@ChainSync.MsgIntersectFound{}) =
     formatMessageWithAgency dtal msg "MsgIntersectFound"
   forMachine dtal (AnyMessage msg@ChainSync.MsgIntersectNotFound{}) =
     formatMessageWithAgency dtal msg "MsgIntersectNotFound"
   forMachine dtal (AnyMessage msg@ChainSync.MsgDone{}) =
     formatMessageWithAgency dtal msg "MsgDone"

docTChainSyncNodeToClient :: Documented (BlockFetch.TraceLabelPeer peer (TraceSendRecv
    (ChainSync x (Point blk) (Tip blk))))
docTChainSyncNodeToClient =
    addDocumentedNamespace  ["Send"] docTChainSync
    `addDocs` addDocumentedNamespace  ["Receive"] docTChainSync


docTChainSyncNodeToNode :: Documented (BlockFetch.TraceLabelPeer peer (TraceSendRecv
    (ChainSync x (Point blk) (Tip blk))))
docTChainSyncNodeToNode =
    addDocumentedNamespace  ["Send"] docTChainSync
    `addDocs` addDocumentedNamespace  ["Receive"] docTChainSync

docTChainSyncNodeToNodeSerisalised :: Documented (BlockFetch.TraceLabelPeer peer (TraceSendRecv
    (ChainSync x (Point blk) (Tip blk))))
docTChainSyncNodeToNodeSerisalised =
    addDocumentedNamespace  ["Send"] docTChainSync
    `addDocs` addDocumentedNamespace  ["Receive"] docTChainSync


docTChainSync :: Documented (BlockFetch.TraceLabelPeer peer (TraceSendRecv
    (ChainSync x (Point blk) (Tip blk))))
docTChainSync = Documented [
      DocMsg
        ["RequestNext"]
        []
        "Request the next update from the producer. The response can be a roll \
        \forward, a roll back or wait."
    , DocMsg
        ["AwaitReply"]
        []
        "Acknowledge the request but require the consumer to wait for the next \
        \update. This means that the consumer is synced with the producer, and \
        \the producer is waiting for its own chain state to change."
    , DocMsg
        ["RollForward"]
        []
        "Tell the consumer to extend their chain with the given header. \
        \\n \
        \The message also tells the consumer about the head point of the producer."
    , DocMsg
        ["RollBackward"]
        []
        "Tell the consumer to roll back to a given point on their chain. \
        \\n \
        \The message also tells the consumer about the head point of the producer."
    , DocMsg
        ["FindIntersect"]
        []
        "Ask the producer to try to find an improved intersection point between \
        \the consumer and producer's chains. The consumer sends a sequence of \
        \points and it is up to the producer to find the first intersection point \
        \on its chain and send it back to the consumer."
    , DocMsg
        ["IntersectFound"]
        []
        "The reply to the consumer about an intersection found. \
        \The consumer can decide weather to send more points. \
        \\n \
        \The message also tells the consumer about the head point of the producer."
    , DocMsg
        ["IntersectNotFound"]
        []
        "The reply to the consumer that no intersection was found: none of the \
        \points the consumer supplied are on the producer chain. \
        \\n \
        \The message also tells the consumer about the head point of the producer."
    , DocMsg
        ["Done"]
        []
        "We have to explain to the framework what our states mean, in terms of \
        \which party has agency in each state. \
        \\n \
        \Idle states are where it is for the client to send a message, \
        \busy states are where the server is expected to send a reply."
  ]

--------------------------------------------------------------------------------
-- LocalTxMonitor Tracer
--------------------------------------------------------------------------------

severityTTxMonitor
  :: TraceLabelPeer peer
       (TraceSendRecv (LTM.LocalTxMonitor (GenTxId blk) (GenTx blk) SlotNo))
  -> SeverityS
severityTTxMonitor _ = Info

namesForTTxMonitor
  :: TraceLabelPeer peer
       (TraceSendRecv (LTM.LocalTxMonitor (GenTxId blk) (GenTx blk) SlotNo))
  -> [Text]
namesForTTxMonitor (TraceLabelPeer _ v) = namesForTTxMonitor' v
  where
    namesForTTxMonitor' (TraceSendMsg msg) = "Send"    : namesForTTxMonitor'' msg
    namesForTTxMonitor' (TraceRecvMsg msg) = "Receive" : namesForTTxMonitor'' msg

    namesForTTxMonitor'' (AnyMessage msg) = namesForTTxMonitor''' msg

    namesForTTxMonitor''' :: Message (LTM.LocalTxMonitor (GenTxId blk) (GenTx blk) SlotNo)
                                     from to
                          -> [Text]
    namesForTTxMonitor''' LTM.MsgAcquire {} = ["Acquire"]
    namesForTTxMonitor''' LTM.MsgAcquired {} = ["Acquired"]
    namesForTTxMonitor''' LTM.MsgAwaitAcquire {} = ["AwaitAcquire"]
    namesForTTxMonitor''' LTM.MsgNextTx {} = ["NextTx"]
    namesForTTxMonitor''' LTM.MsgReplyNextTx {} = ["ReplyNextTx"]
    namesForTTxMonitor''' LTM.MsgHasTx {} = ["HasTx"]
    namesForTTxMonitor''' LTM.MsgReplyHasTx {} = ["ReplyHasTx"]
    namesForTTxMonitor''' LTM.MsgGetSizes {} = ["GetSizes"]
    namesForTTxMonitor''' LTM.MsgReplyGetSizes {} = ["ReplyGetSizes"]
    namesForTTxMonitor''' LTM.MsgRelease {} = ["Release"]
    namesForTTxMonitor''' LTM.MsgDone {} = ["Done"]

instance (forall (st :: LTM.LocalTxMonitor txid tx slotNo)
                 (st' :: LTM.LocalTxMonitor txid tx slotNo).
            Show (Message (LTM.LocalTxMonitor txid tx slotNo) st st'))
      => LogFormatting (AnyMessage (LTM.LocalTxMonitor txid tx slotNo)) where
  forMachine dtal (AnyMessage msg@LTM.MsgAcquire {}) =
    formatMessageWithAgency dtal msg "MsgAcquire"
  forMachine dtal (AnyMessage msg@LTM.MsgAcquired {}) =
    formatMessageWithAgency dtal msg "MsgAcquired"
  forMachine dtal (AnyMessage msg@LTM.MsgAwaitAcquire {}) =
    formatMessageWithAgency dtal msg "MsgAwaitAcquire"
  forMachine dtal (AnyMessage msg@LTM.MsgNextTx {}) =
    formatMessageWithAgency dtal msg "MsgNextTx"
  forMachine dtal (AnyMessage msg@LTM.MsgReplyNextTx {}) =
    formatMessageWithAgency dtal msg "MsgReplyNextTx"
  forMachine dtal (AnyMessage msg@LTM.MsgHasTx {}) =
    formatMessageWithAgency dtal msg "MsgHasTx"
  forMachine dtal (AnyMessage msg@LTM.MsgReplyHasTx {}) =
    formatMessageWithAgency dtal msg "MsgReplyHasTx"
  forMachine dtal (AnyMessage msg@LTM.MsgGetSizes {}) =
    formatMessageWithAgency dtal msg "MsgGetSizes"
  forMachine dtal (AnyMessage msg@LTM.MsgReplyGetSizes {}) =
    formatMessageWithAgency dtal msg "MsgReplyGetSizes"
  forMachine dtal (AnyMessage msg@LTM.MsgRelease {}) =
    formatMessageWithAgency dtal msg "MsgRelease"
  forMachine dtal (AnyMessage msg@LTM.MsgDone {}) =
    formatMessageWithAgency dtal msg "MsgDone"

docTTxMonitor :: Documented
  (TraceLabelPeer
     localPeer
     (TraceSendRecv
        (LTM.LocalTxMonitor
           (GenTxId blk) (GenTx blk) SlotNo)))
docTTxMonitor =
  addDocumentedNamespace  ["Send"] docTState
   `addDocs` addDocumentedNamespace  ["Receive"] docTState


--------------------------------------------------------------------------------
-- LocalTxSubmission Tracer
--------------------------------------------------------------------------------

severityTTxSubmission :: BlockFetch.TraceLabelPeer peer
  (TraceSendRecv (LTS.LocalTxSubmission (GenTx blk) (ApplyTxErr blk)))
  -> SeverityS
severityTTxSubmission (BlockFetch.TraceLabelPeer _ v) = severityTTxSubmission' v
  where
    severityTTxSubmission' (TraceSendMsg msg) = severityTTxSubmission'' msg
    severityTTxSubmission' (TraceRecvMsg msg) = severityTTxSubmission'' msg

    severityTTxSubmission'' (AnyMessage msg) = severityTTxSubmission''' msg

    severityTTxSubmission''' :: Message
                                        (LTS.LocalTxSubmission tx reject) from to
                                      -> SeverityS
    severityTTxSubmission''' LTS.MsgSubmitTx {} = Info
    severityTTxSubmission''' LTS.MsgAcceptTx {} = Info
    severityTTxSubmission''' LTS.MsgRejectTx {} = Info
    severityTTxSubmission''' LTS.MsgDone {}     = Info


namesForTTxSubmission :: BlockFetch.TraceLabelPeer peer
  (TraceSendRecv (LTS.LocalTxSubmission (GenTx blk) (ApplyTxErr blk)))
  -> [Text]
namesForTTxSubmission (BlockFetch.TraceLabelPeer _ v) = namesTTxSubmission v
  where
    namesTTxSubmission (TraceSendMsg msg) = "Send" : namesTTxSubmission' msg
    namesTTxSubmission (TraceRecvMsg msg) = "Receive" : namesTTxSubmission' msg

    namesTTxSubmission' (AnyMessage msg) = namesTTxSubmission'' msg

    namesTTxSubmission'' :: Message
                                    (LTS.LocalTxSubmission tx reject) from to
                                  -> [Text]
    namesTTxSubmission'' LTS.MsgSubmitTx {} = ["SubmitTx"]
    namesTTxSubmission'' LTS.MsgAcceptTx {} = ["AcceptTx"]
    namesTTxSubmission'' LTS.MsgRejectTx {} = ["RejectTx"]
    namesTTxSubmission'' LTS.MsgDone {}     = ["Done"]


instance (forall (st :: LTS.LocalTxSubmission tx err)
                 (st' :: LTS.LocalTxSubmission tx err).
            Show (Message (LTS.LocalTxSubmission tx err) st st'))
      => LogFormatting (AnyMessage (LTS.LocalTxSubmission tx err)) where
  forMachine dtal (AnyMessage msg@LTS.MsgSubmitTx{}) =
    formatMessageWithAgency dtal msg "MsgSubmitTx"
  forMachine dtal (AnyMessage msg@LTS.MsgAcceptTx{}) =
    formatMessageWithAgency dtal msg "MsgAcceptTx"
  forMachine dtal (AnyMessage msg@LTS.MsgRejectTx{}) =
    formatMessageWithAgency dtal msg "MsgRejectTx"
  forMachine dtal (AnyMessage msg@LTS.MsgDone{}) =
    formatMessageWithAgency dtal msg "MsgDone"

docTTxSubmission :: Documented
  (BlockFetch.TraceLabelPeer
     localPeer
     (TraceSendRecv
        (LTS.LocalTxSubmission
           (GenTx blk) (ApplyTxErr blk))))
docTTxSubmission =
  addDocumentedNamespace  ["Send"] docTTxSubmission'
   `addDocs` addDocumentedNamespace  ["Receive"] docTTxSubmission'

docTTxSubmission' :: Documented
   (BlockFetch.TraceLabelPeer
      localPeer
      (TraceSendRecv
         (LTS.LocalTxSubmission
            (GenTx blk) (ApplyTxErr blk))))
docTTxSubmission' = Documented [
      DocMsg
        ["SubmitTx"]
        []
        "The client submits a single transaction and waits a reply."
    , DocMsg
        ["AcceptTx"]
        []
        "The server can reply to inform the client that it has accepted the \
        \transaction."
    , DocMsg
        ["RejectTx"]
        []
        "The server can reply to inform the client that it has rejected the \
        \transaction. A reason for the rejection is included."
    , DocMsg
        ["Done"]
        []
        "The client can terminate the protocol."
  ]

--------------------------------------------------------------------------------
-- TStateQuery Tracer
--------------------------------------------------------------------------------

severityTStateQuery :: BlockFetch.TraceLabelPeer peer
  (TraceSendRecv (LSQ.LocalStateQuery blk (Point blk) query))
  -> SeverityS
severityTStateQuery (BlockFetch.TraceLabelPeer _ v) = severityTStateQuery' v
  where
    severityTStateQuery' (TraceSendMsg msg) = severityTStateQuery'' msg
    severityTStateQuery' (TraceRecvMsg msg) = severityTStateQuery'' msg

    severityTStateQuery'' (AnyMessage msg) = severityTStateQuery''' msg

    severityTStateQuery''' :: Message
                                    (LSQ.LocalStateQuery block point query1) from to
                                  -> SeverityS
    severityTStateQuery''' LSQ.MsgAcquire {}   = Info
    severityTStateQuery''' LSQ.MsgAcquired {}  = Info
    severityTStateQuery''' LSQ.MsgFailure {}   = Warning
    severityTStateQuery''' LSQ.MsgQuery {}     = Info
    severityTStateQuery''' LSQ.MsgResult {}    = Info
    severityTStateQuery''' LSQ.MsgRelease {}   = Info
    severityTStateQuery''' LSQ.MsgReAcquire {} = Info
    severityTStateQuery''' LSQ.MsgDone {}      = Info

namesForTStateQuery :: BlockFetch.TraceLabelPeer peer
  (TraceSendRecv (LSQ.LocalStateQuery blk (Point blk) query))
  -> [Text]
namesForTStateQuery (BlockFetch.TraceLabelPeer _ v) = namesForTStateQuery' v
  where
    namesForTStateQuery' (TraceSendMsg msg) = "Send" : namesForTStateQuery'' msg
    namesForTStateQuery' (TraceRecvMsg msg) = "Receive" : namesForTStateQuery'' msg

    namesForTStateQuery'' (AnyMessage msg) = namesForTStateQuery''' msg

    namesForTStateQuery''' :: Message
                                    (LSQ.LocalStateQuery block point query1) from to
                                  -> [Text]

    namesForTStateQuery''' LSQ.MsgAcquire {}   = ["Acquire"]
    namesForTStateQuery''' LSQ.MsgAcquired {}  = ["Acquired"]
    namesForTStateQuery''' LSQ.MsgFailure {}   = ["Failure"]
    namesForTStateQuery''' LSQ.MsgQuery {}     = ["Query"]
    namesForTStateQuery''' LSQ.MsgResult {}    = ["Result"]
    namesForTStateQuery''' LSQ.MsgRelease {}   = ["Release"]
    namesForTStateQuery''' LSQ.MsgReAcquire {} = ["ReAcquire"]
    namesForTStateQuery''' LSQ.MsgDone {}      = ["Done"]

instance ( forall result. Show (Query blk result)
         , forall (st :: LSQ.LocalStateQuery blk pt (Query blk))
                  (st' :: LSQ.LocalStateQuery blk pt (Query blk)).
             Show (Message (LSQ.LocalStateQuery blk pt (Query blk)) st st')
         )
      => LogFormatting (AnyMessage (LSQ.LocalStateQuery blk pt (Query blk))) where
  forMachine dtal (AnyMessage msg@LSQ.MsgAcquire{}) =
    formatMessageWithAgency dtal msg "MsgAcquire"
  forMachine dtal (AnyMessage msg@LSQ.MsgAcquired{}) =
    formatMessageWithAgency dtal msg "MsgAcquired"
  forMachine dtal (AnyMessage msg@LSQ.MsgFailure{}) =
    formatMessageWithAgency dtal msg "MsgFailure"
  forMachine dtal (AnyMessage msg@LSQ.MsgQuery{}) =
    formatMessageWithAgency dtal msg "MsgQuery"
  forMachine dtal (AnyMessage msg@LSQ.MsgResult{}) =
    formatMessageWithAgency dtal msg "MsgResult"
  forMachine dtal (AnyMessage msg@LSQ.MsgRelease{}) =
    formatMessageWithAgency dtal msg "MsgRelease"
  forMachine dtal (AnyMessage msg@LSQ.MsgReAcquire{}) =
    formatMessageWithAgency dtal msg "MsgReAcquire"
  forMachine dtal (AnyMessage msg@LSQ.MsgDone{}) =
    formatMessageWithAgency dtal msg "MsgDone"

docTStateQuery :: Documented
      (BlockFetch.TraceLabelPeer peer
       (TraceSendRecv
       (LSQ.LocalStateQuery blk pt (Query blk))))
docTStateQuery =
   addDocumentedNamespace  ["Send"] docTState
    `addDocs` addDocumentedNamespace  ["Receive"] docTState

docTState :: Documented
      (BlockFetch.TraceLabelPeer peer
       (TraceSendRecv
         x))
docTState = Documented [
      DocMsg
        ["Acquire"]
        []
        "The client requests that the state as of a particular recent point on \
        \the server's chain (within K of the tip) be made available to query, \
        \and waits for confirmation or failure. \
        \\n \
        \From 'NodeToClient_V8' onwards if the point is not specified, current tip \
        \will be acquired.  For previous versions of the protocol 'point' must be \
        \given."
    , DocMsg
        ["Acquired"]
        []
        "The server can confirm that it has the state at the requested point."
    , DocMsg
        ["Failure"]
        []
        "The server can report that it cannot obtain the state for the \
        \requested point."
    , DocMsg
        ["Query"]
        []
        "The client can perform queries on the current acquired state."
    , DocMsg
        ["Result"]
        []
        "The server must reply with the queries."
    , DocMsg
        ["Release"]
        []
        "The client can instruct the server to release the state. This lets \
        \the server free resources."
    , DocMsg
        ["ReAcquire"]
        []
        "This is like 'MsgAcquire' but for when the client already has a \
        \state. By moveing to another state directly without a 'MsgRelease' it \
        \enables optimisations on the server side (e.g. moving to the state for \
        \the immediate next block). \
        \\n \
        \Note that failure to re-acquire is equivalent to 'MsgRelease', \
        \rather than keeping the exiting acquired state. \
        \\n \
        \From 'NodeToClient_V8' onwards if the point is not specified, current tip \
        \will be acquired.  For previous versions of the protocol 'point' must be \
        \given."
    , DocMsg
        ["Done"]
        []
        "The client can terminate the protocol."
  ]
