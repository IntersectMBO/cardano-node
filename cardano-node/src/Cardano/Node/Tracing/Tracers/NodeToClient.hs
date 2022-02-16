{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
import           Data.Text (pack)
import           Network.TypedProtocol.Codec (AnyMessageAndAgency (..))
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


instance LogFormatting (AnyMessageAndAgency ps)
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

    severityTChainSync'' (AnyMessageAndAgency _agency msg) = severityTChainSync''' msg

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
namesForTChainSync (BlockFetch.TraceLabelPeer _ v) = "NodeToClient" : namesTChainSync v
  where

    namesTChainSync (TraceSendMsg msg) = "Send" : namesTChainSync' msg
    namesTChainSync (TraceRecvMsg msg) = "Receive" : namesTChainSync' msg

    namesTChainSync' (AnyMessageAndAgency _agency msg) = namesTChainSync'' msg

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


instance LogFormatting (AnyMessageAndAgency (ChainSync blk pt tip)) where
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgRequestNext{}) =
     mconcat [ "kind" .= String "MsgRequestNext"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgAwaitReply{}) =
     mconcat [ "kind" .= String "MsgAwaitReply"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgRollForward{}) =
     mconcat [ "kind" .= String "MsgRollForward"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgRollBackward{}) =
     mconcat [ "kind" .= String "MsgRollBackward"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgFindIntersect{}) =
     mconcat [ "kind" .= String "MsgFindIntersect"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgIntersectFound{}) =
     mconcat [ "kind" .= String "MsgIntersectFound"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgIntersectNotFound{}) =
     mconcat [ "kind" .= String "MsgIntersectNotFound"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgDone{}) =
     mconcat [ "kind" .= String "MsgDone"
              , "agency" .= String (pack $ show stok)
              ]

docTChainSyncNodeToClient :: Documented (BlockFetch.TraceLabelPeer peer (TraceSendRecv
    (ChainSync x (Point blk) (Tip blk))))
docTChainSyncNodeToClient =
    addDocumentedNamespace  ["NodeToClient", "Send"] docTChainSync
    `addDocs` addDocumentedNamespace  ["NodeToClient", "Recieve"] docTChainSync


docTChainSyncNodeToNode :: Documented (BlockFetch.TraceLabelPeer peer (TraceSendRecv
    (ChainSync x (Point blk) (Tip blk))))
docTChainSyncNodeToNode =
    addDocumentedNamespace  ["NodeToNode", "Send"] docTChainSync
    `addDocs` addDocumentedNamespace  ["NodeToNode", "Recieve"] docTChainSync

docTChainSyncNodeToNodeSerisalised :: Documented (BlockFetch.TraceLabelPeer peer (TraceSendRecv
    (ChainSync x (Point blk) (Tip blk))))
docTChainSyncNodeToNodeSerisalised =
    addDocumentedNamespace  ["NodeToNode", "Send"] docTChainSync
    `addDocs` addDocumentedNamespace  ["NodeToNode", "Recieve"] docTChainSync


docTChainSync :: Documented (BlockFetch.TraceLabelPeer peer (TraceSendRecv
    (ChainSync x (Point blk) (Tip blk))))
docTChainSync = Documented [
      DocMsg
        ["RequestNext"]
        []
        "Request the next update from the producer. The response can be a roll\
        \forward, a roll back or wait."
    , DocMsg
        ["AwaitReply"]
        []
        "Acknowledge the request but require the consumer to wait for the next\
        \update. This means that the consumer is synced with the producer, and\
        \the producer is waiting for its own chain state to change."
    , DocMsg
        ["RollForward"]
        []
        "Tell the consumer to extend their chain with the given header.\
        \\n\
        \The message also tells the consumer about the head point of the producer."
    , DocMsg
        ["RollBackward"]
        []
        "Tell the consumer to roll back to a given point on their chain.\
        \\n\
        \The message also tells the consumer about the head point of the producer."
    , DocMsg
        ["FindIntersect"]
        []
        "Ask the producer to try to find an improved intersection point between\
        \the consumer and producer's chains. The consumer sends a sequence of\
        \points and it is up to the producer to find the first intersection point\
        \on its chain and send it back to the consumer."
    , DocMsg
        ["IntersectFound"]
        []
        "The reply to the consumer about an intersection found.\
        \The consumer can decide weather to send more points.\
        \\n\
        \The message also tells the consumer about the head point of the producer."
    , DocMsg
        ["IntersectNotFound"]
        []
        "The reply to the consumer that no intersection was found: none of the\
        \points the consumer supplied are on the producer chain.\
        \\n\
        \The message also tells the consumer about the head point of the producer."
    , DocMsg
        ["Done"]
        []
        "We have to explain to the framework what our states mean, in terms of\
        \which party has agency in each state.\
        \\n\
        \Idle states are where it is for the client to send a message,\
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

    namesForTTxMonitor'' (AnyMessageAndAgency _agency msg) = namesForTTxMonitor''' msg

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

instance LogFormatting (AnyMessageAndAgency (LTM.LocalTxMonitor txid tx slotNo)) where
  forMachine _dtal (AnyMessageAndAgency stok LTM.MsgAcquire {}) =
    mconcat [ "kind" .= String "MsgAcquire"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LTM.MsgAcquired {}) =
    mconcat [ "kind" .= String "MsgAcquired"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LTM.MsgAwaitAcquire {}) =
    mconcat [ "kind" .= String "MsgAwaitAcquire"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LTM.MsgNextTx {}) =
    mconcat [ "kind" .= String "MsgNextTx"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LTM.MsgReplyNextTx {}) =
    mconcat [ "kind" .= String "MsgReplyNextTx"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LTM.MsgHasTx {}) =
    mconcat [ "kind" .= String "MsgHasTx"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LTM.MsgReplyHasTx {}) =
    mconcat [ "kind" .= String "MsgReplyHasTx"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LTM.MsgGetSizes {}) =
    mconcat [ "kind" .= String "MsgGetSizes"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LTM.MsgReplyGetSizes {}) =
    mconcat [ "kind" .= String "MsgReplyGetSizes"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LTM.MsgRelease {}) =
    mconcat [ "kind" .= String "MsgRelease"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LTM.MsgDone {}) =
    mconcat [ "kind" .= String "MsgDone"
             , "agency" .= String (pack $ show stok)
             ]

docTTxMonitor :: Documented
  (TraceLabelPeer
     localPeer
     (TraceSendRecv
        (LTM.LocalTxMonitor
           (GenTxId blk) (GenTx blk) SlotNo)))
docTTxMonitor =
  addDocumentedNamespace  ["Send"] docTState
   `addDocs` addDocumentedNamespace  ["Recieve"] docTState


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

    severityTTxSubmission'' (AnyMessageAndAgency _agency msg) = severityTTxSubmission''' msg

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

    namesTTxSubmission' (AnyMessageAndAgency _agency msg) = namesTTxSubmission'' msg

    namesTTxSubmission'' :: Message
                                    (LTS.LocalTxSubmission tx reject) from to
                                  -> [Text]
    namesTTxSubmission'' LTS.MsgSubmitTx {} = ["SubmitTx"]
    namesTTxSubmission'' LTS.MsgAcceptTx {} = ["AcceptTx"]
    namesTTxSubmission'' LTS.MsgRejectTx {} = ["RejectTx"]
    namesTTxSubmission'' LTS.MsgDone {}     = ["Done"]


instance LogFormatting (AnyMessageAndAgency (LTS.LocalTxSubmission tx err)) where
  forMachine _dtal (AnyMessageAndAgency stok LTS.MsgSubmitTx{}) =
    mconcat [ "kind" .= String "MsgSubmitTx"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LTS.MsgAcceptTx{}) =
    mconcat [ "kind" .= String "MsgAcceptTx"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LTS.MsgRejectTx{}) =
    mconcat [ "kind" .= String "MsgRejectTx"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LTS.MsgDone{}) =
    mconcat [ "kind" .= String "MsgDone"
             , "agency" .= String (pack $ show stok)
             ]

docTTxSubmission :: Documented
  (BlockFetch.TraceLabelPeer
     localPeer
     (TraceSendRecv
        (LTS.LocalTxSubmission
           (GenTx blk) (ApplyTxErr blk))))
docTTxSubmission =
  addDocumentedNamespace  ["Send"] docTTxSubmission'
   `addDocs` addDocumentedNamespace  ["Recieve"] docTTxSubmission'

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
        "The server can reply to inform the client that it has accepted the\
        \transaction."
    , DocMsg
        ["RejectTx"]
        []
        "The server can reply to inform the client that it has rejected the\
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

    severityTStateQuery'' (AnyMessageAndAgency _agency msg) = severityTStateQuery''' msg

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

    namesForTStateQuery'' (AnyMessageAndAgency _agency msg) = namesForTStateQuery''' msg

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

instance LSQ.ShowQuery (Query blk)
      => LogFormatting (AnyMessageAndAgency (LSQ.LocalStateQuery blk pt (Query blk))) where
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgAcquire{}) =
    mconcat [ "kind" .= String "MsgAcquire"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgAcquired{}) =
    mconcat [ "kind" .= String "MsgAcquired"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgFailure{}) =
    mconcat [ "kind" .= String "MsgFailure"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgQuery{}) =
    mconcat [ "kind" .= String "MsgQuery"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgResult{}) =
    mconcat [ "kind" .= String "MsgResult"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgRelease{}) =
    mconcat [ "kind" .= String "MsgRelease"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgReAcquire{}) =
    mconcat [ "kind" .= String "MsgReAcquire"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgDone{}) =
    mconcat [ "kind" .= String "MsgDone"
             , "agency" .= String (pack $ show stok)
             ]

docTStateQuery :: Documented
      (BlockFetch.TraceLabelPeer peer
       (TraceSendRecv
       (LSQ.LocalStateQuery blk pt (Query blk))))
docTStateQuery =
   addDocumentedNamespace  ["Send"] docTState
    `addDocs` addDocumentedNamespace  ["Recieve"] docTState

docTState :: Documented
      (BlockFetch.TraceLabelPeer peer
       (TraceSendRecv
         x))
docTState = Documented [
      DocMsg
        ["Acquire"]
        []
        "The client requests that the state as of a particular recent point on\
        \the server's chain (within K of the tip) be made available to query,\
        \and waits for confirmation or failure.\
        \\n\
        \From 'NodeToClient_V8' onwards if the point is not specified, current tip\
        \will be acquired.  For previous versions of the protocol 'point' must be\
        \given."
    , DocMsg
        ["Acquired"]
        []
        "The server can confirm that it has the state at the requested point."
    , DocMsg
        ["Failure"]
        []
        "The server can report that it cannot obtain the state for the\
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
        "The client can instruct the server to release the state. This lets\
        \the server free resources."
    , DocMsg
        ["ReAcquire"]
        []
        "This is like 'MsgAcquire' but for when the client already has a\
        \state. By moveing to another state directly without a 'MsgRelease' it\
        \enables optimisations on the server side (e.g. moving to the state for\
        \the immediate next block).\
        \\n\
        \Note that failure to re-acquire is equivalent to 'MsgRelease',\
        \rather than keeping the exiting acquired state.\
        \\n\
        \From 'NodeToClient_V8' onwards if the point is not specified, current tip\
        \will be acquired.  For previous versions of the protocol 'point' must be\
        \given."
    , DocMsg
        ["Done"]
        []
        "The client can terminate the protocol."
  ]
