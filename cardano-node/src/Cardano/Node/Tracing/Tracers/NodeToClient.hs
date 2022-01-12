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
  , docTChainSync

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

import           Ouroboros.Consensus.Ledger.Query (Query)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx)

import           Ouroboros.Network.Block (Point, Serialised, Tip)
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.Driver.Simple (TraceSendRecv (..))
import           Ouroboros.Network.Protocol.ChainSync.Type as ChainSync
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as LSQ
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type as LTS


instance LogFormatting (AnyMessageAndAgency ps)
      => LogFormatting (TraceSendRecv ps) where
  forMachine dtal (TraceSendMsg m) = mkObject
    [ "kind" .= String "Send" , "msg" .= forMachine dtal m ]
  forMachine dtal (TraceRecvMsg m) = mkObject
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
    namesTChainSync (TraceRecvMsg msg) = "Recieve" : namesTChainSync' msg

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
     mkObject [ "kind" .= String "MsgRequestNext"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgAwaitReply{}) =
     mkObject [ "kind" .= String "MsgAwaitReply"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgRollForward{}) =
     mkObject [ "kind" .= String "MsgRollForward"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgRollBackward{}) =
     mkObject [ "kind" .= String "MsgRollBackward"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgFindIntersect{}) =
     mkObject [ "kind" .= String "MsgFindIntersect"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgIntersectFound{}) =
     mkObject [ "kind" .= String "MsgIntersectFound"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgIntersectNotFound{}) =
     mkObject [ "kind" .= String "MsgIntersectNotFound"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgDone{}) =
     mkObject [ "kind" .= String "MsgDone"
              , "agency" .= String (pack $ show stok)
              ]


docTChainSync :: Documented (BlockFetch.TraceLabelPeer peer (TraceSendRecv
    (ChainSync x (Point blk) (Tip blk))))
docTChainSync = Documented [
      DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg (AnyMessageAndAgency anyProto MsgRequestNext)))
        []
        "Request the next update from the producer. The response can be a roll\
        \forward, a roll back or wait."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg (AnyMessageAndAgency anyProto MsgAwaitReply)))
        []
        "Acknowledge the request but require the consumer to wait for the next\
        \update. This means that the consumer is synced with the producer, and\
        \the producer is waiting for its own chain state to change."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg (AnyMessageAndAgency anyProto MsgAwaitReply)))
        []
        "Tell the consumer to extend their chain with the given header.\
        \\n\
        \The message also tells the consumer about the head point of the producer."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg (AnyMessageAndAgency anyProto
            (MsgRollForward anyProto anyProto))))
        []
        "Tell the consumer to extend their chain with the given header.\
        \\n\
        \The message also tells the consumer about the head point of the producer."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg (AnyMessageAndAgency anyProto
            (MsgRollBackward anyProto anyProto))))
        []
        "Tell the consumer to roll back to a given point on their chain.\
        \\n\
        \The message also tells the consumer about the head point of the producer."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg (AnyMessageAndAgency anyProto
            (MsgFindIntersect [anyProto]))))
        []
        "Ask the producer to try to find an improved intersection point between\
        \the consumer and producer's chains. The consumer sends a sequence of\
        \points and it is up to the producer to find the first intersection point\
        \on its chain and send it back to the consumer."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg (AnyMessageAndAgency anyProto
            (MsgIntersectFound anyProto anyProto))))
        []
        "The reply to the consumer about an intersection found.\
        \The consumer can decide weather to send more points.\
        \\n\
        \The message also tells the consumer about the head point of the producer."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg (AnyMessageAndAgency anyProto
            (MsgIntersectNotFound anyProto))))
        []
        "The reply to the consumer that no intersection was found: none of the\
        \points the consumer supplied are on the producer chain.\
        \\n\
        \The message also tells the consumer about the head point of the producer."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg (AnyMessageAndAgency anyProto
            MsgDone)))
        []
        "We have to explain to the framework what our states mean, in terms of\
        \which party has agency in each state.\
        \\n\
        \Idle states are where it is for the client to send a message,\
        \busy states are where the server is expected to send a reply."
  ]

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
    namesTTxSubmission (TraceRecvMsg msg) = "Recieve" : namesTTxSubmission' msg

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
    mkObject [ "kind" .= String "MsgSubmitTx"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LTS.MsgAcceptTx{}) =
    mkObject [ "kind" .= String "MsgAcceptTx"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LTS.MsgRejectTx{}) =
    mkObject [ "kind" .= String "MsgRejectTx"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LTS.MsgDone{}) =
    mkObject [ "kind" .= String "MsgDone"
             , "agency" .= String (pack $ show stok)
             ]

docTTxSubmission :: Documented
   (BlockFetch.TraceLabelPeer
      localPeer
      (TraceSendRecv
         (LTS.LocalTxSubmission
            (GenTx blk) (ApplyTxErr blk))))
docTTxSubmission = Documented [
      DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg (AnyMessageAndAgency anyProto (LTS.MsgSubmitTx anyProto))))
        []
        "The client submits a single transaction and waits a reply."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg (AnyMessageAndAgency anyProto LTS.MsgAcceptTx)))
        []
        "The server can reply to inform the client that it has accepted the\
        \transaction."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg (AnyMessageAndAgency anyProto (LTS.MsgRejectTx anyProto))))
        []
        "The server can reply to inform the client that it has rejected the\
        \transaction. A reason for the rejection is included."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg (AnyMessageAndAgency anyProto LTS.MsgDone)))
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
    namesForTStateQuery' (TraceSendMsg msg) = namesForTStateQuery'' msg
    namesForTStateQuery' (TraceRecvMsg msg) = namesForTStateQuery'' msg

    namesForTStateQuery'' (AnyMessageAndAgency _agency msg) = namesForTStateQuery''' msg

    namesForTStateQuery''' :: Message
                                    (LSQ.LocalStateQuery block point query1) from to
                                  -> [Text]

    namesForTStateQuery''' LSQ.MsgAcquire {}   = ["Acquire"]
    namesForTStateQuery''' LSQ.MsgAcquired {}  = ["Acquired"]
    namesForTStateQuery''' LSQ.MsgFailure {}   = ["Acquired"]
    namesForTStateQuery''' LSQ.MsgQuery {}     = ["Query"]
    namesForTStateQuery''' LSQ.MsgResult {}    = ["Result"]
    namesForTStateQuery''' LSQ.MsgRelease {}   = ["Release"]
    namesForTStateQuery''' LSQ.MsgReAcquire {} = ["ReAcquire"]
    namesForTStateQuery''' LSQ.MsgDone {}      = ["Done"]

instance (forall result. Show (Query blk result))
      => LogFormatting (AnyMessageAndAgency (LSQ.LocalStateQuery blk pt (Query blk))) where
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgAcquire{}) =
    mkObject [ "kind" .= String "MsgAcquire"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgAcquired{}) =
    mkObject [ "kind" .= String "MsgAcquired"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgFailure{}) =
    mkObject [ "kind" .= String "MsgFailure"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgQuery{}) =
    mkObject [ "kind" .= String "MsgQuery"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgResult{}) =
    mkObject [ "kind" .= String "MsgResult"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgRelease{}) =
    mkObject [ "kind" .= String "MsgRelease"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgReAcquire{}) =
    mkObject [ "kind" .= String "MsgReAcquire"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgDone{}) =
    mkObject [ "kind" .= String "MsgDone"
             , "agency" .= String (pack $ show stok)
             ]

docTStateQuery :: Documented
      (BlockFetch.TraceLabelPeer peer
       (TraceSendRecv
         (LSQ.LocalStateQuery blk (Point blk) query)))
docTStateQuery = Documented [
      DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg (AnyMessageAndAgency anyProto (LSQ.MsgAcquire Nothing))))
        []
        "The client requests that the state as of a particular recent point on\
        \the server's chain (within K of the tip) be made available to query,\
        \and waits for confirmation or failure.\
        \\n\
        \From 'NodeToClient_V8' onwards if the point is not specified, current tip\
        \will be acquired.  For previous versions of the protocol 'point' must be\
        \given."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg (AnyMessageAndAgency anyProto LSQ.MsgAcquired)))
        []
        "The server can confirm that it has the state at the requested point."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              (LSQ.MsgFailure anyProto))))
        []
        "The server can report that it cannot obtain the state for the\
        \requested point."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              (LSQ.MsgQuery anyProto))))
        []
        "The client can perform queries on the current acquired state."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              (LSQ.MsgResult anyProto anyProto))))
        []
        "The server must reply with the queries."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              LSQ.MsgRelease)))
        []
        "The client can instruct the server to release the state. This lets\
        \the server free resources."
    , DocMsg
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              (LSQ.MsgReAcquire Nothing))))
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
        (BlockFetch.TraceLabelPeer anyProto
          (TraceSendMsg
            (AnyMessageAndAgency anyProto
              LSQ.MsgDone)))
        []
        "The client can terminate the protocol."
  ]
