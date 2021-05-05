{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# OPTIONS_GHC -Wno-deprecations  #-}

module Cardano.TraceDispatcher.Network.Docu
  ( docTChainSync
  , docTTxSubmission
  , docTStateQuery
  , docTBlockFetch
  ) where

import           Cardano.Logging
import           Cardano.Prelude

import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx)

import           Ouroboros.Network.Block (Point, Tip)
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.Codec (AnyMessageAndAgency (..))
import           Ouroboros.Network.Driver.Simple (TraceSendRecv (..))
import           Ouroboros.Network.Protocol.BlockFetch.Type
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync (..),
                     Message (..))
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as LSQ
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type as LTS


protoHeader :: header
protoHeader = undefined

protoPoint :: Point blk
protoPoint = undefined

protoTip :: Tip blk
protoTip = undefined

protoPeer :: peer
protoPeer = undefined

protoStok :: stok
protoStok = undefined

protoTx :: tx
protoTx = undefined

protoAcquireFailure :: LSQ.AcquireFailure
protoAcquireFailure = undefined

docTChainSync :: Documented (BlockFetch.TraceLabelPeer peer (TraceSendRecv
    (ChainSync x (Point blk) (Tip blk))))
docTChainSync = Documented [
      DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg (AnyMessageAndAgency protoStok MsgRequestNext)))
        []
        "Request the next update from the producer. The response can be a roll\
        \forward, a roll back or wait."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg (AnyMessageAndAgency protoStok MsgAwaitReply)))
        []
        "Acknowledge the request but require the consumer to wait for the next\
        \update. This means that the consumer is synced with the producer, and\
        \the producer is waiting for its own chain state to change."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg (AnyMessageAndAgency protoStok MsgAwaitReply)))
        []
        "Tell the consumer to extend their chain with the given header.\
        \ \
        \The message also tells the consumer about the head point of the producer."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg (AnyMessageAndAgency protoStok
            (MsgRollForward protoHeader protoTip))))
        []
        "Tell the consumer to extend their chain with the given header.\
        \ \
        \The message also tells the consumer about the head point of the producer."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg (AnyMessageAndAgency protoStok
            (MsgRollBackward protoPoint protoTip))))
        []
        "Tell the consumer to roll back to a given point on their chain.\
        \\
        \The message also tells the consumer about the head point of the producer."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg (AnyMessageAndAgency protoStok
            (MsgFindIntersect [protoPoint]))))
        []
        "Ask the producer to try to find an improved intersection point between\
        \the consumer and producer's chains. The consumer sends a sequence of\
        \points and it is up to the producer to find the first intersection point\
        \on its chain and send it back to the consumer."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg (AnyMessageAndAgency protoStok
            (MsgIntersectFound protoPoint protoTip))))
        []
        "The reply to the consumer about an intersection found.\
        \The consumer can decide weather to send more points.\
        \\
        \The message also tells the consumer about the head point of the producer."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg (AnyMessageAndAgency protoStok
            (MsgIntersectNotFound protoTip))))
        []
        "The reply to the consumer that no intersection was found: none of the\
        \points the consumer supplied are on the producer chain.\
        \\
        \The message also tells the consumer about the head point of the producer."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg (AnyMessageAndAgency protoStok
            MsgDone)))
        []
        "We have to explain to the framework what our states mean, in terms of\
        \which party has agency in each state.\
        \ \
        \Idle states are where it is for the client to send a message,\
        \busy states are where the server is expected to send a reply."
  ]

docTTxSubmission :: Documented
   (BlockFetch.TraceLabelPeer
      localPeer
      (TraceSendRecv
         (LTS.LocalTxSubmission
            (GenTx blk) (ApplyTxErr blk))))
docTTxSubmission = Documented [
      DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg (AnyMessageAndAgency protoStok (LTS.MsgSubmitTx protoTx))))
        []
        "The client submits a single transaction and waits a reply."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg (AnyMessageAndAgency protoStok LTS.MsgAcceptTx)))
        []
        "The server can reply to inform the client that it has accepted the\
        \transaction."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg (AnyMessageAndAgency protoStok (LTS.MsgRejectTx undefined))))
        []
        "The server can reply to inform the client that it has rejected the\
        \transaction. A reason for the rejection is included."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg (AnyMessageAndAgency protoStok LTS.MsgDone)))
        []
        "The client can terminate the protocol."
  ]

docTStateQuery :: Documented
      (BlockFetch.TraceLabelPeer peer
       (TraceSendRecv
         (LSQ.LocalStateQuery blk (Point blk) query)))
docTStateQuery = Documented [
      DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg (AnyMessageAndAgency protoStok (LSQ.MsgAcquire Nothing))))
        []
        "The client requests that the state as of a particular recent point on\
        \the server's chain (within K of the tip) be made available to query,\
        \and waits for confirmation or failure.\
        \\
        \From 'NodeToClient_V8' onwards if the point is not specified, current tip\
        \will be acquired.  For previous versions of the protocol 'point' must be\
        \given."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg (AnyMessageAndAgency protoStok LSQ.MsgAcquired)))
        []
        "The server can confirm that it has the state at the requested point."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency protoStok
              (LSQ.MsgFailure protoAcquireFailure))))
        []
        "The server can report that it cannot obtain the state for the\
        \requested point."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency protoStok
              (LSQ.MsgQuery (undefined :: query result)))))
        []
        "The client can perform queries on the current acquired state."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency protoStok
              (LSQ.MsgResult (undefined :: query result) (undefined :: result)))))
        []
        "The server must reply with the queries."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency protoStok
              LSQ.MsgRelease)))
        []
        "The client can instruct the server to release the state. This lets\
        \the server free resources."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency protoStok
              (LSQ.MsgReAcquire Nothing))))
        []
        "This is like 'MsgAcquire' but for when the client already has a\
        \state. By moveing to another state directly without a 'MsgRelease' it\
        \enables optimisations on the server side (e.g. moving to the state for\
        \the immediate next block).\
        \\
        \Note that failure to re-acquire is equivalent to 'MsgRelease',\
        \rather than keeping the exiting acquired state.\
        \\
        \From 'NodeToClient_V8' onwards if the point is not specified, current tip\
        \will be acquired.  For previous versions of the protocol 'point' must be\
        \given."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency protoStok
              LSQ.MsgDone)))
        []
        "The client can terminate the protocol."
  ]

docTBlockFetch :: Documented
      (BlockFetch.TraceLabelPeer peer
       (TraceSendRecv
         (BlockFetch blk (Point blk))))
docTBlockFetch = Documented [
      ]
      --DocMsg
