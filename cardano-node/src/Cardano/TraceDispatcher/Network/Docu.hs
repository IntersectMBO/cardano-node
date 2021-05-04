{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# OPTIONS_GHC -Wno-deprecations  #-}

module Cardano.TraceDispatcher.Network.Docu
  ( docTChainSync
  , docTTxSubmission
  ) where

import           Cardano.Logging
import           Cardano.Prelude

import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx)
import           Ouroboros.Network.Block (Point, Serialised, Tip)
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.Codec (AnyMessageAndAgency (..))
import           Ouroboros.Network.Driver.Simple (TraceSendRecv (..))
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync (..),
                     Message (..))
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


docTChainSync :: Documented (BlockFetch.TraceLabelPeer peer (TraceSendRecv
    (ChainSync (Serialised blk) (Point blk) (Tip blk))))
docTChainSync = Documented [
      DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg (AnyMessageAndAgency protoStok MsgRequestNext)))
        []
        "Request the next update from the producer. The response can be a roll\
        \ forward, a roll back or wait."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg (AnyMessageAndAgency protoStok MsgAwaitReply)))
        []
        "Acknowledge the request but require the consumer to wait for the next\
        \ update. This means that the consumer is synced with the producer, and\
        \ the producer is waiting for its own chain state to change."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg (AnyMessageAndAgency protoStok MsgAwaitReply)))
        []
        "Tell the consumer to extend their chain with the given header.\
        \ \
        \ The message also tells the consumer about the head point of the producer."
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
        \ which party has agency in each state.\
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
