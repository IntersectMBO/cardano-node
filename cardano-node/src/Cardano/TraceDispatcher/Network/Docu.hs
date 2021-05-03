{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# OPTIONS_GHC -Wno-deprecations  #-}

module Cardano.TraceDispatcher.Network.Docu
  ( docTChainSync
  ) where

import           Cardano.Logging
import           Cardano.Prelude

import           Ouroboros.Network.Block (Serialised, Tip, Point)
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.Driver.Simple (TraceSendRecv (..))
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)

docTChainSync :: Documented (BlockFetch.TraceLabelPeer peer (TraceSendRecv
    (ChainSync (Serialised blk) (Point blk) (Tip blk))))
docTChainSync = Documented [

  ]
  --
  -- -- | The messages in the chain sync protocol.
  -- --
  -- -- In this protocol the consumer always initiates things and the producer
  -- -- replies.
  -- --
  -- data Message (ChainSync header point tip) from to where
  --
  --   -- | Request the next update from the producer. The response can be a roll
  --   -- forward, a roll back or wait.
  --   --
  --   MsgRequestNext :: Message (ChainSync header point tip)
  --                             StIdle (StNext StCanAwait)
  --
  --   -- | Acknowledge the request but require the consumer to wait for the next
  --   -- update. This means that the consumer is synced with the producer, and
  --   -- the producer is waiting for its own chain state to change.
  --   --
  --   MsgAwaitReply :: Message (ChainSync header point tip)
  --                            (StNext StCanAwait) (StNext StMustReply)
  --
  --   -- | Tell the consumer to extend their chain with the given header.
  --   --
  --   -- The message also tells the consumer about the head point of the producer.
  --   --
  --   MsgRollForward :: header -> tip
  --                  -> Message (ChainSync header point tip)
  --                             (StNext any) StIdle
  --
  --   -- | Tell the consumer to roll back to a given point on their chain.
  --   --
  --   -- The message also tells the consumer about the head point of the producer.
  --   --
  --   MsgRollBackward :: point -> tip
  --                   -> Message (ChainSync header point tip)
  --                              (StNext any) StIdle
  --
  --   -- | Ask the producer to try to find an improved intersection point between
  --   -- the consumer and producer's chains. The consumer sends a sequence of
  --   -- points and it is up to the producer to find the first intersection point
  --   -- on its chain and send it back to the consumer.
  --   --
  --   MsgFindIntersect :: [point]
  --                    -> Message (ChainSync header point tip)
  --                               StIdle StIntersect
  --
  --   -- | The reply to the consumer about an intersection found.
  --   -- The consumer can decide weather to send more points.
  --   --
  --   -- The message also tells the consumer about the head point of the producer.
  --   --
  --   MsgIntersectFound  :: point -> tip
  --                      -> Message (ChainSync header point tip)
  --                                 StIntersect StIdle
  --
  --   -- | The reply to the consumer that no intersection was found: none of the
  --   -- points the consumer supplied are on the producer chain.
  --   --
  --   -- The message also tells the consumer about the head point of the producer.
  --   --
  --   MsgIntersectNotFound :: tip
  --                        -> Message (ChainSync header point tip)
  --                                   StIntersect StIdle
  --
  --   -- | Terminating messages
  --   --
  --   MsgDone :: Message (ChainSync header point tip)
  --                      StIdle StDone
  --
  -- -- | We have to explain to the framework what our states mean, in terms of
  -- -- which party has agency in each state.
  -- --
  -- -- Idle states are where it is for the client to send a message,
  -- -- busy states are where the server is expected to send a reply.
  -- --
