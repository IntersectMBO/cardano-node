{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Api.LocalChainSync
  ( getLocalTip
  ) where

import           Cardano.Prelude hiding (atomically, catch)

import           Cardano.Api.Typed
import           Control.Concurrent.STM

import           Ouroboros.Network.Protocol.ChainSync.Client (ChainSyncClient (..),
                     ClientStIdle (..), ClientStNext (..))


-- | Get the node's tip using the local chain sync protocol.
getLocalTip :: LocalNodeConnectInfo mode -> IO ChainTip
getLocalTip connctInfo = do
    resultVar <- newEmptyTMVarIO
    connectToLocalNode
      connctInfo
      nullLocalNodeClientProtocols {
        localChainSyncClient = chainSyncGetCurrentTip resultVar
      }
    atomically (takeTMVar resultVar)

chainSyncGetCurrentTip :: forall mode.
                          TMVar ChainTip
                       -> ChainSyncClient (BlockInMode mode) ChainPoint ChainTip IO ()
chainSyncGetCurrentTip tipVar =
  ChainSyncClient (pure clientStIdle)
 where
  clientStIdle :: ClientStIdle (BlockInMode mode) ChainPoint ChainTip IO ()
  clientStIdle =
    SendMsgRequestNext clientStNext (pure clientStNext)

  --TODO: we should be able to simply return the tip as the result with
  -- SendMsgDone and collect this as the result of the overall protocol.
  -- While currently we can have protocols return things, the current OuroborosApplication
  -- stuff gets in the way of returning an overall result, but that's being worked on,
  -- and this can be improved when that's ready.
  clientStNext :: ClientStNext (BlockInMode mode) ChainPoint ChainTip IO ()
  clientStNext = ClientStNext
    { recvMsgRollForward = \_blk tip -> ChainSyncClient $ do
        void $ atomically $ tryPutTMVar tipVar tip
        pure $ SendMsgDone ()
    , recvMsgRollBackward = \_point tip -> ChainSyncClient $ do
        void $ atomically $ tryPutTMVar tipVar tip
        pure $ SendMsgDone ()
    }
