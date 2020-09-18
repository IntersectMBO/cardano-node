{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Api.LocalChainSync
  ( getLocalTip
  ) where

import           Cardano.Prelude hiding (atomically, catch)

import           Cardano.Api.Typed
import           Control.Concurrent.STM

import           Ouroboros.Consensus.Ledger.Abstract (Query, ShowQuery)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx)
import           Ouroboros.Network.Block (Tip)
import           Ouroboros.Network.Protocol.ChainSync.Client (ClientStIdle (..), ClientStNext (..))
import           Ouroboros.Network.Util.ShowProxy (ShowProxy)


-- | Get the node's tip using the local chain sync protocol.
getLocalTip :: (ShowProxy block, ShowProxy (ApplyTxErr block),
                ShowProxy (Query block), ShowProxy (GenTx block),
                ShowQuery (Query block))
            => LocalNodeConnectInfo mode block -> IO (Tip block)
getLocalTip connctInfo = do
    resultVar <- newEmptyTMVarIO
    connectToLocalNode
      connctInfo
      nullLocalNodeClientProtocols {
        localChainSyncClient = Just (chainSyncGetCurrentTip resultVar)
      }
    atomically (takeTMVar resultVar)

chainSyncGetCurrentTip :: forall blk.
                          TMVar (Tip blk)
                       -> ChainSyncClient blk (Tip blk) IO ()
chainSyncGetCurrentTip tipVar =
  ChainSyncClient (pure clientStIdle)
 where
  clientStIdle :: ClientStIdle blk (Tip blk) IO ()
  clientStIdle =
    SendMsgRequestNext clientStNext (pure clientStNext)

  --TODO: we should be able to simply return the tip as the result with
  -- SendMsgDone and collect this as the result of the overall protocol.
  -- While currently we can have protocols return things, the current OuroborosApplication
  -- stuff gets in the way of returning an overall result, but that's being worked on,
  -- and this can be improved when that's ready.
  clientStNext :: ClientStNext blk (Tip blk) IO ()
  clientStNext = ClientStNext
    { recvMsgRollForward = \_blk tip -> ChainSyncClient $ do
        void $ atomically $ tryPutTMVar tipVar tip
        pure $ SendMsgDone ()
    , recvMsgRollBackward = \_point tip -> ChainSyncClient $ do
        void $ atomically $ tryPutTMVar tipVar tip
        pure $ SendMsgDone ()
    }
