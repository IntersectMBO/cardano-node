{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Api.IPC.Monad
  ( LocalStateQueryScript
  , queryLocalState
  , localChainSync
  , sendMsgQuery
  , setupLocalStateQueryScript
  , runNtcProgram
  ) where

import Cardano.Api.Block
import Cardano.Api.IPC
import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Free
import Data.Either
import Data.Function
import Data.Maybe
import Data.Ord
import Shelley.Spec.Ledger.Scripts ()
import System.IO

import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as Net.Query
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as Net.Query
import qualified Ouroboros.Network.Protocol.ChainSync.Client as Net.Sync

-- | Monadic type for constructing local state queries.
--
-- Use 'sendMsgQuery' in a do block to construct queries of this type and convert
-- the expression to a 'Net.Query.LocalStateQueryClient' with 'setupLocalStateQueryScript'.
newtype LocalStateQueryScript block point query r m a = LocalStateQueryScript
  { runLocalStateQueryScript :: ContT (Net.Query.ClientStAcquired block point query m r) m a
  } deriving (Functor, Applicative, Monad, MonadIO)

data BlockInModeWrapper where
  BlockInModeWrapper :: BlockInMode mode -> BlockInModeWrapper

data Program query next
  = LocalChainSync (Either (ChainPoint, ChainTip) (BlockInModeWrapper, ChainTip) -> next)
  | forall result. QueryLocalState (query result) (result -> next)

deriving instance Functor (Program query)

localChainSync :: MonadIO m => FreeT (Program query) m (Either (ChainPoint, ChainTip) (BlockInModeWrapper, ChainTip))
localChainSync = liftF (LocalChainSync id)

queryLocalState :: MonadIO m => query result -> FreeT (Program query) m result
queryLocalState query = liftF (QueryLocalState query id)

-- | Use 'sendMsgQuery' in a do block to construct monadic local state queries.
sendMsgQuery :: Monad m => query a -> LocalStateQueryScript block point query r m a
sendMsgQuery q = LocalStateQueryScript . ContT $ \f -> pure $
  Net.Query.SendMsgQuery q $
    Net.Query.ClientStQuerying
    { Net.Query.recvMsgResult = f
    }

-- | Use 'sendMsgQuery' in a do block to construct monadic local state queries.
setupLocalStateQueryScript ::
     STM x
  -> Maybe ChainPoint
  -> NodeToClientVersion
  -> TMVar (Maybe (Either Net.Query.AcquireFailure a))
  -> LocalStateQueryScript (BlockInMode CardanoMode) ChainPoint (QueryInMode CardanoMode) () IO a
  -> Net.Query.LocalStateQueryClient (BlockInMode CardanoMode) ChainPoint (QueryInMode CardanoMode) IO ()
setupLocalStateQueryScript waitDone mPointVar' ntcVersion resultVar' f =
  LocalStateQueryClient $
    if ntcVersion >= NodeToClientV_8
      then do
        pure . Net.Query.SendMsgAcquire mPointVar' $
          Net.Query.ClientStAcquiring
          { Net.Query.recvMsgAcquired = runContT (runLocalStateQueryScript f) $ \result -> do
              atomically $ putTMVar resultVar' (Just (Right result))
              void $ atomically waitDone
              pure $ Net.Query.SendMsgRelease $ pure $ Net.Query.SendMsgDone ()

          , Net.Query.recvMsgFailure = \failure -> do
              atomically $ putTMVar resultVar' (Just (Left failure))
              void $ atomically waitDone
              pure $ Net.Query.SendMsgDone ()
          }
      else do
        atomically $ putTMVar resultVar' Nothing
        void $ atomically waitDone
        pure $ Net.Query.SendMsgDone ()

runNtcProgram ::
  forall query mode a. query ~ QueryInMode mode
  => LocalNodeConnectInfo mode
  -> Maybe ChainPoint
  -> FreeT (Program query) IO a
  -> IO a
runNtcProgram connectInfo mPointVar f = do
  tmvLocalStateQueryQuery <- newEmptyTMVarIO
  tmvLocalChainSyncQuery <- newEmptyTMVarIO

  connectToLocalNodeWithVersion
    connectInfo
    (\_ntcVersion -> LocalNodeClientProtocols
      { localChainSyncClient   = LocalChainSyncClient $ ChainSyncClient $ do
          query <- atomically $ readTMVar tmvLocalChainSyncQuery
          pure query
      , localStateQueryClient  = Just . LocalStateQueryClient . pure . Net.Query.SendMsgAcquire mPointVar $
          Net.Query.ClientStAcquiring
          { Net.Query.recvMsgAcquired = do
              query <- atomically $ readTMVar tmvLocalStateQueryQuery
              pure query

          , Net.Query.recvMsgFailure = \_failure -> do
              pure $ Net.Query.SendMsgDone ()
          }

      , localTxSubmissionClient = Nothing
      }
    )

  result <- go tmvLocalChainSyncQuery tmvLocalStateQueryQuery f
  atomically $ putTMVar tmvLocalStateQueryQuery $ Net.Query.SendMsgRelease $ pure $ Net.Query.SendMsgDone ()
  return result
  where go
          :: TMVar (Net.Sync.ClientStIdle (BlockInMode mode) ChainPoint ChainTip IO ())
          -> TMVar (Net.Query.ClientStAcquired block point query IO b)
          -> FreeT (Program query) IO a -> IO a
        go tmvLocalChainSyncQuery tmvLocalStateQueryQuery program = do
          step <- runFreeT program
          case step of
            Pure r -> return r
            Free (LocalChainSync next) -> do
              tmvResult <- newEmptyTMVarIO :: IO (TMVar (Either (ChainPoint, ChainTip) (BlockInModeWrapper, ChainTip)))
              atomically $ putTMVar tmvLocalChainSyncQuery $
                let clientStNext :: Net.Sync.ClientStNext (BlockInMode mode) ChainPoint ChainTip IO ()
                    clientStNext = Net.Sync.ClientStNext
                      { Net.Sync.recvMsgRollForward = \block tip -> ChainSyncClient $ do
                          atomically $ putTMVar tmvResult (Right (BlockInModeWrapper block, tip))
                          atomically $ readTMVar tmvLocalChainSyncQuery
                      , Net.Sync.recvMsgRollBackward = \point tip -> ChainSyncClient $ do
                          void . atomically $ putTMVar tmvResult (Left (point, tip))
                          atomically $ readTMVar tmvLocalChainSyncQuery
                      }
                in Net.Sync.SendMsgRequestNext clientStNext (pure clientStNext)

              result <- atomically $ readTMVar tmvResult

              go tmvLocalChainSyncQuery tmvLocalStateQueryQuery (next result)
            Free (QueryLocalState query next) -> do
              tmvResult <- newEmptyTMVarIO

              atomically $ putTMVar tmvLocalStateQueryQuery $ Net.Query.SendMsgQuery query $
                Net.Query.ClientStQuerying
                { Net.Query.recvMsgResult = \result -> do
                    atomically $ putTMVar tmvResult result
                    atomically $ readTMVar tmvLocalStateQueryQuery
                }

              result <- atomically $ readTMVar tmvResult

              go tmvLocalChainSyncQuery tmvLocalStateQueryQuery (next result)
