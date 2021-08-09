{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Cardano.Api.IPC.Monad
  ( LocalStateQueryExpr
  , executeQueryLocalState
  , executeQueryLocalStateWithChainSync
  , queryExpr
  , determineEraExpr
  ) where

import Cardano.Api.Block
import Cardano.Api.Eras
import Cardano.Api.IPC
import Cardano.Api.Modes
import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Except
import Data.Either
import Data.Function
import Data.Maybe
import Shelley.Spec.Ledger.Scripts ()
import System.IO

import qualified Ouroboros.Network.Protocol.ChainSync.Client as Net.Sync
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as Net.Query
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as Net.Query

{- HLINT ignore "Use let" -}

-- | Monadic type for constructing local state query expressions.
--
-- Use 'queryExpr' in a do block to construct queries of this type and convert
-- the expression to a 'Net.Query.LocalStateQueryClient' with 'setupLocalStateQueryExpr'.
--
-- Some consideration was made to use Applicative instead of Monad as the abstraction in
-- order to support pipelining, but we actually have a fair amount of code where the next
-- query depends on the result of the former and therefore actually need Monad.
--
-- In order to make pipelining still possible we can explore the use of Selective Functors
-- which would allow us to straddle both worlds.
newtype LocalStateQueryExpr block point query r m a = LocalStateQueryExpr
  { runLocalStateQueryExpr :: ContT (Net.Query.ClientStAcquired block point query m r) m a
  } deriving (Functor, Applicative, Monad, MonadIO)

-- | Execute a local state query expression.
executeQueryLocalState
  :: LocalNodeConnectInfo mode
  -> Maybe ChainPoint
  -> (NodeToClientVersion -> ExceptT e (LocalStateQueryExpr (BlockInMode mode) ChainPoint (QueryInMode mode) () IO) a)
  -> IO (Either AcquireFailure (Either e a))
executeQueryLocalState connectInfo mpoint f = do
  tmvResultLocalState <- newEmptyTMVarIO
  let waitResult = readTMVar tmvResultLocalState

  connectToLocalNodeWithVersion
    connectInfo
    (\ntcVersion ->
      LocalNodeClientProtocols
      { localChainSyncClient    = NoLocalChainSyncClient
      , localStateQueryClient   = Just $ setupLocalStateQueryExpr waitResult mpoint tmvResultLocalState (runExceptT (f ntcVersion))
      , localTxSubmissionClient = Nothing
      }
    )

  atomically waitResult

-- | Execute a local state query expression concurrently with a chain sync.
executeQueryLocalStateWithChainSync
  :: LocalNodeConnectInfo mode
  -> Maybe ChainPoint
  -> (NodeToClientVersion -> ExceptT e (LocalStateQueryExpr (BlockInMode mode) ChainPoint (QueryInMode mode) () IO) a)
  -> IO (ChainTip, Either AcquireFailure (Either e a))
executeQueryLocalStateWithChainSync connectInfo mpoint f = do
  tmvResultLocalState <- newEmptyTMVarIO
  tmvResultChainTip <- newEmptyTMVarIO

  let waitResult = (,)
        <$> readTMVar tmvResultChainTip
        <*> readTMVar tmvResultLocalState

  connectToLocalNodeWithVersion
    connectInfo
    (\ntcVersion ->
      LocalNodeClientProtocols
      { localChainSyncClient    = LocalChainSyncClient $ chainSyncGetCurrentTip waitResult tmvResultChainTip
      , localStateQueryClient   = Just $ setupLocalStateQueryExpr waitResult mpoint tmvResultLocalState (runExceptT (f ntcVersion))
      , localTxSubmissionClient = Nothing
      }
    )

  atomically waitResult

  where
    chainSyncGetCurrentTip
      :: STM b
      -> TMVar  ChainTip
      -> ChainSyncClient (BlockInMode mode) ChainPoint ChainTip IO ()
    chainSyncGetCurrentTip waitDone tipVar =
      ChainSyncClient $ pure clientStIdle
      where
        clientStIdle :: Net.Sync.ClientStIdle (BlockInMode mode) ChainPoint ChainTip IO ()
        clientStIdle =
          Net.Sync.SendMsgRequestNext clientStNext (pure clientStNext)

        clientStNext :: Net.Sync.ClientStNext (BlockInMode mode) ChainPoint ChainTip IO ()
        clientStNext = Net.Sync.ClientStNext
          { Net.Sync.recvMsgRollForward = \_block tip -> ChainSyncClient $ do
              void . atomically $ putTMVar tipVar tip
              void $ atomically waitDone
              pure $ Net.Sync.SendMsgDone ()
          , Net.Sync.recvMsgRollBackward = \_point tip -> ChainSyncClient $ do
              void . atomically $ putTMVar tipVar tip
              void $ atomically waitDone
              pure $ Net.Sync.SendMsgDone ()
          }

-- | Use 'queryExpr' in a do block to construct monadic local state queries.
setupLocalStateQueryExpr ::
     STM x
  -> Maybe ChainPoint
  -> TMVar (Either Net.Query.AcquireFailure a)
  -> LocalStateQueryExpr (BlockInMode mode) ChainPoint (QueryInMode mode) () IO a
  -> Net.Query.LocalStateQueryClient (BlockInMode mode) ChainPoint (QueryInMode mode) IO ()
setupLocalStateQueryExpr waitDone mPointVar' resultVar' f =
  LocalStateQueryClient . pure . Net.Query.SendMsgAcquire mPointVar' $
    Net.Query.ClientStAcquiring
    { Net.Query.recvMsgAcquired = runContT (runLocalStateQueryExpr f) $ \result -> do
        atomically $ putTMVar resultVar' (Right result)
        void $ atomically waitDone
        pure $ Net.Query.SendMsgRelease $ pure $ Net.Query.SendMsgDone ()

    , Net.Query.recvMsgFailure = \failure -> do
        atomically $ putTMVar resultVar' (Left failure)
        void $ atomically waitDone
        pure $ Net.Query.SendMsgDone ()
    }

-- | Use 'queryExpr' in a do block to construct monadic local state queries.
queryExpr :: Monad m => query a -> ExceptT e (LocalStateQueryExpr block point query r m) a
queryExpr q = ExceptT $ do
  r <- LocalStateQueryExpr . ContT $ \f -> pure $
    Net.Query.SendMsgQuery q $
      Net.Query.ClientStQuerying
      { Net.Query.recvMsgResult = f
      }
  return (Right r)

-- | A monad expresion that determines what era the node is in.
determineEraExpr :: Monad m
  => ConsensusModeParams mode
  -> ExceptT e (LocalStateQueryExpr block point (QueryInMode mode) r m) AnyCardanoEra
determineEraExpr cModeParams =
  case consensusModeOnly cModeParams of
    ByronMode -> return $ AnyCardanoEra ByronEra
    ShelleyMode -> return $ AnyCardanoEra ShelleyEra
    CardanoMode -> queryExpr $ QueryCurrentEra CardanoModeIsMultiEra
