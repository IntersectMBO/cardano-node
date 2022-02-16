{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Cardano.Api.IPC.Monad
  ( LocalStateQueryExpr
  , executeLocalStateQueryExpr
  , queryExpr
  , determineEraExpr
  ) where

import           Cardano.Api.Block
import           Cardano.Api.Eras
import           Cardano.Api.IPC
import           Cardano.Api.Modes
import           Cardano.Ledger.Shelley.Scripts ()
import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Cont
import           Data.Either
import           Data.Function
import           Data.Maybe
import           System.IO

import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as Net.Query
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as Net.Query

{- HLINT ignore "Use const" -}
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
executeLocalStateQueryExpr
  :: LocalNodeConnectInfo mode
  -> Maybe ChainPoint
  -> (NodeToClientVersion -> LocalStateQueryExpr (BlockInMode mode) ChainPoint (QueryInMode mode) () IO a)
  -> IO (Either AcquireFailure a)
executeLocalStateQueryExpr connectInfo mpoint f = do
  tmvResultLocalState <- newEmptyTMVarIO
  let waitResult = readTMVar tmvResultLocalState

  connectToLocalNodeWithVersion
    connectInfo
    (\ntcVersion ->
      LocalNodeClientProtocols
      { localChainSyncClient    = NoLocalChainSyncClient
      , localStateQueryClient   = Just $ setupLocalStateQueryExpr waitResult mpoint tmvResultLocalState (f ntcVersion)
      , localTxSubmissionClient = Nothing
      , localTxMonitoringClient = Nothing
      }
    )

  atomically waitResult

-- | Use 'queryExpr' in a do block to construct monadic local state queries.
setupLocalStateQueryExpr ::
     STM x
     -- ^ An STM expression that only returns when all protocols are complete.
     -- Protocols must wait until 'waitDone' returns because premature exit will
     -- cause other incomplete protocols to abort which may lead to deadlock.
  -> Maybe ChainPoint
  -> TMVar (Either Net.Query.AcquireFailure a)
  -> LocalStateQueryExpr (BlockInMode mode) ChainPoint (QueryInMode mode) () IO a
  -> Net.Query.LocalStateQueryClient (BlockInMode mode) ChainPoint (QueryInMode mode) IO ()
setupLocalStateQueryExpr waitDone mPointVar' resultVar' f =
  LocalStateQueryClient . pure . Net.Query.SendMsgAcquire mPointVar' $
    Net.Query.ClientStAcquiring
    { Net.Query.recvMsgAcquired = runContT (runLocalStateQueryExpr f) $ \result -> do
        atomically $ putTMVar resultVar' (Right result)
        void $ atomically waitDone -- Wait for all protocols to complete before exiting.
        pure $ Net.Query.SendMsgRelease $ pure $ Net.Query.SendMsgDone ()

    , Net.Query.recvMsgFailure = \failure -> do
        atomically $ putTMVar resultVar' (Left failure)
        void $ atomically waitDone -- Wait for all protocols to complete before exiting.
        pure $ Net.Query.SendMsgDone ()
    }

-- | Use 'queryExpr' in a do block to construct monadic local state queries.
queryExpr :: QueryInMode mode fp a -> LocalStateQueryExpr block point (QueryInMode mode) r IO a
queryExpr q =
  LocalStateQueryExpr . ContT $ \f -> pure $
    Net.Query.SendMsgQuery q $
      Net.Query.ClientStQuerying
      { Net.Query.recvMsgResult = f
      }

-- | A monad expression that determines what era the node is in.
determineEraExpr ::
     ConsensusModeParams mode
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO AnyCardanoEra
determineEraExpr cModeParams =
  case consensusModeOnly cModeParams of
    ByronMode -> return $ AnyCardanoEra ByronEra
    ShelleyMode -> return $ AnyCardanoEra ShelleyEra
    CardanoMode -> queryExpr $ QueryCurrentEra CardanoModeIsMultiEra
