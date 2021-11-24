{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Api.IPC.Monad
  ( LocalStateQueryExpr
  , executeLocalStateQueryExpr
  , queryExpr
  , determineEraExpr
  , NtcVersionOf (..)
  , getNtcVersion
  , maybeQueryExpr
  ) where

import Cardano.Api.Block
import Cardano.Api.Eras
import Cardano.Api.IPC
import Cardano.Api.IPC.NtcVersionOf (NtcVersionOf (..))
import Cardano.Api.Modes
import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Reader
import Data.Either
import Data.Function
import Data.Maybe
import Data.Ord
import Cardano.Ledger.Shelley.Scripts ()
import System.IO

import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as Net.Query

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
  { runLocalStateQueryExpr :: ReaderT NodeToClientVersion (ContT (Net.Query.ClientStAcquired block point query m r) m) a
  } deriving (Functor, Applicative, Monad, MonadIO)

-- | Execute a local state query expression.
executeLocalStateQueryExpr
  :: forall e mode a .
     LocalNodeConnectInfo mode
  -> Maybe ChainPoint
  -> (QueryError -> e)
  -> LocalStateQueryExpr (BlockInMode mode) ChainPoint (QueryInMode mode) () IO (Either e a)
  -> IO (Either e a)
executeLocalStateQueryExpr connectInfo mpoint mapQueryError f = do
  tmvResultLocalState <- newEmptyTMVarIO
  let waitResult = readTMVar tmvResultLocalState

  connectToLocalNodeWithVersion
    connectInfo
    (\ntcVersion ->
      LocalNodeClientProtocols
      { localChainSyncClient    = NoLocalChainSyncClient
      , localStateQueryClient   = Just $ setupLocalStateQueryExpr waitResult mpoint tmvResultLocalState ntcVersion mapQueryError f
      , localTxSubmissionClient = Nothing
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
  -> TMVar (Either e a)
  -> NodeToClientVersion
  -> (QueryError -> e)
  -> LocalStateQueryExpr (BlockInMode mode) ChainPoint (QueryInMode mode) () IO (Either e a)
  -> Net.Query.LocalStateQueryClient (BlockInMode mode) ChainPoint (QueryInMode mode) IO ()
setupLocalStateQueryExpr waitDone mPointVar' resultVar' ntcVersion mapQueryError f =
  LocalStateQueryClient . pure . Net.Query.SendMsgAcquire mPointVar' $
    Net.Query.ClientStAcquiring
    { Net.Query.recvMsgAcquired = runContT (runReaderT (runLocalStateQueryExpr f) ntcVersion) $ \result -> do
        atomically $ putTMVar resultVar' result
        void $ atomically waitDone -- Wait for all protocols to complete before exiting.
        pure $ Net.Query.SendMsgRelease $ pure $ Net.Query.SendMsgDone ()

    , Net.Query.recvMsgFailure = \failure -> do
        atomically $ putTMVar resultVar' (Left (mapQueryError (QueryErrorAcquireFailure failure)))
        void $ atomically waitDone -- Wait for all protocols to complete before exiting.
        pure $ Net.Query.SendMsgDone ()
    }

-- | Get the node server's Node-to-Client version.
getNtcVersion :: LocalStateQueryExpr block point (QueryInMode mode) r IO (Either QueryError NodeToClientVersion)
getNtcVersion =  LocalStateQueryExpr $ do
  v <- ask
  pure $ Right v

-- | Lift a query value into a monadic query expression.
-- Use 'queryExpr' in a do block to construct monadic local state queries.
queryExpr :: QueryInMode mode a -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either QueryError a)
queryExpr q = runExceptT $ do
  let minNtcVersion = ntcVersionOf q
  ntcVersion <- ExceptT getNtcVersion
  if ntcVersion >= minNtcVersion
    then lift
      $ LocalStateQueryExpr $ ReaderT $ \_ -> ContT $ \f -> pure $
        Net.Query.SendMsgQuery q $
          Net.Query.ClientStQuerying
          { Net.Query.recvMsgResult = f
          }
    else throwE $ QueryErrorUnsupportedVersion minNtcVersion ntcVersion

-- | Lift a query value into a monadic query expression returning Maybe of a result.
-- This is the same as 'queryExpr' except if the query is not supported by the server, will return Nothing instead
-- of throwing an error.
maybeQueryExpr :: QueryInMode mode a -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either QueryError (Maybe a))
maybeQueryExpr q = runExceptT $ catchE (Just <$> ExceptT (queryExpr q)) $ \e ->
  case e of
    QueryErrorUnsupportedVersion _ _ -> return Nothing
    _ -> throwE e

-- | A monadic expresion that determines what era the node is in.
determineEraExpr ::
     ConsensusModeParams mode
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either QueryError AnyCardanoEra)
determineEraExpr cModeParams = runExceptT $
  case consensusModeOnly cModeParams of
    ByronMode -> return $ AnyCardanoEra ByronEra
    ShelleyMode -> return $ AnyCardanoEra ShelleyEra
    CardanoMode -> ExceptT . queryExpr $ QueryCurrentEra CardanoModeIsMultiEra
