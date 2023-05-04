{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.IPC.SimpleQueries
  ( SimpleQueryError(..)
  , executeLocalStateQueryExprSimple
  , queryExprSimple
  ) where

import           Control.Monad.Except

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.Except.Extra (left)

import           Cardano.Ledger.Shelley.Scripts ()
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as Net.Query

import           Cardano.Api.Block
import           Cardano.Api.IPC
import           Cardano.Api.IPC.Types
import           Cardano.Api.IPC.Version
import           Cardano.Api.Query


{- HLINT ignore "Use const" -}
{- HLINT ignore "Use let" -}


-- | Execute a local state query expression.
executeLocalStateQueryExprSimple
  :: LocalNodeConnectInfo mode
  -> Maybe ChainPoint
  -> LocalStateQueryExprWithError SimpleQueryError (BlockInMode mode) ChainPoint (QueryInMode mode) () IO a
  -> IO (Either SimpleQueryError a)
executeLocalStateQueryExprSimple connectInfo mpoint f = do
  tmvResultLocalState <- newEmptyTMVarIO
  let waitResult = readTMVar tmvResultLocalState

  connectToLocalNodeWithVersion
    connectInfo
    (\ntcVersion ->
      LocalNodeClientProtocols
      { localChainSyncClient    = NoLocalChainSyncClient
      , localStateQueryClient   = Just $ setupLocalStateQueryExpr waitResult mpoint tmvResultLocalState ntcVersion f
      , localTxSubmissionClient = Nothing
      , localTxMonitoringClient = Nothing
      }
    )

  atomically waitResult


-- | Get the node server's Node-to-Client version.
getNtcVersion :: LocalStateQueryExpr block point (QueryInMode mode) r IO NodeToClientVersion
getNtcVersion = LocalStateQueryExpr ask


-- | Use 'queryExprSimple' in a do block to construct monadic local state queries.
queryExprSimple
  :: QueryInMode mode a
  -> LocalStateQueryExprWithError SimpleQueryError block point (QueryInMode mode) r IO a
queryExprSimple query = do
  let minNtcVersion = nodeToClientVersionOf query
  ntcVersion <- lift getNtcVersion
  if ntcVersion >= minNtcVersion
    then
      lift . LocalStateQueryExpr . ReaderT $ \_ -> ContT $ \f -> pure $
        Net.Query.SendMsgQuery query $
          Net.Query.ClientStQuerying
          { Net.Query.recvMsgResult = f
          }
    else left $ SimpleQueryErrorUnsupportedVer $ UnsupportedNtcVersionError minNtcVersion ntcVersion

-- | Use 'queryExprSimple' in a do block to construct monadic local state queries.
setupLocalStateQueryExpr ::
     STM x
     -- ^ An STM expression that only returns when all protocols are complete.
     -- Protocols must wait until 'waitDone' returns because premature exit will
     -- cause other incomplete protocols to abort which may lead to deadlock.
  -> Maybe ChainPoint
  -> TMVar (Either SimpleQueryError a)
  -> NodeToClientVersion
  -> LocalStateQueryExprWithError SimpleQueryError (BlockInMode mode) ChainPoint (QueryInMode mode) () IO a
  -> Net.Query.LocalStateQueryClient (BlockInMode mode) ChainPoint (QueryInMode mode) IO ()
setupLocalStateQueryExpr waitDone mPointVar' resultVar' ntcVersion f =
  LocalStateQueryClient . pure . Net.Query.SendMsgAcquire mPointVar' $
    Net.Query.ClientStAcquiring
    { Net.Query.recvMsgAcquired =
      runContT (runReaderT (runLocalStateQueryExpr $ runExceptT f) ntcVersion) $ \result -> do
        atomically $ putTMVar resultVar' result
        void $ atomically waitDone -- Wait for all protocols to complete before exiting.
        pure $ Net.Query.SendMsgRelease $ pure $ Net.Query.SendMsgDone ()

    , Net.Query.recvMsgFailure = \failure -> do
        atomically $ putTMVar resultVar' (Left . SimpleQueryErrorAcquiringFail $ toAcquiringFailure failure)
        void $ atomically waitDone -- Wait for all protocols to complete before exiting.
        pure $ Net.Query.SendMsgDone ()
    }

