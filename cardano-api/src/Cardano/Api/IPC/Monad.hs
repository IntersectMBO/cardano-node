{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Cardano.Api.IPC.Monad
  ( LocalStateQueryScript
  , sendMsgQuery
  , setupLocalStateQueryScript
  ) where

import Cardano.Api.Block
import Cardano.Api.IPC
import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Cont
import Data.Either
import Data.Function
import Data.Maybe
import Data.Ord
import Shelley.Spec.Ledger.Scripts ()
import System.IO

import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as Net.Query
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as Net.Query

-- | Monadic type for constructing local state queries.
--
-- Use 'sendMsgQuery' in a do block to construct queries of this type and convert
-- the expression to a 'Net.Query.LocalStateQueryClient' with 'setupLocalStateQueryScript'.
newtype LocalStateQueryScript block point query r m a = LocalStateQueryScript
  { runLocalStateQueryScript :: ContT (Net.Query.ClientStAcquired block point query m r) m a
  } deriving (Functor, Applicative, Monad, MonadIO)

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
