{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.IPC.Types
  ( LocalStateQueryExpr(..)
  , LocalStateQueryExprWithError
  ) where

import           Control.Monad.Except

import           Control.Monad.Reader
import           Control.Monad.Trans.Cont

import           Cardano.Ledger.Shelley.Scripts ()
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as Net.Query

import           Cardano.Api.IPC


{- HLINT ignore "Use const" -}
{- HLINT ignore "Use let" -}


type LocalStateQueryExprWithError e block point query r m a
  = ExceptT e (LocalStateQueryExpr block point query r m) a

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
  } deriving (Functor, Applicative, Monad, MonadReader NodeToClientVersion, MonadIO)

