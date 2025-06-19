{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Rpc.Server.Internal.Monad
  ( Has (..)
  , MonadRpc
  , grab
  )
where

import           Cardano.Api

import           Cardano.Rpc.Server.Internal.Env

import           GHC.Stack

import           RIO

class Has field env where
  obtain :: env -> field

grab :: forall field env m. (Has field env, MonadReader env m) => m field
grab = asks $ obtain @field
{-# INLINE grab #-}

instance Has LocalNodeConnectInfo RpcEnv where
  obtain RpcEnv{rpcLocalNodeConnectInfo} = rpcLocalNodeConnectInfo

type MonadRpc e m = (Has LocalNodeConnectInfo e, HasCallStack, MonadReader e m, MonadIO m)
