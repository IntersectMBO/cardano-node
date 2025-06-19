{-# LANGUAGE NoFieldSelectors #-}

module Cardano.Rpc.Server.Internal.Env
  (
  RpcEnv(..)
  ) where

import           Cardano.Api

import           Cardano.Rpc.Server.Config


data RpcEnv = RpcEnv
  { config :: !RpcConfig
  -- TODO replace with connection manager
  , rpcLocalNodeConnectInfo :: !LocalNodeConnectInfo
  }

