{-# LANGUAGE NoFieldSelectors #-}

module Cardano.Rpc.Server.Internal.Env
  ( RpcEnv (..)
  , mkLocalNodeConnectInfo
  )
where

import Cardano.Api

import Cardano.Rpc.Server.Config

data RpcEnv = RpcEnv
  { config :: !RpcConfig
  , -- TODO replace with connection manager for connection pooling, Data.Pool from resource-pool perhaps?
    rpcLocalNodeConnectInfo :: !LocalNodeConnectInfo
  }

mkLocalNodeConnectInfo :: SocketPath -> NetworkMagic -> LocalNodeConnectInfo
mkLocalNodeConnectInfo nodeSocketPath networkMagic =
  LocalNodeConnectInfo
    { localConsensusModeParams = CardanoModeParams (EpochSlots 21600)
    , localNodeNetworkId = fromNetworkMagic networkMagic
    , localNodeSocketPath = nodeSocketPath
    }
