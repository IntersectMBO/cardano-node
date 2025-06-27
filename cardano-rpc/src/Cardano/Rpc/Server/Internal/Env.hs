{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Rpc.Server.Internal.Env
  ( RpcEnv (..)
  , mkLocalNodeConnectInfo
  )
where

import           Cardano.Api

import           Cardano.Rpc.Server.Config

import           Control.Tracer (Tracer)

data RpcEnv = RpcEnv
  { config :: !RpcConfig
  , tracer :: forall m. MonadIO m => Tracer m String
  , -- TODO replace with better connection management than one connection per rpc request
    rpcLocalNodeConnectInfo :: !LocalNodeConnectInfo
  }

mkLocalNodeConnectInfo :: SocketPath -> NetworkMagic -> LocalNodeConnectInfo
mkLocalNodeConnectInfo nodeSocketPath networkMagic =
  LocalNodeConnectInfo
    { localConsensusModeParams = CardanoModeParams (EpochSlots 21600)
    , localNodeNetworkId = fromNetworkMagic networkMagic
    , localNodeSocketPath = nodeSocketPath
    }
