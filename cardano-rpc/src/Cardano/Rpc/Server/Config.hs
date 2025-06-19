{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
module Cardano.Rpc.Server.Config
  ( RpcConfig(..)
  , mkLocalNodeConnectInfo
  )
  where

import           Cardano.Api

data RpcConfig = RpcConfig
  { isEnabled :: !Bool
  , rpcSocketPath :: !SocketPath
  , nodeSocketPath :: !SocketPath
  , networkMagic :: !NetworkMagic
  } deriving (Show, Eq)

mkLocalNodeConnectInfo :: RpcConfig -> LocalNodeConnectInfo
mkLocalNodeConnectInfo RpcConfig{networkMagic, nodeSocketPath} =
  LocalNodeConnectInfo
    { localConsensusModeParams = CardanoModeParams (EpochSlots 21600)
    , localNodeNetworkId = fromNetworkMagic networkMagic
    , localNodeSocketPath = nodeSocketPath
    }

