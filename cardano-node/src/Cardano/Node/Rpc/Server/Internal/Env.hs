module Cardano.Node.Rpc.Server.Internal.Env
  (
  RpcEnv(..)
  ) where

import           Cardano.Api

data RpcEnv = RpcEnv
  { rpcLocalNodeConnectInfo :: LocalNodeConnectInfo
  }

