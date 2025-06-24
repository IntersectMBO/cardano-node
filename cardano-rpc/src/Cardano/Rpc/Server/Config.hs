{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DeriveGeneric #-}

module Cardano.Rpc.Server.Config
  ( RpcConfigF (..)
  , RpcConfig
  , PartialRpcConfig
  , nodeSocketPathToRpcSocketPath
  )
where

import Cardano.Api
import Data.Monoid
import RIO
import Generic.Data (gmappend)
import System.FilePath (takeDirectory, (</>))

type PartialRpcConfig = RpcConfigF Last

type RpcConfig = RpcConfigF Identity

-- | RPC server configuration, which is a part of cardano-node configuration.
data RpcConfigF m = RpcConfig
  { isEnabled :: !(m Bool)
  -- ^ whether the RPC server is enabled
  , rpcSocketPath :: !(m SocketPath)
  -- ^ path to the socket where the RPC server listens
  }

-- | Convert node socket path to a default rpc socket path.
-- By default it's @rpc.sock@ in the same directory as node socket path.
nodeSocketPathToRpcSocketPath :: SocketPath -> SocketPath
nodeSocketPathToRpcSocketPath nodeSocketPath = do
  let socketDir = takeDirectory $ unFile nodeSocketPath
  File $ socketDir </> "rpc.sock"


deriving instance Show (RpcConfigF Identity)

deriving instance Eq (RpcConfigF Identity)

deriving instance Show (RpcConfigF Last)

deriving instance Eq (RpcConfigF Last)

deriving instance Generic (RpcConfigF Last)

instance Semigroup (RpcConfigF Last) where
  (<>) = gmappend

