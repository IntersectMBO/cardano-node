{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Rpc.Server.Config
  ( RpcConfig
  , PartialRpcConfig
  , RpcConfigF(..)
  , makeRpcConfig
  , nodeSocketPathToRpcSocketPath
  )
where

import           Cardano.Api

import           Data.Monoid
import           System.FilePath (takeDirectory, (</>))

import           Generic.Data (gmappend, gmempty)
import           RIO

type PartialRpcConfig = RpcConfigF Last

type RpcConfig = RpcConfigF Identity

-- | RPC server configuration, which is a part of cardano-node configuration.
data RpcConfigF m = RpcConfig
  { isEnabled :: !(m Bool)
  -- ^ whether the RPC server is enabled
  , rpcSocketPath :: !(m SocketPath)
  -- ^ path to the socket file where the RPC server listens
  , nodeSocketPath :: !(m SocketPath)
  -- ^ cardano-node socket path. Only valid if RPC endpoint is enabled.
  }

deriving instance Show (RpcConfigF Identity)

deriving instance Eq (RpcConfigF Identity)

deriving instance Show (RpcConfigF Last)

deriving instance Eq (RpcConfigF Last)

deriving instance Generic (RpcConfigF Last)

instance Semigroup (RpcConfigF Last) where
  (<>) = gmappend

instance Monoid (RpcConfigF Last) where
  mempty = gmempty

-- | Build RPC Config
--
-- Uses the following defaults if the values are not provided
-- * RPC is disabled
-- * @rpc.sock@ is placed in the same path as the node socket
--
-- Validates if the node socket is enabled if RPC is enabled.
makeRpcConfig :: MonadError String m
              => PartialRpcConfig
              -> m RpcConfig
makeRpcConfig RpcConfig{isEnabled = Last mIsEnabled, rpcSocketPath = Last mRpcSocketPath, nodeSocketPath = Last mNodeSocketPath} = do
  let isEnabled = fromMaybe False mIsEnabled
      -- default to a some non-existing path. Does not matter if the gRPC endpoint is disabled
      nodeSocketPath = fromMaybe "./node.socket" mNodeSocketPath
      rpcSocketPath = fromMaybe (nodeSocketPathToRpcSocketPath nodeSocketPath) mRpcSocketPath
  when (isEnabled && isNothing mNodeSocketPath) $
    throwError "Configuration error: gRPC endpoint was enabled but node socket file was not specified. Cannot run gRPC server without node socket."
  pure $
    RpcConfig
      (pure isEnabled)
      (pure rpcSocketPath)
      (pure nodeSocketPath)

-- | Convert node socket path to a default rpc socket path.
-- By default it's @rpc.sock@ in the same directory as node socket path.
nodeSocketPathToRpcSocketPath :: SocketPath -> SocketPath
nodeSocketPathToRpcSocketPath nodeSocketPath = do
  let socketDir = takeDirectory $ unFile nodeSocketPath
  File $ socketDir </> "rpc.sock"
