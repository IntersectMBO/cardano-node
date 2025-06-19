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

module Cardano.Rpc.Server
  ( runRpcServer
  )
where

import           Cardano.Api

import qualified Cardano.Rpc.Proto.Api.Node as Rpc
import qualified Cardano.Rpc.Proto.Api.UtxoRpc.Query as UtxoRpc
import           Cardano.Rpc.Server.Config
import           Cardano.Rpc.Server.Internal.Env
import           Cardano.Rpc.Server.Internal.Monad
import           Cardano.Rpc.Server.Internal.UtxoRpc.Query

import           Data.ProtoLens (defMessage)
import           Data.ProtoLens.Field (field)
import           Lens.Micro
import           Network.GRPC.Common
import           Network.GRPC.Server
import           Network.GRPC.Server.Protobuf
import           Network.GRPC.Server.Run
import           Network.GRPC.Server.StreamType
import           Network.GRPC.Spec

import           Proto.Google.Protobuf.Empty
import           RIO

-- Individual handlers

getEraMethod :: MonadRpc e m => Proto Empty -> m (Proto Rpc.CurrentEra)
getEraMethod _ =
  pure mockNodeResponse

-- Mock node response
mockNodeResponse :: Proto Rpc.CurrentEra
mockNodeResponse = Proto $ defMessage & field @"era" .~ Rpc.Conway

-- Server top level
methodsNodeRpc
  :: MonadRpc e m
  => Methods m (ProtobufMethodsOf Rpc.Node)
methodsNodeRpc = Method (mkNonStreaming getEraMethod) NoMoreMethods

methodsUtxoRpc
  :: MonadRpc e m
  => Methods m (ProtobufMethodsOf UtxoRpc.QueryService)
methodsUtxoRpc =
  Method (mkNonStreaming readChainConfigMethod)
    . Method (mkNonStreaming readDataMethod)
    . Method (mkNonStreaming readParamsMethod)
    . Method (mkNonStreaming readTxMethod)
    . Method (mkNonStreaming readUtxosMethod)
    . Method (mkNonStreaming searchUtxosMethod)
    $ NoMoreMethods

runRpcServer
  :: IO RpcConfig
  -- ^ action which reloads RPC configuration
  -> IO ()
runRpcServer loadRpcConfig = do
  rpcConfig@RpcConfig{rpcSocketPath = File rpcSocketPathFp} <- loadRpcConfig
  let config =
        ServerConfig
          { -- serverInsecure = Just (InsecureConfig Nothing defaultInsecurePort)
            serverInsecure = Just $ InsecureUnix rpcSocketPathFp
          , serverSecure = Nothing
          }
      rpcEnv =
        RpcEnv
          { config = rpcConfig
          , rpcLocalNodeConnectInfo = mkLocalNodeConnectInfo rpcConfig
          }
  runRIO rpcEnv $
    withRunInIO $ \runInIO ->
      runServerWithHandlers def config . fmap (hoistSomeRpcHandler runInIO) $
        mconcat
          [ fromMethods methodsNodeRpc
          , fromMethods methodsUtxoRpc
          ]
