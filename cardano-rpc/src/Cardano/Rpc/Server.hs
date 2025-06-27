{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import           Control.Tracer
import           Data.ProtoLens (defMessage)
import           Data.ProtoLens.Field (field)
import           Lens.Micro
import           Network.GRPC.Common
import           Network.GRPC.Server
import           Network.GRPC.Server.Protobuf
import           Network.GRPC.Server.Run
import           Network.GRPC.Server.StreamType
import           Network.GRPC.Spec hiding (Identity)

import           Proto.Google.Protobuf.Empty
import           RIO

-- Individual handlers

getEraMethod :: MonadRpc e m => Proto Empty -> m (Proto Rpc.CurrentEra)
getEraMethod _ = pure mockNodeResponse

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
  :: Tracer IO String
  -> IO (RpcConfig, NetworkMagic)
  -- ^ action which reloads RPC configuration
  -> IO ()
runRpcServer tracer loadRpcConfig = handleExceptions $ do
  ( rpcConfig@RpcConfig
      { isEnabled = Identity isEnabled
      , rpcSocketPath = Identity (File rpcSocketPathFp)
      , nodeSocketPath = Identity nodeSocketPath
      }
    , networkMagic
    ) <-
    loadRpcConfig
  let config =
        ServerConfig
          { serverInsecure = Just $ InsecureUnix rpcSocketPathFp
          , serverSecure = Nothing
          }
      rpcEnv =
        RpcEnv
          { config = rpcConfig
          , tracer = natTracer liftIO tracer
          , rpcLocalNodeConnectInfo = mkLocalNodeConnectInfo nodeSocketPath networkMagic
          }

  -- TODO this is logged by node configuration already, so it would make sense to log it again when
  -- configuration gets reloaded
  -- putTrace $ "RPC configuration: " <> show rpcConfig

  when isEnabled $
    runRIO rpcEnv $
      withRunInIO $ \runInIO ->
        runServerWithHandlers def config . fmap (hoistSomeRpcHandler runInIO) $
          mconcat
            [ fromMethods methodsNodeRpc
            , fromMethods methodsUtxoRpc
            ]
  where
    handleExceptions :: (HasCallStack => IO ()) -> IO ()
    handleExceptions = handleAny $ \e ->
      putTrace $ "RPC server fatal error: " <> displayException e

    putTrace = traceWith tracer
