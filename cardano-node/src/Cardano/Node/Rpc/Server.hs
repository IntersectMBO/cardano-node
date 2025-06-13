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

module Cardano.Node.Rpc.Server
  ( runRpcServer
  )
where

import           Cardano.Api

import           Cardano.Node.Configuration.POM
import           Cardano.Node.Configuration.Socket (SocketConfig (..))
import qualified Cardano.Node.Rpc.Proto.Api.Node as Rpc
import qualified Cardano.Node.Rpc.Proto.Api.UtxoRpc.Query as UtxoRpc
import           Cardano.Node.Rpc.Server.Internal.Env
import           Cardano.Node.Rpc.Server.Internal.Monad
import           Cardano.Node.Rpc.Server.Internal.UtxoRpc.Query

import           Data.Monoid
import           Data.ProtoLens (defMessage)
import           Data.ProtoLens.Field (field)
import           Lens.Micro
import           Network.GRPC.Common
import           Network.GRPC.Server
import           Network.GRPC.Server.Protobuf
import           Network.GRPC.Server.Run
import           Network.GRPC.Server.StreamType
import           Network.GRPC.Spec
import           System.FilePath (takeDirectory, (</>))

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

runRpcServer :: PartialNodeConfiguration -> NetworkMagic -> IO ()
runRpcServer cmdPc networkMagic = do
  configYamlPc <- parseNodeConfigurationFP . getLast $ pncConfigFile cmdPc

  nc <- case makeNodeConfiguration $ defaultPartialNodeConfiguration <> configYamlPc <> cmdPc of
    Left err -> error $ "Error in creating the NodeConfiguration: " <> err
    Right nc' -> return nc'

  SocketConfig{ncSocketPath = Last (Just n2cSocket)} <- pure $ ncSocketConfig nc
  let socketDir = takeDirectory $ unFile n2cSocket
      networkType = fromNetworkMagic networkMagic
      localNodeConnInfo = LocalNodeConnectInfo (CardanoModeParams (EpochSlots 21600)) networkType n2cSocket
      config =
        ServerConfig
          { -- serverInsecure = Just (InsecureConfig Nothing defaultInsecurePort)
            serverInsecure = Just (InsecureUnix $ socketDir </> "rpc.sock")
          , serverSecure = Nothing
          }
      rpcEnv = RpcEnv{rpcLocalNodeConnectInfo = localNodeConnInfo}
  runRIO rpcEnv $
    withRunInIO $ \runInIO ->
      runServerWithHandlers def config . fmap (hoistSomeRpcHandler runInIO) $
        mconcat
          [ fromMethods methodsNodeRpc
          , fromMethods methodsUtxoRpc
          ]
