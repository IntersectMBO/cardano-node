{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Node.Rpc.Server
  ( rungrpcExampleClient
  , rungrpcServer
  , methods
  )
  where


import           Cardano.Api

import           Cardano.Node.Configuration.POM
import           Cardano.Node.Configuration.Socket (SocketConfig (..))
import           Cardano.Node.Rpc.Proto.Api.NodeMetadata ()

import           Data.Monoid
import           Data.ProtoLens (defMessage)
import           Data.ProtoLens.Field (field)
import           Lens.Micro
import           Network.GRPC.Client
import           Network.GRPC.Client.StreamType.IO
import           Network.GRPC.Common
import           Network.GRPC.Server.Protobuf
import           Network.GRPC.Server.Run
import           Network.GRPC.Server.StreamType
import           Network.GRPC.Spec
import           System.FilePath (takeDirectory, (</>))

import           Proto.Cardano.Node.Rpc.Node
import           Proto.Google.Protobuf.Empty

-- Individual handlers

getEraMethod :: LocalNodeConnectInfo -> Proto Empty -> IO (Proto CurrentEra)
getEraMethod connInfo _ =
  pure mockNodeResponse

-- Server top level
methods :: LocalNodeConnectInfo -> Methods IO (ProtobufMethodsOf Node)
methods node =  Method (mkNonStreaming $ getEraMethod node)  NoMoreMethods

-- Mock node response
mockNodeResponse :: Proto CurrentEra
mockNodeResponse = Proto $ defMessage & field @"era" .~ Conway


rungrpcServer :: PartialNodeConfiguration -> IO ()
rungrpcServer cmdPc = do

    configYamlPc <- parseNodeConfigurationFP . getLast $ pncConfigFile cmdPc

    nc <- case makeNodeConfiguration $ defaultPartialNodeConfiguration <> configYamlPc <> cmdPc of
            Left err -> error $ "Error in creating the NodeConfiguration: " <> err
            Right nc' -> return nc'

    SocketConfig{ncSocketPath=Last (Just (File n2cSocketPath))} <- pure $ ncSocketConfig nc
    let socketDir = takeDirectory n2cSocketPath
        localNodeConnInfo = LocalNodeConnectInfo (CardanoModeParams (EpochSlots 21600)) Mainnet (File "dummy-socket-path")
        config = ServerConfig {
          -- serverInsecure = Just (InsecureConfig Nothing defaultInsecurePort)
          serverInsecure = Just (InsecureUnix $ socketDir </> "grpc.sock")
        , serverSecure   = Nothing
        }
    runServerWithHandlers def config $ fromMethods (methods localNodeConnInfo)


rungrpcExampleClient :: IO ()
rungrpcExampleClient =
  withConnection def server $ \conn -> do
    getEraRequest conn
 where
   server :: Server
   server = ServerInsecure $ Address "127.0.0.1" defaultInsecurePort Nothing

getEraRequest :: Connection -> IO ()
getEraRequest conn = do
  let req = defMessage :: Proto Empty
  response <- nonStreaming conn (rpc @(Protobuf Node "getEra")) req
  print response
