{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Node.Rpc.Server
  ( rungrpcExampleClient
  , rungrpcServer
  , methods
  )
  where 


import Network.GRPC.Server.Run
import Network.GRPC.Spec
import Cardano.Api
import Proto.Google.Protobuf.Empty
import Data.ProtoLens (defMessage)
import Cardano.Node.Rpc.Proto.Api.Node 
import Lens.Micro
import Network.GRPC.Common 
import Data.ProtoLens.Field (field)
import Network.GRPC.Server.Protobuf
import Network.GRPC.Server.StreamType
import Data.Monoid
import Cardano.Node.Rpc.Proto.Api.NodeMetadata ()
import Cardano.Node.Configuration.POM
import Network.GRPC.Client 
import Network.GRPC.Client.StreamType.IO

-- Individual handlers 

getEraMethod :: LocalNodeConnectInfo -> Proto Empty -> IO (Proto CurrentEra)
getEraMethod connInfo _ = 
   case connInfo of 
    _ -> return mockNodeResponse 

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

    let _nodeSocketConfig = ncSocketConfig nc 
        localNodeConnInfo = LocalNodeConnectInfo (CardanoModeParams (EpochSlots 21600)) Mainnet (File "dummy-socket-path")
        config = ServerConfig {
          serverInsecure = Just (InsecureConfig Nothing defaultInsecurePort)
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