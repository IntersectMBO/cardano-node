{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Cardano.Benchmarking.Script.NodeConfig
  ( startProtocol
  , shutDownLogging
  ) where

import           Paths_tx_generator (version)
import           Prelude

import           Data.Monoid
import            Data.Text
import           Data.Version (showVersion)

import           Control.Concurrent (threadDelay)
import           Control.Monad (forM_)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except

import           Cardano.Node.Configuration.Logging (LoggingLayer, createLoggingLayer, shutdownLoggingLayer)
import           Cardano.Node.Configuration.POM
import           Cardano.Node.Handlers.Shutdown
import           Cardano.Node.Protocol.Cardano
import           Cardano.Node.Protocol.Types (SomeConsensusProtocol)
import           Cardano.Node.Types

import           Cardano.Tracing.Config

import           Cardano.Benchmarking.OuroborosImports as Core (getGenesis, protocolToNetworkId)
import           Cardano.Benchmarking.LegacyTracer
import           Cardano.Benchmarking.Tracer
import           Cardano.Benchmarking.Script.Env
import           Cardano.Benchmarking.Script.Setters
import           Cardano.Benchmarking.Script.Store as Store

liftToAction :: ExceptT Error IO a -> ActionM a
liftToAction = ExceptT . liftIO . runExceptT
  
makeConsensusProtocol
  :: NodeConfiguration
  -> ActionM SomeConsensusProtocol
makeConsensusProtocol nodeConfig = liftToAction $ case ncProtocolConfig nodeConfig of
  NodeProtocolConfigurationByron _    -> throwE $ ProtocolInstantiationError "NodeProtocolConfigurationByron not supported"
  NodeProtocolConfigurationShelley _  -> throwE $ ProtocolInstantiationError "NodeProtocolConfigurationShelley not supported"
  NodeProtocolConfigurationCardano byronConfig shelleyConfig alonzoConfig hardforkConfig
    -> withExceptT (ProtocolInstantiationError . show) $
         mkSomeConsensusProtocolCardano byronConfig shelleyConfig alonzoConfig hardforkConfig Nothing

makeLegacyLoggingLayer :: NodeConfiguration -> SomeConsensusProtocol -> ActionM LoggingLayer
makeLegacyLoggingLayer nc ptcl = liftToAction $ withExceptT NodeConfigError $
  createLoggingLayer
    (pack $ showVersion version)
    nc {ncTraceConfig=TracingOff}
    ptcl

makeNodeConfig :: FilePath -> ActionM NodeConfiguration
makeNodeConfig logConfig = liftToAction $ ExceptT $ do
 let configFp = ConfigYamlFilePath logConfig
     filesPc = defaultPartialNodeConfiguration
               { pncProtocolFiles = Last . Just $
                 ProtocolFilepaths
                 { byronCertFile = Just ""
                 , byronKeyFile = Just ""
                 , shelleyKESFile = Just ""
                 , shelleyVRFFile = Just ""
                 , shelleyCertFile = Just ""
                 , shelleyBulkCredsFile = Just ""
                 }
               , pncValidateDB = Last $ Just False
               , pncShutdownConfig = Last $ Just $ ShutdownConfig Nothing Nothing
               , pncConfigFile = Last $ Just configFp
               }
 configYamlPc <- parseNodeConfigurationFP . Just $ configFp
 case makeNodeConfiguration $ configYamlPc <> filesPc of
    Left err -> return $ Left $ MkNodeConfigError err
    Right nc' -> return $ Right nc'

startProtocol :: FilePath -> ActionM ()
startProtocol filePath = do
  nodeConfig <- makeNodeConfig filePath
  protocol <- makeConsensusProtocol nodeConfig
  set Protocol protocol
  set Genesis $ Core.getGenesis protocol
  set (User TNetworkId) $ protocolToNetworkId protocol
  case ncTraceConfig nodeConfig of
    TraceDispatcher _ -> liftIO initDefaultLegacyTracers >>= set Store.BenchTracers
    TracingOnLegacy _ -> do
      loggingLayer <- makeLegacyLoggingLayer nodeConfig protocol
      set Store.LoggingLayer $ Just loggingLayer
      set Store.BenchTracers $ createLoggingLayerTracers loggingLayer
    TracingOff -> liftIO initDefaultLegacyTracers >>= set Store.BenchTracers
  liftIO initDefaultTracers >>= set Store.BenchTracers

shutDownLogging :: ActionM ()
shutDownLogging = do
  ll <- get LoggingLayer
  traceError "QRT Last Message. LoggingLayer going to shutdown. 73 . . . ."
  liftIO $ do
    threadDelay (200*1000)
    forM_ ll
      shutdownLoggingLayer
