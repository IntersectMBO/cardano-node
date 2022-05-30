{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Cardano.Benchmarking.Script.NodeConfig
  ( startProtocol
  , shutDownLogging
  ) where

import           Paths_tx_generator (version)
import           Prelude

import           Data.Bifunctor (second)
import           Data.Monoid
import           Data.Text as Text
import           Data.Version (showVersion)

import           Control.Concurrent (threadDelay)
import           Control.Monad (forM_)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except

import           Cardano.BM.Data.Backend
import           Cardano.BM.Tracing
import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Setup (setupTrace_)
import           Cardano.BM.Data.LogItem (mapLogObject)

import           Cardano.Tracing.Config (TraceOptions(..))
import           Cardano.BM.Data.Output
import           Cardano.Node.Configuration.Logging (LoggingLayer, createLoggingLayer, shutdownLoggingLayer)
import           Cardano.Node.Configuration.POM
import           Cardano.Node.Handlers.Shutdown
import           Cardano.Node.Protocol.Cardano
import           Cardano.Node.Protocol.Types (SomeConsensusProtocol)
import           Cardano.Node.Types

import           Cardano.Benchmarking.OuroborosImports as Core (getGenesis, protocolToNetworkId)
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

initLegacyTracer :: ActionM ()
initLegacyTracer = do
  baseTracer <- liftIO $ do
    c <- defaultConfigStdout
    CM.setDefaultBackends c [KatipBK ]
    CM.setSetupBackends c [KatipBK ]
    CM.setDefaultBackends c [KatipBK ]
    CM.setSetupScribes c [ ScribeDefinition {
                              scName = "cli"
                            , scFormat = ScJson
                            , scKind = StdoutSK
                            , scPrivacy = ScPublic
                            , scMinSev = minBound
                            , scMaxSev = maxBound
                            , scRotation = Nothing
                            }
                         ]
    CM.setScribes c "cardano.cli" (Just ["StdoutSK::cli"])
    (tr :: Trace IO String, _switchboard) <- setupTrace_ c "cardano"

    let tr' = appendName "cli" tr
    return tr'

  let
    (bt :: Trace IO Text.Text)  = contramap (second $ mapLogObject Text.unpack) baseTracer
  set Store.BenchTracers $ initTracers bt bt

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
    TraceDispatcher _ -> initLegacyTracer
    TracingOnLegacy _ -> do
      loggingLayer <- makeLegacyLoggingLayer nodeConfig protocol
      set Store.LoggingLayer $ Just loggingLayer
      set Store.BenchTracers $ createLoggingLayerTracers loggingLayer
    TracingOff -> initLegacyTracer

shutDownLogging :: ActionM ()
shutDownLogging = do
  ll <- get LoggingLayer
  traceError "QRT Last Message. LoggingLayer going to shutdown. 73 . . . ."
  liftIO $ do
    threadDelay (200*1000)
    forM_ ll
      shutdownLoggingLayer
