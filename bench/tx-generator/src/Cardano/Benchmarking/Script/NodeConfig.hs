{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Benchmarking.Script.NodeConfig
  ( startProtocol
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except.Extra

import           Cardano.Benchmarking.OuroborosImports as Core (getGenesis, protocolToNetworkId)
import           Cardano.Benchmarking.Script.Env
import           Cardano.Benchmarking.Script.Setters
import           Cardano.Benchmarking.Script.Store as Store
import           Cardano.Benchmarking.Tracer

import           Cardano.TxGenerator.Setup.NodeConfig
import           Cardano.TxGenerator.Types (TxGenError)


liftToAction :: IO (Either TxGenError a) -> ActionM a
liftToAction = firstExceptT TxGenError . newExceptT . liftIO

startProtocol :: FilePath -> Maybe FilePath -> ActionM ()
startProtocol configFile tracerSocket = do
  nodeConfig <- makeNodeConfig configFile
  protocol <- makeConsensusProtocol nodeConfig
  set Protocol protocol
  set Genesis $ Core.getGenesis protocol
  let networkId = protocolToNetworkId protocol
  set (User TNetworkId) networkId
  tracers <- case tracerSocket of
    Nothing -> liftIO initDefaultTracers
    Just socket -> do
      iomgr <- askIOManager
      liftIO $ initTracers iomgr networkId socket
  set Store.BenchTracers tracers

shutDownLogging :: ActionM ()
shutDownLogging = do
  traceError "QRT Last Message. LoggingLayer going to shutdown. 73 . . . ."
  liftIO $ threadDelay (200*1000)
