{-# LANGUAGE GADTs #-}

module Cardano.Benchmarking.Script.Action
       ( action
       , liftToAction
       )
       where

import qualified Data.Text as Text (unpack)

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except.Extra

import           Cardano.Benchmarking.OuroborosImports as Core (protocolToNetworkId)
import           Cardano.Benchmarking.Script.Core
import           Cardano.Benchmarking.Script.Env
import           Cardano.Benchmarking.Script.Types
import           Cardano.Benchmarking.Tracer
import           Cardano.TxGenerator.Setup.NodeConfig
import           Cardano.TxGenerator.Types (TxGenError)


action :: Action -> ActionM ()
action a = case a of
  SetNetworkId val -> setEnvNetworkId val
  SetSocketPath val -> setEnvSocketPath val
  InitWallet name -> initWallet name
  SetProtocolParameters p -> setProtocolParameters p
  StartProtocol configFile cardanoTracerSocket -> startProtocol configFile cardanoTracerSocket
  ReadSigningKey name filePath -> readSigningKey name filePath
  DefineSigningKey name descr -> defineSigningKey name descr
  AddFund era wallet txIn lovelace keyName -> addFund era wallet txIn lovelace keyName
  Delay t -> delay t
  Submit era submitMode txParams generator -> submitAction era submitMode generator txParams
  WaitBenchmark thread -> waitBenchmark thread
  CancelBenchmark thread -> cancelBenchmark thread
  WaitForEra era -> waitForEra era
  LogMsg txt -> traceDebug $ Text.unpack txt
  Reserved options -> reserved options

liftToAction :: IO (Either TxGenError a) -> ActionM a
liftToAction = firstExceptT TxGenError . newExceptT . liftIO

startProtocol :: FilePath -> Maybe FilePath -> ActionM ()
startProtocol configFile tracerSocket = do
  nodeConfig <- liftToAction $ mkNodeConfig configFile
  protocol <-  liftToAction $ mkConsensusProtocol nodeConfig
  setEnvProtocol protocol
  setEnvGenesis $ getGenesis protocol
  let networkId = protocolToNetworkId protocol
  setEnvNetworkId networkId
  tracers <- case tracerSocket of
    Nothing -> liftIO initDefaultTracers
    Just socket -> do
      iomgr <- askIOManager
      liftIO $ initTracers iomgr networkId socket
  setBenchTracers tracers
