{-# LANGUAGE GADTs #-}
{-|
Module      : Cardano.Benchmarking.Script.Action
Description : Convert an 'Action' to a monadic 'ActionM'.

This is just exporting 'action' in order to avoid circular
module dependencies.
-}

module Cardano.Benchmarking.Script.Action
       ( action
       , liftToAction
       )
       where

import qualified Data.Text as Text (unpack)

import           Control.Monad.IO.Class

import           Cardano.Benchmarking.OuroborosImports as Core (protocolToNetworkId)
import           Cardano.Benchmarking.Script.Core
import           Cardano.Benchmarking.Script.Env
import           Cardano.Benchmarking.Script.Types
import           Cardano.Benchmarking.Tracer
import           Cardano.TxGenerator.Setup.NodeConfig


-- | 'action' has as its sole callers
-- 'Cardano.Benchmark.Script.runScript' from "Cardano.Benchmark.Script"
-- and 'Cardano.Benchmark.Script.Selftest' from
-- "Cardano.Benchmark.Script.Selftest".
-- It translates the various cases of the 'Action' to monadic values
-- which execute the specified actions when evaluated. It passes all
-- the cases' fields to functions with very similar names to the
-- constructors.
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

-- | 'startProtocol' sets up the protocol for the transaction
-- generator from the first argument, @configFile@ and optionally
-- traces to the second, @tracerSocket@.
startProtocol :: FilePath -> Maybe FilePath -> ActionM ()
startProtocol configFile tracerSocket = do
  nodeConfig <- liftToAction $ mkNodeConfig configFile
  protocol <-  liftToAction $ mkConsensusProtocol nodeConfig
  setEnvProtocol protocol
  setEnvGenesis $ getGenesis protocol
  iomgr <- askIOManager

  let
    networkId = protocolToNetworkId protocol
    tracerSocket' = (,,) iomgr networkId `fmap` tracerSocket

  setEnvNetworkId networkId
  liftIO (initTxGenTracers tracerSocket') >>= setBenchTracers
