module Test.GetUpdatedMetrics
  ( getUpdatedMetricsViaPipe
  , getUpdatedMetricsViaSocket
  ) where

import Test.Hspec

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (modifyTVar', newTVarIO)
import qualified System.Metrics as EKG
import qualified System.Metrics.Gauge as G
import qualified System.Metrics.Label as L
import qualified System.Metrics.Counter as C

import           System.Metrics.Acceptor (runEKGAcceptor)
import           System.Metrics.Forwarder (runEKGForwarder)
import           System.Metrics.Configuration (HowToConnect (..))
import           System.Metrics.ReqResp (Request (..))

import           Test.MkConfig (mkAcceptorConfig, mkForwarderConfig)

getUpdatedMetricsViaPipe, getUpdatedMetricsViaSocket :: IO ()
getUpdatedMetricsViaPipe   = getUpdatedMetrics $ LocalPipe "/tmp/ekg-forward-test-3.sock"
getUpdatedMetricsViaSocket = getUpdatedMetrics $ RemoteSocket "127.0.0.1" 3030

getUpdatedMetrics :: HowToConnect -> IO ()
getUpdatedMetrics endpoint = do
  forwarderStore <- EKG.newStore
  acceptorStore  <- EKG.newStore
  weAreDone <- newTVarIO False

  let acceptorConfig = mkAcceptorConfig endpoint weAreDone GetUpdatedMetrics
      forwarderConfig = mkForwarderConfig endpoint

  _acceptorThr <- forkIO $ runEKGAcceptor acceptorConfig acceptorStore

  EKG.createGauge         "test1.gauge.1" forwarderStore >>= flip G.set 123
  g2 <- EKG.createGauge   "test1.gauge.2" forwarderStore
  G.set g2 456      
  EKG.createLabel         "test1.label.1" forwarderStore >>= flip L.set "TestLabel_1"
  l2 <- EKG.createLabel   "test1.label.2" forwarderStore 
  L.set l2 "TestLabel_2"
  EKG.createCounter       "test1.cntr.1"  forwarderStore >>= flip C.add 10
      
  _forwarderThr <- forkIO $ runEKGForwarder forwarderConfig forwarderStore

  threadDelay 1500000
  G.set g2 789
  L.set l2 "TestLabel_3.1415"
  EKG.createCounter       "test1.cntr.2"  forwarderStore >>= flip C.add 42
  threadDelay 1500000

  atomically $ modifyTVar' weAreDone (const True)

  threadDelay 1000000

  forwarderMetrics <- EKG.sampleAll forwarderStore
  acceptorMetrics  <- EKG.sampleAll acceptorStore

  acceptorMetrics `shouldBe` forwarderMetrics
