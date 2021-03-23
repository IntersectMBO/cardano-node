module Test.GetLogObjects
  ( getLogObjectsViaPipe
  , getLogObjectsViaSocket
  ) where

import Test.Hspec

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.STM.TBQueue (newTBQueueIO, readTBQueue,
                                                 writeTBQueue)
import           Control.Monad.STM (atomically)
import           Data.IORef (atomicModifyIORef', newIORef)
import           Data.Text (Text)
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock (UTCTime (..))
import           Data.Time.LocalTime (TimeOfDay (..), timeOfDayToTime)
import           Data.Word (Word16)

import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

import           Cardano.BM.Data.LogItem (LogObject (..), LOContent (..),
                                          LOMeta (..), PrivacyAnnotation (..))
import           Cardano.BM.Data.Severity (Severity (..))

import           Trace.Forward.Acceptor (runTraceAcceptor)
import           Trace.Forward.Forwarder (runTraceForwarder)
import           Trace.Forward.Configuration (HowToConnect (..))
import           Trace.Forward.ReqResp (Request (..))

import           Test.MkConfig (mkAcceptorConfig, mkForwarderConfig)

getLogObjectsViaPipe, getLogObjectsViaSocket :: IO ()
getLogObjectsViaPipe   = getLogObjects 3 $ LocalPipe "/tmp/trace-forward-test-1.sock"
getLogObjectsViaSocket = getLogObjects 3 $ RemoteSocket "127.0.0.1" 3010

getLogObjects :: Word16 -> HowToConnect -> IO ()
getLogObjects howManyToRequest endpoint = do
  let qSize = 100
  forwarderQueue <- newTBQueueIO qSize
  acceptorQueue  <- newTBQueueIO qSize
  weAreDone <- newIORef False

  let acceptorConfig = mkAcceptorConfig endpoint
                                        weAreDone
                                        $ GetLogObjects howManyToRequest
      forwarderConfig = mkForwarderConfig endpoint

  _acceptorThr <- forkIO $ runTraceAcceptor acceptorConfig acceptorQueue

  atomically $ do
    writeTBQueue forwarderQueue loF1
    writeTBQueue forwarderQueue loF2
    writeTBQueue forwarderQueue loF3

  _forwarderThr <- forkIO $ runTraceForwarder forwarderConfig forwarderQueue

  threadDelay 2000000

  atomicModifyIORef' weAreDone (const (True, ()))

  threadDelay 1000000

  loA1 <- atomically $ readTBQueue acceptorQueue
  loA2 <- atomically $ readTBQueue acceptorQueue
  loA3 <- atomically $ readTBQueue acceptorQueue

  [loA1, loA2, loA3] `shouldBe` [loF1, loF2, loF3]

loF1, loF2, loF3 :: LogObject Text
loF1 = LogObject "test.LO.1" meta $ LogMessage "test.LogMessage.1"
loF2 = LogObject "test.LO.2" meta $ LogError   "test.LogError.1"
loF3 = LogObject "test.LO.3" meta $ LogError   "test.LogError.2"

meta :: LOMeta
meta =
  LOMeta
    { tstamp   = UTCTime (fromGregorian 2020 07 08) (timeOfDayToTime $ TimeOfDay 8 45 56.553)
    , tid      = "tid"
    , hostname = "nixos"
    , severity = Error
    , privacy  = Public
    }

-- We need it for 'AcceptorConfiguration a' and 'ForwarderConfiguration a'
-- (in this example it is 'Text').
instance ShowProxy Text
