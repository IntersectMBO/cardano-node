import           Criterion.Main
import           Data.IORef

import           Cardano.Logging
import           Cardano.Logging.Test.Config
import           Cardano.Logging.Test.Messages
import           Cardano.Logging.Test.Oracles
import           Cardano.Logging.Test.Script
import           Cardano.Logging.Test.Tracer
import           Cardano.Logging.Test.Types
import           System.Remote.Monitoring (forkServer)

import           Debug.Trace

-- Can be run with:
-- cabal bench trace-dispatcher-bench --benchmark-option='-o benchmark-trace.html'

main :: IO ()
main = do
  stdioTr <- standardTracer
  tr      <- stdoutTracers stdioTr
  filtr   <- filterTracers stdioTr
  imtr    <- inMemoryTracers
  tlTr    <- timeLimitedTracers stdioTr
  ekgTr   <- ekgTracers
  defaultMain [
    bgroup "tracer" [
                        bench "sendMessageStdout1"  $ whnfIO (sendMessage 1 tr)
                      , bench "sendMessageStdout10"  $ whnfIO (sendMessage 10 tr)
                      , bench "sendMessageStdout100"  $ whnfIO (sendMessage 100 tr)
                      , bench "sendMessageStdout1000"  $ whnfIO (sendMessage 1000 tr)

                      , bench "sendMessageInMemory1"  $ whnfIO (sendMessage 1 imtr)
                      , bench "sendMessageInMemory10"  $ whnfIO (sendMessage 10 imtr)
                      , bench "sendMessageInMemory100"  $ whnfIO (sendMessage 100 imtr)
                      , bench "sendMessageInMemory1000"  $ whnfIO (sendMessage 1000 imtr)

                      , bench "sendMessageFiltered1"  $ whnfIO (sendMessage 1 filtr)
                      , bench "sendMessageFiltered10"  $ whnfIO (sendMessage 10 filtr)
                      , bench "sendMessageFiltered100"  $ whnfIO (sendMessage 100 filtr)
                      , bench "sendMessageFiltered1000"  $ whnfIO (sendMessage 1000 filtr)

                      , bench "sendMessageTimeLimited1000_100"  $ whnfIO (sendMessage 1000 tlTr)
                      , bench "sendMessageTimeLimited10000_100"  $ whnfIO (sendMessage 10000 tlTr)

                      , bench "sendEKG 5/1"  $ whnfIO (sendMessage 1 ekgTr)
                      , bench "sendEKG 5/10"  $ whnfIO (sendMessage 10 ekgTr)
                      , bench "sendEKG 5/100"  $ whnfIO (sendMessage 100 ekgTr)
                      , bench "sendEKG 5/1000"  $ whnfIO (sendMessage 1000 ekgTr)
                    ]
              ]

stdoutTracers :: Trace IO FormattedMessage -> IO (Trace IO Message)
stdoutTracers stdoutTracer = do
    forwardTrRef    <- newIORef []
    forwardTracer'  <- testTracer forwardTrRef
    tr              <- mkCardanoTracer
                        stdoutTracer
                        forwardTracer'
                        Nothing
                        ["Test"]
                        namesForMessage
                        severityForMessage
                        privacyForMessage
    configureTracers config1 docMessage [tr]
    pure tr

filterTracers :: Trace IO FormattedMessage -> IO (Trace IO Message)
filterTracers stdoutTracer = do
    forwardTrRef    <- newIORef []
    forwardTracer'  <- testTracer forwardTrRef
    tr              <- mkCardanoTracer
                        stdoutTracer
                        forwardTracer'
                        Nothing
                        ["Test"]
                        namesForMessage
                        severityForMessage
                        privacyForMessage
    configureTracers config2 docMessage [tr]
    pure tr

inMemoryTracers :: IO (Trace IO Message)
inMemoryTracers = do
    stdoutTrRef     <- newIORef []
    stdoutTracer'   <- testTracer stdoutTrRef
    forwardTrRef    <- newIORef []
    forwardTracer'  <- testTracer forwardTrRef
    tr              <- mkCardanoTracer
                        stdoutTracer'
                        forwardTracer'
                        Nothing
                        ["Test"]
                        namesForMessage
                        severityForMessage
                        privacyForMessage
    configureTracers config1 docMessage [tr]
    pure tr

timeLimitedTracers :: Trace IO FormattedMessage -> IO (Trace IO Message)
timeLimitedTracers stdoutTracer = do
    forwardTrRef    <- newIORef []
    forwardTracer'  <- testTracer forwardTrRef
    tr              <- mkCardanoTracer
                        stdoutTracer
                        forwardTracer'
                        Nothing
                        ["Test"]
                        namesForMessage
                        severityForMessage
                        privacyForMessage
    configureTracers config3 docMessage [tr]
    pure tr

ekgTracers :: IO (Trace IO Message)
ekgTracers = do
    stdoutTrRef     <- newIORef []
    stdoutTracer'   <- testTracer stdoutTrRef
    forwardTrRef    <- newIORef []
    forwardTracer'  <- testTracer forwardTrRef
    ekgServer       <- forkServer "localhost" 8000
    ekgTracer       <- ekgTracer (Right ekgServer)
    tr              <- mkCardanoTracer
                        stdoutTracer'
                        forwardTracer'
                        Nothing
                        ["Test"]
                        namesForMessage
                        severityForMessage
                        privacyForMessage
    configureTracers config4 docMessage [tr]
    pure tr

timesRepeat :: Int -> IO () -> IO ()
timesRepeat 0 _ = pure ()
timesRepeat n action = do
  action
  timesRepeat (n - 1) action

sendMessage n tr =
  timesRepeat n (traceWith tr (Message1 1 1))
