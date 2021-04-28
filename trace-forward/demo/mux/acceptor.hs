import           Control.Concurrent (ThreadId, killThread, myThreadId, threadDelay)
import           Control.Concurrent.Async (async, withAsync)
import           Control.Concurrent.STM.TVar
import           Control.Tracer (contramap, nullTracer, stdoutTracer)
import           Control.Monad (forever, void, when)
import           Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import           Data.Fixed (Pico)
import           Data.Maybe (isJust)
import           Data.Text (Text, pack)
import           Data.Time.Clock (NominalDiffTime, getCurrentTime,
                                  diffUTCTime, secondsToNominalDiffTime)
import           Data.Word (Word16, Word64)
import           System.Environment (getArgs)
import           System.Exit (die)

import           Cardano.BM.Data.LogItem (LogObject)

import qualified Trace.Forward.Configuration as TF
import qualified Trace.Forward.Protocol.Type as TF

import qualified System.Metrics.Configuration as EKGF
import qualified System.Metrics.ReqResp as EKGF

import           Network.Acceptor (HowToConnect (..), launchAcceptors)

main :: IO ()
main = do
  (listenIt, freq, itemsNum, benchSpeedFreq, totalObjs, reConnectTest) <- do
    args <- getArgs
    if "--dc" `elem` args
      then
        case args of
          [host, port, "--dc", freq] ->
            return ( RemoteSocket host port
                   , 1   -- This is disconnect test, so the frequency of requests doesn't matter.
                   , 100 -- This is disconnect test, so the number of requested LogObjects doesn't matter.
                   , Nothing
                   , Nothing
                   , Just (read freq :: Pico) -- This is how often the server will be shut down.
                   )
          _ -> die "Usage: demo-acceptor-mux host port --dc freqInSecs"
      else
        case args of
          [path, freq, n] ->
            return ( LocalPipe path
                   , read freq :: Pico
                   , read n :: Word16
                   , Nothing
                   , Nothing
                   , Nothing
                   )
          [host, port, freq, n] ->
            return ( RemoteSocket host port
                   , read freq :: Pico
                   , read n :: Word16
                   , Nothing
                   , Nothing
                   , Nothing
                   )
          [path, freq, n, "-b", sp] ->
            return ( LocalPipe path
                   , read freq :: Pico
                   , read n :: Word16
                   , Just (read sp :: Pico)
                   , Nothing
                   , Nothing
                   )
          [path, freq, n, "-b", sp, "-t", tn] ->
            return ( LocalPipe path
                   , read freq :: Pico
                   , read n :: Word16
                   , Just (read sp :: Pico)
                   , Just (read tn :: Word64)
                   , Nothing
                   )
          _ ->
            die "Usage: demo-acceptor-mux (pathToPipe | host port) freqInSecs itemsNum [-b freqInSecs] [-t totalObjs]"

  configs <- mkConfigs listenIt freq itemsNum benchSpeedFreq totalObjs

  tidVar <- newTVarIO =<< myThreadId -- Just for filling TVar, it will be replaced anyway.

  case reConnectTest of
    Nothing -> launchAcceptors listenIt configs tidVar
    Just rcFreq -> runReConnector (launchAcceptors listenIt configs tidVar) rcFreq tidVar

mkConfigs
  :: HowToConnect
  -> Pico
  -> Word16
  -> Maybe Pico
  -> Maybe Word64
  -> IO (EKGF.AcceptorConfiguration, TF.AcceptorConfiguration (LogObject Text))
mkConfigs listenIt freq itemsNum benchSpeedFreq totalObjs = do
  stopEKGF <- newIORef False
  stopTF <- newIORef False
  loCounter <- newIORef (0 :: Word64)

  when benchMode $
    void . async $ runSpeedPrinter loCounter 0 stopTF

  case totalObjs of
    Nothing -> return ()
    Just tn -> do
      startTime <- getCurrentTime
      putStrLn $ "Start time: " <> show startTime
      void . async $ stopWhenTotalReached startTime loCounter tn stopTF stopEKGF

  let ekgConfig =
        EKGF.AcceptorConfiguration
          { EKGF.acceptorTracer    = if benchMode then nullTracer else contramap show stdoutTracer
          , EKGF.forwarderEndpoint = forEKGF listenIt
          , EKGF.requestFrequency  = secondsToNominalDiffTime freq
          , EKGF.whatToRequest     = EKGF.GetAllMetrics
            -- Currently, only TF works in bench mode.
          , EKGF.actionOnResponse  = if benchMode then (\_ -> return ()) else print
          , EKGF.shouldWeStop      = stopEKGF
          , EKGF.actionOnDone      = putStrLn "EKGF: we are done!"
          }
      tfConfig :: TF.AcceptorConfiguration (LogObject Text)
      tfConfig =
        TF.AcceptorConfiguration
          { TF.acceptorTracer    = if benchMode then nullTracer else contramap show stdoutTracer
          , TF.forwarderEndpoint = forTF listenIt
          , TF.whatToRequest     = TF.GetLogObjects itemsNum
            -- Currently, only TF works in bench mode.
          , TF.actionOnReply     = if benchMode then count loCounter else print
          , TF.shouldWeStop      = stopTF
          , TF.actionOnDone      = putStrLn "TF: we are done!"
          }
  return (ekgConfig, tfConfig)
 where
  forTF (LocalPipe p)      = TF.LocalPipe p
  forTF (RemoteSocket h p) = TF.RemoteSocket (pack h) (read p :: TF.Port)

  forEKGF (LocalPipe p)      = EKGF.LocalPipe p
  forEKGF (RemoteSocket h p) = EKGF.RemoteSocket (pack h) (read p :: EKGF.Port)

  benchMode = isJust benchSpeedFreq

  count :: IORef Word64 -> [LogObject Text] -> IO ()
  count loCounter los =
    atomicModifyIORef' loCounter $ \cnt -> (cnt + fromIntegral (length los), ())

  runSpeedPrinter loCounter diff stopTF =
    case benchSpeedFreq of
      Nothing -> return ()
      Just sp -> do
        let waitInMicroSecs = toMicroSecs . secondsToNominalDiffTime $ sp
        threadDelay waitInMicroSecs
        --Check should we stop...
        shouldIStop <- readIORef stopTF
        if shouldIStop
          then return ()
          else do
            n <- readIORef loCounter
            let newObjsNum = n - diff
            putStrLn $ "Bench mode: " <> show newObjsNum
                       <> " new LogObjects were received during last "
                       <> show waitInMicroSecs <> " mks."
            runSpeedPrinter loCounter n stopTF

  stopWhenTotalReached startTime loCounter totalObjsNum stopTF stopEKGF = do
    n <- readIORef loCounter
    if n < totalObjsNum
      then do
        threadDelay 1000
        stopWhenTotalReached startTime loCounter totalObjsNum stopTF stopEKGF
      else do
        stopTime <- getCurrentTime
        let timeDiff = stopTime `diffUTCTime` startTime
        putStrLn $ "Stop time: " <> show stopTime
        putStrLn $ show n <> " LogObjects were received during "
                          <> show timeDiff
        atomicModifyIORef' stopTF   $ const (True, ())
        atomicModifyIORef' stopEKGF $ const (True, ())

toMicroSecs :: NominalDiffTime -> Int
toMicroSecs dt = fromEnum dt `div` 1000000

runReConnector :: IO () -> Pico -> TVar ThreadId -> IO ()
runReConnector acceptor rcFreq tidVar = forever $ do
  putStrLn "ReConnect test, start acceptor..."
  withAsync acceptor $ \_ -> do
    threadDelay . toMicroSecs . secondsToNominalDiffTime $ rcFreq
    putStrLn "ReConnect test, stop acceptor..."
    tid <- readTVarIO tidVar
    putStrLn $ "KILL TID: " <> show tid
    killThread tid
