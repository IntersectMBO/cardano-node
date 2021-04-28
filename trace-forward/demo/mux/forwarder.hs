{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (withAsync)
import           Control.Monad (forever)
import           Control.Tracer (contramap, nullTracer, stdoutTracer)
import           Data.Fixed (Pico)
import           Data.Maybe (isJust)
import           Data.Text (Text, pack)
import           Data.Time.Clock (NominalDiffTime, secondsToNominalDiffTime)
import           System.Environment (getArgs)
import           System.Exit (die)

import           Cardano.BM.Data.LogItem (LogObject)

import qualified Trace.Forward.Configuration as TF

import qualified System.Metrics.Configuration as EKGF

import           Network.Forwarder (HowToConnect (..), launchForwarders)

main :: IO ()
main = do
  (howToConnect, freq, benchFillFreq, reConnectTest) <- do
    args <- getArgs
    if "--dc" `elem` args
      then
        case args of
          [host, port, "--dc", freq] ->
            return ( RemoteSocket host port
                   , 0.5
                   , Nothing
                   , Just (read freq :: Pico) -- This is how often the client will be shut down.
                   )
          _ -> die "Usage: demo-forwarder-mux host port --dc freqInSecs"
      else
        case args of
          [path, freq] ->
            return ( LocalPipe path
                   , read freq :: Pico
                   , Nothing
                   , Nothing
                   )
          [host, port, freq] ->
            return ( RemoteSocket host port
                   , read freq :: Pico
                   , Nothing
                   , Nothing
                   )
          [path, freq, "-b", ff] ->
            return ( LocalPipe path
                   , read freq :: Pico
                   , Just (read ff :: Pico)
                   , Nothing
                   )
          _ ->
            die "Usage: demo-forwarder-mux (pathToLocalPipe | host port) freqInSecs [-b fillFreqInSecs]"
  let configs = mkConfigs howToConnect freq benchFillFreq
  
  case reConnectTest of
    Nothing -> launchForwarders howToConnect benchFillFreq configs
    Just rcFreq -> runReConnector (launchForwarders howToConnect benchFillFreq configs) rcFreq

mkConfigs
  :: HowToConnect
  -> Pico
  -> Maybe Pico
  -> (EKGF.ForwarderConfiguration, TF.ForwarderConfiguration (LogObject Text))
mkConfigs howToConnect freq benchFillFreq = (ekgConfig, tfConfig)
 where
  ekgConfig =
    EKGF.ForwarderConfiguration
      { EKGF.forwarderTracer    = if benchMode then nullTracer else contramap show stdoutTracer
      , EKGF.acceptorEndpoint   = forEKGF howToConnect
      , EKGF.reConnectFrequency = secondsToNominalDiffTime freq
      , EKGF.actionOnRequest    = const (return ())
      }
  tfConfig =
    TF.ForwarderConfiguration
      { TF.forwarderTracer  = if benchMode then nullTracer else contramap show stdoutTracer
      , TF.acceptorEndpoint = forTF howToConnect
      , TF.nodeBasicInfo    = return [("NodeName", "node-1")]
      , TF.actionOnRequest  = const (return ())
      }

  forTF (LocalPipe p)      = TF.LocalPipe p
  forTF (RemoteSocket h p) = TF.RemoteSocket (pack h) (read p :: TF.Port)

  forEKGF (LocalPipe p)      = EKGF.LocalPipe p
  forEKGF (RemoteSocket h p) = EKGF.RemoteSocket (pack h) (read p :: EKGF.Port)

  benchMode = isJust benchFillFreq

toMicroSecs :: NominalDiffTime -> Int
toMicroSecs dt = fromEnum dt `div` 1000000

runReConnector :: IO () -> Pico -> IO ()
runReConnector forwarder rcFreq = forever $ do
  putStrLn "ReConnect test, start forwarder..."
  withAsync forwarder $ \_ -> do
    threadDelay . toMicroSecs . secondsToNominalDiffTime $ rcFreq
    putStrLn "ReConnect test, stop forwarder..."
