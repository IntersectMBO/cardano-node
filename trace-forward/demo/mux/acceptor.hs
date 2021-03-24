{-# LANGUAGE LambdaCase #-}

import           Control.Concurrent.Async (async, waitAnyCancel)
import           Control.Tracer (contramap, stdoutTracer)
import           Data.IORef (newIORef)
import           Data.Fixed (Pico)
import           Data.Text (Text, pack)
import           Data.Time.Clock (secondsToNominalDiffTime)
import           Data.Word (Word16)
import           System.Environment (getArgs)
import           System.Exit (die)

import qualified Trace.Forward.Acceptor as TF
import qualified Trace.Forward.Configuration as TF
import qualified Trace.Forward.ReqResp as TF

import qualified System.Metrics.Acceptor as EKGF
import qualified System.Metrics.Configuration as EKGF
import qualified System.Metrics.ReqResp as EKGF

import           Network.Acceptor (HowToConnect (..), launchAcceptors)

main :: IO ()
main = do
  (listenIt, freq, itemsNum) <- getArgs >>= \case
    [path, freq, n]       -> return (LocalPipe path,         read freq :: Pico, read n :: Word16)
    [host, port, freq, n] -> return (RemoteSocket host port, read freq :: Pico, read n :: Word16)
    _                     -> die "Usage: demo-acceptor-mux (pathToLocalPipe | host port) freqInSecs itemsNum"
  launchAcceptors listenIt =<< mkConfigs listenIt freq itemsNum

mkConfigs
  :: HowToConnect
  -> Pico
  -> Word16
  -> IO (EKGF.AcceptorConfiguration, TF.AcceptorConfiguration Text)
mkConfigs listenIt freq itemsNum = do
  stopEKGF <- newIORef False
  stopTF   <- newIORef False
  let ekgConfig =
        EKGF.AcceptorConfiguration
          { EKGF.acceptorTracer    = contramap show stdoutTracer
          , EKGF.forwarderEndpoint = forEKGF listenIt
          , EKGF.requestFrequency  = secondsToNominalDiffTime freq
          , EKGF.whatToRequest     = EKGF.GetAllMetrics
          , EKGF.actionOnResponse  = print
          , EKGF.shouldWeStop      = stopEKGF
          , EKGF.actionOnDone      = putStrLn "EKGF: we are done!"
          }
      tfConfig :: TF.AcceptorConfiguration Text
      tfConfig =
        TF.AcceptorConfiguration
          { TF.acceptorTracer    = contramap show stdoutTracer
          , TF.forwarderEndpoint = forTF listenIt
          , TF.requestFrequency  = secondsToNominalDiffTime freq
          , TF.whatToRequest     = TF.GetLogObjects itemsNum
          , TF.actionOnResponse  = print
          , TF.shouldWeStop      = stopTF
          , TF.actionOnDone      = putStrLn "TF: we are done!"
          }
  return (ekgConfig, tfConfig)
 where
  forTF (LocalPipe p)      = TF.LocalPipe p
  forTF (RemoteSocket h p) = TF.RemoteSocket (pack h) (read p :: TF.Port)

  forEKGF (LocalPipe p)      = EKGF.LocalPipe p
  forEKGF (RemoteSocket h p) = EKGF.RemoteSocket (pack h) (read p :: EKGF.Port)
