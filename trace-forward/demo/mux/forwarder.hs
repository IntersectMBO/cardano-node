{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async)
import           Control.Concurrent.STM.TBQueue (TBQueue, newTBQueueIO, writeTBQueue)
import           Control.Monad (forever)
import           Control.Monad.STM (atomically)
import           Control.Tracer (contramap, nullTracer, stdoutTracer)
import           Data.Fixed (Pico)
import           Data.Maybe (isJust)
import           Data.Text (Text, pack)
import           Data.Time.Clock (secondsToNominalDiffTime)
import           Data.Word (Word16)
import           System.Environment (getArgs)
import           System.Exit (die)

import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

import           Cardano.BM.Data.LogItem (LogObject (..), LOContent (..), LOMeta (..),
                                          PrivacyAnnotation (..), mkLOMeta)
import           Cardano.BM.Data.Severity (Severity (..))

import qualified Trace.Forward.Acceptor as TF
import qualified Trace.Forward.Configuration as TF
import qualified Trace.Forward.ReqResp as TF

import qualified System.Metrics.Acceptor as EKGF
import qualified System.Metrics.Configuration as EKGF
import qualified System.Metrics.ReqResp as EKGF

import           Network.Forwarder (HowToConnect (..), launchForwarders)

main :: IO ()
main = do
  (howToConnect, freq, benchFillFreq) <- getArgs >>= \case
    [path, freq] ->
      return (LocalPipe path, read freq :: Pico, Nothing)
    [host, port, freq] ->
      return (RemoteSocket host port, read freq :: Pico, Nothing)
    [path, freq, "-b", ff] ->
      return (LocalPipe path, read freq :: Pico, Just (read ff :: Pico))
    _ ->
      die "Usage: demo-forwarder-mux (pathToLocalPipe | host port) freqInSecs [-b fillFreqInSecs]"
  launchForwarders howToConnect benchFillFreq $ mkConfigs howToConnect freq benchFillFreq

mkConfigs
  :: HowToConnect
  -> Pico
  -> Maybe Pico
  -> (EKGF.ForwarderConfiguration, TF.ForwarderConfiguration Text)
mkConfigs howToConnect freq benchFillFreq = (ekgConfig, tfConfig)
 where
  ekgConfig =
    EKGF.ForwarderConfiguration
      { EKGF.forwarderTracer    = if benchMode then nullTracer else contramap show stdoutTracer
      , EKGF.acceptorEndpoint   = forEKGF howToConnect
      , EKGF.reConnectFrequency = secondsToNominalDiffTime freq
      , EKGF.actionOnRequest    = \_ -> return ()
      }
  tfConfig =
    TF.ForwarderConfiguration
      { TF.forwarderTracer    = if benchMode then nullTracer else contramap show stdoutTracer
      , TF.acceptorEndpoint   = forTF howToConnect
      , TF.reConnectFrequency = secondsToNominalDiffTime freq
      , TF.actionOnRequest    = \_ -> return ()
      }

  forTF (LocalPipe p)      = TF.LocalPipe p
  forTF (RemoteSocket h p) = TF.RemoteSocket (pack h) (read p :: TF.Port)

  forEKGF (LocalPipe p)      = EKGF.LocalPipe p
  forEKGF (RemoteSocket h p) = EKGF.RemoteSocket (pack h) (read p :: EKGF.Port)

  benchMode = isJust benchFillFreq
