{-# LANGUAGE LambdaCase #-}

import           Control.Concurrent.STM.TVar (newTVarIO)
import           Control.Tracer (contramap, stdoutTracer)
import           Data.Fixed (Pico)
import           Data.Text (pack)
import           Data.Time.Clock (secondsToNominalDiffTime)
import           System.Environment (getArgs)
import           System.Exit (die)
import qualified System.Metrics as EKG

import           System.Metrics.Acceptor (runEKGAcceptor)
import           System.Metrics.Configuration (AcceptorConfiguration (..),
                                               HowToConnect (..), Port)
import           System.Metrics.ReqResp (Request (..))

main :: IO ()
main = do
  -- Prepare the acceptor's configuration.
  (listenIt, freq) <- getArgs >>= \case
    [path, freq]       -> return (LocalPipe path, freq)
    [host, port, freq] -> return (RemoteSocket (pack host) (read port :: Port), freq)
    _                  -> die "Usage: demo-acceptor (pathToLocalPipe | host port) freqInSecs"
  weAreDone <- newTVarIO False
  let config =
        AcceptorConfiguration
          { acceptorTracer    = contramap show stdoutTracer
          , forwarderEndpoint = listenIt
          , requestFrequency  = secondsToNominalDiffTime (read freq :: Pico)
          , whatToRequest     = GetAllMetrics
          , shouldWeStop      = weAreDone
          }

  -- Create an empty EKG store.
  store <- EKG.newStore

  -- Run the acceptor. It will listen to the forwarder, and after the connection
  -- will be established, the acceptor will periodically (using 'requestFrequency')
  -- ask for the metrics from the forwarder. After these metrics will be received,
  -- the acceptor will put them in the 'store'.
  runEKGAcceptor config store
