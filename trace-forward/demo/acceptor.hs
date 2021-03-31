{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-orphans #-}

import           Control.Concurrent.STM.TBQueue (newTBQueueIO)
import           Control.Tracer (contramap, stdoutTracer)
import           Data.IORef (newIORef)
import           Data.Fixed (Pico)
import           Data.Text (Text, pack)
import           Data.Time.Clock (secondsToNominalDiffTime)
import           System.Environment (getArgs)
import           System.Exit (die)

import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

import           Trace.Forward.Acceptor (runTraceAcceptor)
import           Trace.Forward.Configuration (AcceptorConfiguration (..),
                                              HowToConnect (..), Port)
import           Trace.Forward.ReqResp (Request (..))

main :: IO ()
main = do
  -- Prepare the acceptor's configuration.
  (listenIt, freq) <- getArgs >>= \case
    [path, freq]       -> return (LocalPipe path, freq)
    [host, port, freq] -> return (RemoteSocket (pack host) (read port :: Port), freq)
    _                  -> die "Usage: demo-acceptor (pathToLocalPipe | host port) freqInSecs"
  weAreDone <- newIORef False
  let config :: AcceptorConfiguration Text
      config =
        AcceptorConfiguration
          { acceptorTracer    = contramap show stdoutTracer
          , forwarderEndpoint = listenIt
          , requestFrequency  = secondsToNominalDiffTime (read freq :: Pico)
          , whatToRequest     = GetLogObjects 10
          , actionOnResponse  = print
          , shouldWeStop      = weAreDone
          , actionOnDone      = putStrLn "We are done!"
          }

  -- Create an empty TBQueue where received 'LogObject's will be stored.
  queue <- newTBQueueIO 100

  -- Run the acceptor. It will listen to the forwarder, and after the connection
  -- will be established, the acceptor will periodically (using 'requestFrequency')
  -- ask for N 'LogObject's from the forwarder. After these 'LogObject's will be received,
  -- the acceptor will write them in the 'queue'.
  runTraceAcceptor config queue

-- We need it for 'AcceptorConfiguration a' (in this example it is 'Text').
instance ShowProxy Text
