{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-orphans #-}

import           Control.Concurrent.STM.TBQueue (newTBQueueIO)
import           Control.Tracer (contramap, stdoutTracer)
import           Data.IORef (newIORef)
import           Data.Text (Text, pack)
import           System.Environment (getArgs)
import           System.Exit (die)

import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

import           Cardano.BM.Data.LogItem (LogObject)

import           Trace.Forward.Acceptor (runTraceAcceptor)
import           Trace.Forward.Configuration (AcceptorConfiguration (..),
                                              HowToConnect (..), Port)
import           Trace.Forward.LogObject ()
import           Trace.Forward.Protocol.Type (Request (..))

main :: IO ()
main = do
  -- Prepare the acceptor's configuration.
  listenIt <- getArgs >>= \case
    [path]       -> return $ LocalPipe path
    [host, port] -> return $ RemoteSocket (pack host) (read port :: Port)
    _            -> die "Usage: demo-acceptor (pathToLocalPipe | host port)"
  weAreDone <- newIORef False
  let config :: AcceptorConfiguration (LogObject Text)
      config =
        AcceptorConfiguration
          { acceptorTracer    = contramap show stdoutTracer
          , forwarderEndpoint = listenIt
          , whatToRequest     = GetLogObjects 10
          , actionOnReply     = print
          , shouldWeStop      = weAreDone
          , actionOnDone      = putStrLn "We are done!"
          }

  -- Create an empty TBQueue where received 'LogObject's will be stored.
  queue <- newTBQueueIO 100

  -- Create an empty store where received node's info will be stored.
  niStore <- newIORef []

  -- Run the acceptor. It will listen to the forwarder, and after the connection
  -- will be established, the acceptor will ask for N 'LogObject's from the forwarder.
  -- After these 'LogObject's will be received, the acceptor will write them in the 'queue'.
  runTraceAcceptor config queue niStore

-- We need it for 'AcceptorConfiguration lo' (in this example it is 'LogObject Text').
instance ShowProxy (LogObject Text)
