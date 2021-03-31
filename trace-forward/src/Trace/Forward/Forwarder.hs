{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- This top-level module will be used by the forwarder app
-- (the app that collects 'LogObject's and sends them to the acceptor).
module Trace.Forward.Forwarder
  ( runTraceForwarder
  ) where

import qualified Codec.Serialise as CBOR
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM.TBQueue (TBQueue)
import           Control.Exception (SomeException, try)
import           Data.Time.Clock (NominalDiffTime)
import           Data.Typeable (Typeable)

import           Cardano.BM.Data.LogItem (LogObject)

import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

import           Trace.Forward.Configuration (ForwarderConfiguration (..))
import           Trace.Forward.Network.Forwarder (connectToAcceptor)

-- | Please note that forwarder is a client from the __networking__ point of view:
-- it establishes network connection with the acceptor.
runTraceForwarder
  :: (CBOR.Serialise a,
      ShowProxy a,
      Typeable a)
  => ForwarderConfiguration a  -- ^ Forwarder configuration.
  -> TBQueue (LogObject a)     -- ^ The queue the forwarder will take 'LogObject's from.
  -> IO ()
runTraceForwarder config@ForwarderConfiguration{..} loQueue =
  try (connectToAcceptor config loQueue) >>= \case
    Left (_e :: SomeException) -> do
      threadDelay $ toMicroSecs reConnectFrequency
      runTraceForwarder config loQueue
    Right _ -> return ()
 where
  toMicroSecs :: NominalDiffTime -> Int
  toMicroSecs dt = fromEnum dt `div` 1000000
