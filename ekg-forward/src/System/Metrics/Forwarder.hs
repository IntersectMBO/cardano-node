{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- This top-level module will be used by the forwarder app
-- (the app that collects EKG metrics and sends them to the acceptor).
module System.Metrics.Forwarder
  ( runEKGForwarder
  ) where

import           Control.Exception (SomeException, try)
import           Control.Concurrent (threadDelay)
import           Data.Time.Clock (NominalDiffTime)
import qualified System.Metrics as EKG

import           System.Metrics.Configuration (ForwarderConfiguration (..))
import           System.Metrics.Network.Forwarder (connectToAcceptor)

-- | Please note that forwarder is a client from the __networking__ point of view:
-- it establishes network connection with the acceptor.
runEKGForwarder
  :: ForwarderConfiguration  -- ^ Forwarder configuration.
  -> EKG.Store               -- ^ The store the forwarder will take EKG metrics from.
  -> IO ()
runEKGForwarder config@ForwarderConfiguration{..} ekgStore =
  try (connectToAcceptor config ekgStore) >>= \case
    Left (_e :: SomeException) -> do
      threadDelay $ toMicroSecs reConnectFrequency
      runEKGForwarder config ekgStore
    Right _ -> return ()
 where
  toMicroSecs :: NominalDiffTime -> Int
  toMicroSecs dt = fromEnum dt `div` 1000000
