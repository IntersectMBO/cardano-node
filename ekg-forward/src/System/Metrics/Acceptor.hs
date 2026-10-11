{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This top-level module will be used by the acceptor app
-- (the app that asks EKG metrics from the forwarder app).
module System.Metrics.Acceptor (
  runEKGAcceptor
  ) where

import           Control.Concurrent.STM.TVar (newTVarIO)
import           Control.Exception (SomeException, try)
import           Control.Monad (void)
import qualified System.Metrics as EKG

import           System.Metrics.Network.Acceptor (listenToForwarder)
import           System.Metrics.Store.Acceptor (emptyMetricsLocalStore, storeMetrics)
import           System.Metrics.Configuration (AcceptorConfiguration (..))

runEKGAcceptor
  :: AcceptorConfiguration  -- ^ Acceptor configuration.
  -> EKG.Store              -- ^ The store all received metrics will be stored in.
  -> IO ()
runEKGAcceptor config ekgStore =
  try (void $ listenToForwarder config mkStores insertStores peerErrorHandler) >>= \case
    Left (_e :: SomeException) -> runEKGAcceptor config ekgStore
    Right _ -> return ()
 where
  mkStores _ = do
    metricsStore <- newTVarIO emptyMetricsLocalStore
    return (ekgStore, metricsStore)

  insertStores _ (a, b) resp = storeMetrics resp a b

  peerErrorHandler _ = return ()
