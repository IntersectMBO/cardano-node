{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Tracer.EKG where

import           Cardano.Logging.Types
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Tracer as T
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import           Data.Text (intercalate)
import qualified System.Metrics as Metrics
import qualified System.Metrics.Gauge as Gauge
import           System.Remote.Monitoring (Server, getGauge)

ekgTracer :: MonadIO m => Metrics.Store -> m (Trace m Int)
ekgTracer store = liftIO $ do
  registeredRef <- newIORef Map.empty
  pure $ T.arrow $ T.emit $ output registeredRef
    where
      output registeredRef (LoggingContext{..}, v) = liftIO $ do
        registeredMap <- readIORef registeredRef
        let name = intercalate "." lcContext
        case Map.lookup name registeredMap of
          Just gauge -> Gauge.set gauge (fromIntegral v)
          Nothing -> do
            gauge <- Metrics.createGauge name store
            let registeredMap' = Map.insert name gauge registeredMap
            writeIORef registeredRef registeredMap'
            Gauge.set gauge (fromIntegral v)

ekgTracer' :: MonadIO m => Server -> m (Trace m Int)
ekgTracer' store = liftIO $ do
  registeredRef <- newIORef Map.empty
  pure $ T.arrow $ T.emit $ output registeredRef
    where
      output registeredRef (LoggingContext{..}, v) = liftIO $ do
        registeredMap <- readIORef registeredRef
        let name = intercalate "." lcContext
        case Map.lookup name registeredMap of
          Just gauge -> Gauge.set gauge (fromIntegral v)
          Nothing -> do
            gauge <- getGauge name store
            let registeredMap' = Map.insert name gauge registeredMap
            writeIORef registeredRef registeredMap'
            Gauge.set gauge (fromIntegral v)
