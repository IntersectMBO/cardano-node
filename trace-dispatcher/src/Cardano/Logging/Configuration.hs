{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Configuration where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Tracer as T
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import           Katip (Severity)

import           Cardano.Logging.Trace (filterTraceByPrivacy,
                     filterTraceBySeverity)
import           Cardano.Logging.Types

withConfig :: (MonadIO m) =>
     (TraceConfig -> Context -> b)
  -> (b -> Trace m a -> Trace m a)
  -> Trace m a
  -> m (Trace m a)
withConfig extract needsConfigFunc tr = do
    ref  <- liftIO (newIORef Nothing)
    pure $ T.arrow $ T.emit $ mkTrace ref
  where
    mkTrace ref (lc, Right a) = do
            condConfValue <- liftIO $ readIORef ref
            case condConfValue of
              Just confValue -> T.traceWith
                                    (needsConfigFunc confValue tr)
                                    (lc, Right a)
              Nothing        -> error $ "Unconfigured trace with context "
                                      ++ show (lcContext lc)
    mkTrace ref (lc, Left c) = do
      let ! confValue = extract c (lcContext lc)
      liftIO $ writeIORef ref (Just confValue)
      T.traceWith (needsConfigFunc confValue tr) (lc, Left c)

filterSeverityFromConfig :: (MonadIO m) =>
     Trace m a
  -> m (Trace m a)
filterSeverityFromConfig = withConfig getSeverity filterTraceBySeverity

filterPrivacyFromConfig :: (MonadIO m) =>
     Trace m a
  -> m (Trace m a)
filterPrivacyFromConfig = withConfig getPrivacy filterTraceByPrivacy

getSeverity :: TraceConfig -> Context -> SeverityF
getSeverity config context = coSeverity $ getOption config context

getPrivacy :: TraceConfig -> Context -> Privacy
getPrivacy config context = coPrivacy $ getOption config context

getOption :: TraceConfig -> Context -> ConfigOption
getOption config context = undefined
