{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Cardano.Logging.Configuration where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Tracer as T
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map as Map
import           Katip (Severity)

import           Cardano.Logging.Trace (filterTraceByPrivacy,
                     filterTraceBySeverity)
import           Cardano.Logging.Types

withConfig :: (MonadIO m, Eq b) =>
     (TraceConfig -> Context -> b)
  -> (Maybe b -> Trace m a -> Trace m a)
  -> Trace m a
  -> m (Trace m a)
withConfig extract needsConfigFunc tr = do
    ref  <- liftIO (newIORef (Left Map.empty))
    pure $ T.arrow $ T.emit $ mkTrace ref
  where
    mkTrace ref (lc, Right a) = do
      eitherConf <- liftIO $ readIORef ref
      case eitherConf of
        Right val -> T.traceWith (needsConfigFunc (Just val) tr) (lc, Right a)
        Left map -> case Map.lookup (lcContext lc) map of
                      Just val -> T.traceWith
                                    (needsConfigFunc (Just val) tr)
                                    (lc, Right a)
                      Nothing  -> error $ "Unconfigured trace with context "
                                        ++ show (lcContext lc)
    mkTrace ref (lc, Left Reset) = do
      liftIO $ writeIORef ref (Left Map.empty)
      T.traceWith (needsConfigFunc Nothing tr) (lc, Left Reset)
    mkTrace ref (lc, Left (Config c)) = do
      let ! val = extract c (lcContext lc)
      eitherConf <- liftIO $ readIORef ref
      case eitherConf of
        Left map ->
          case Map.lookup (lcContext lc) map of
            Nothing -> do
              liftIO $ writeIORef ref $ Left (Map.insert (lcContext lc) val map)
              T.traceWith (needsConfigFunc (Just val) tr) (lc, Left (Config c))
            Just v  -> do
              if v == val
                then T.traceWith (needsConfigFunc (Just val) tr) (lc, Left (Config c))
                else error $ "Inconsistent trace configuration with context "
                                  ++ show (lcContext lc)
        Right val -> error $ "Trace not reset before reconfiguration "
                            ++ show (lcContext lc)
    mkTrace ref (lc, Left Optimize) = do
      eitherConf <- liftIO $ readIORef ref
      case eitherConf of
        Left cmap ->
          case Map.size cmap of
            0 ->  -- This will never be called!?
                  pure ()
            1 -> do
                  case Map.elems cmap of
                    [val] -> do
                      liftIO $ writeIORef ref $ Right val
                      T.traceWith (needsConfigFunc (Just val) tr) (lc, Left Optimize)
                    _   -> error "Cardano.Logging.Configuration>>withConfig: Impossible"
            _ -> T.traceWith (needsConfigFunc Nothing tr) (lc, Left Optimize)
        Right val -> error $ "Trace not reset before reconfiguration "
                            ++ show (lcContext lc)


filterSeverityFromConfig :: (MonadIO m) =>
     Trace m a
  -> m (Trace m a)
filterSeverityFromConfig = withConfig getSeverity filterTraceBySeverity

filterPrivacyFromConfig :: (MonadIO m) =>
     Trace m a
  -> m (Trace m a)
filterPrivacyFromConfig = withConfig getPrivacy filterTraceByPrivacy

getSeverity :: TraceConfig -> Context -> SeverityF
getSeverity config context = undefined

getPrivacy :: TraceConfig -> Context -> Privacy
getPrivacy config context = undefined

getOption :: TraceConfig -> Context -> ConfigOption
getOption config context = undefined
