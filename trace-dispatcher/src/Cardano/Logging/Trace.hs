{-# LANGUAGE ScopedTypeVariables #-}


module Cardano.Logging.Trace where

import           Control.Monad (join)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Tracer as T
import qualified Control.Tracer.Arrow as TA
import           Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef,
                     writeIORef)
import           Data.Maybe (fromMaybe, isJust)
import           Data.Text (Text)
import           Katip (Katip, Severity (..))

import           Cardano.Logging.Types

-- | Adds a message object to a trace
traceWith :: Monad m => Trace m a -> a -> m ()
traceWith tr a = T.traceWith tr (emptyLoggingContext, a)

--- | Don't process further if the result of the selector function
---   is False.
filterTrace :: (Monad m) =>
     ((LoggingContext, a) -> Bool)
  -> Trace m a
  -> Trace m a
filterTrace ff = T.squelchUnless (\ (c, a) -> ff (c, a))

--- | Only processes messages further with a severity equal or greater as the
--- given one
filterTraceBySeverity :: (Monad m) =>
     SeverityF
  -> Trace m a
  -> Trace m a
filterTraceBySeverity minSeverity = filterTrace
  (\ (c, a) -> case lcSeverity c of
                Just s  -> fromEnum s >= fromEnum minSeverity
                Nothing -> True)

-- | Appends a name to the context.
-- E.g. appendName "out" $ appendName "middle" $ appendName "in" tracer
-- give the result: `in.middle.out`.
appendName :: Monad m => Text -> Trace m a -> Trace m a
appendName name = T.contramap
  (\ (lc,v) -> (lc {lcContext = name : lcContext lc}, v))

-- | Sets severity for the messages in this trace
setSeverity :: Monad m => Severity -> Trace m a -> Trace m a
setSeverity s = T.contramap
  (\ (lc,v) -> if isJust (lcSeverity lc)
                    then (lc,v)
                    else (lc {lcSeverity = Just s}, v))

-- | Sets severities for the messages in this trace based on the selector function
withSeverity :: Monad m => (a -> Severity) -> Trace m a -> Trace m a
withSeverity fs = T.contramap
  (\ (lc,v) -> if isJust (lcSeverity lc)
                    then (lc,v)
                    else (lc {lcSeverity = Just (fs v)}, v))

-- | Sets privacy for the messages in this trace
setPrivacy :: Monad m => PrivacyAnnotation -> Trace m a -> Trace m a
setPrivacy p = T.contramap
  (\ (lc,v) -> if isJust (lcPrivacy lc)
                    then (lc,v)
                    else (lc {lcPrivacy = Just p}, v))

-- | Folds the cata function with acc over a.
-- Uses an IORef to store the state
-- TODO: Build a foldTrace which uses something like an ArrowLoop for keeping
-- the state without using an IORef
foldTraceM
  :: forall a acc m . MonadIO m
  => (acc -> a -> acc)
  -> acc
  -> Trace m (Folding a acc)
  -> m (Trace m a)
foldTraceM cata initial tr = do
  ref <- liftIO (newIORef initial)
  let tr = mkTracer ref
  pure (T.Tracer tr)
 where
    mkTracer :: IORef acc -> TA.Tracer m (LoggingContext, a) ()
    mkTracer ref = T.emit
      (\(lc,a) -> do
        x' <- liftIO $ atomicModifyIORef' ref $ \x -> join (,) (cata x a)
        T.traceWith tr (lc, Folding x'))

-- | Folds the monadic cata function with acc over a.
-- Uses an IORef to store the state
foldTraceM'
  :: forall a acc m . MonadIO m
  => (acc -> a -> m acc)
  -> acc
  -> Trace m (Folding a acc)
  -> m (Trace m a)
foldTraceM' cata initial tr = do
  ref <- liftIO (newIORef initial)
  let tr = mkTracer ref
  pure (T.arrow tr)
 where
    mkTracer :: IORef acc -> TA.Tracer m (LoggingContext, a) ()
    mkTracer ref = T.emit
      (\(lc,v) -> do
        acc <- liftIO $ readIORef ref
        acc' <- cata acc v
        liftIO $ writeIORef ref acc'
        T.traceWith tr (lc, Folding acc'))

-- foldTrace
--   :: forall a acc m . MonadIO m
--   => (acc -> a -> acc)
--   -> acc
--   -> Trace m (Folding a acc)
--   -> Trace m a
-- foldTrace cata initial tr =
--   trace foldF
--     where
--       foldF (Emitting emits _noEmits) acc =
--         let x' = cata acc
--       foldF (Squelching     _       ) acc =           arr (const ())
--
-- trace :: (input -> feedback -> (output, feedback)) -> input -> output
-- trace f input = let (output, feedback) = f input feedback
--                 in output



-- | Allows to route to different tracers,
--   based on the message being processed.
routingTrace
  :: forall m a . Monad m
  => (a -> Trace m a)
  -> Trace m a
routingTrace rf = T.arrow $
  T.emit (\(lc,x) -> T.traceWith (rf x) (lc,x))
