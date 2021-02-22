{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}


module Cardano.Logging.Trace where

import           Control.Arrow
import           Control.Monad (join)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Tracer as T
import qualified Control.Tracer.Arrow as TA
import           Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef,
                     writeIORef)
import           Data.Maybe (fromMaybe, isJust)
import           Data.Text (Text)

import           Cardano.Logging.Types

-- | Adds a message object to a trace
traceWith :: (Monad m) => Trace m a -> a -> m ()
traceWith tr a = T.traceWith tr (emptyLoggingContext, Right a)

-- | Convenience function for naming a message when tracing
traceNamed :: (Monad m) => Trace m a -> Text -> a -> m ()
traceNamed tr n = traceWith (appendName n tr)

--- | Don't process further if the result of the selector function
---   is False.
filterTrace :: (Monad m) =>
     ((LoggingContext, a) -> Bool)
  -> Trace m a
  -> Trace m a
filterTrace ff = T.squelchUnless $
    \case
      (lc, Right a) -> ff (lc, a)
      (lc, Left  c) -> True

--- | Just keep the Just values and forget about the Nothings
filterTraceMaybe :: Monad m =>
     Trace m a
  -> Trace m (Maybe a)
filterTraceMaybe tr =
  T.squelchUnless
    (\case
      (lc, Right (Just a)) -> True
      (lc, Right Nothing)  -> False
      (lc, Left  c) -> True)
    (T.contramap
        (\case
          (lc, Right (Just a)) -> (lc, Right a)
          (lc, Right Nothing)  -> error "filterTraceMaybe: impossible"
          (lc, Left  c) -> (lc, Left  c))
        tr)

--- | Only processes messages further with a severity equal or greater as the
--- given one
filterTraceBySeverity :: (Monad m) =>
     Maybe SeverityF
  -> Trace m a
  -> Trace m a
filterTraceBySeverity (Just minSeverity) = filterTrace $
    \(c, e) -> case lcSeverity c of
                  Just s  -> fromEnum s >= fromEnum minSeverity
                  Nothing -> True
filterTraceBySeverity Nothing = id

-- | Appends a name to the context.
-- E.g. appendName "specific" $ appendName "middle" $ appendName "general" tracer
-- give the result: `general.middle.specific`.
appendName :: Monad m => Text -> Trace m a -> Trace m a
appendName name = T.contramap
  (\ (lc,e) -> (lc {lcContext = name : lcContext lc}, e))

-- | Sets severity for the messages in this trace
setSeverity :: Monad m => SeverityS -> Trace m a -> Trace m a
setSeverity s = T.contramap
  (\ (lc,e) -> if isJust (lcSeverity lc)
                    then (lc,e)
                    else (lc {lcSeverity = Just s}, e))

-- | Sets severities for the messages in this trace based on the selector function
withSeverity :: Monad m => (a -> SeverityS) -> Trace m a -> Trace m a
withSeverity fs = T.contramap $
    \case
      (lc, Right e) -> if isJust (lcSeverity lc)
                          then (lc, Right e)
                          else (lc {lcSeverity = Just (fs e)}, Right e)
      (lc, Left  c) -> (lc, Left c)

--- | Only processes messages further with a privacy greater then the given one
filterTraceByPrivacy :: (Monad m) =>
     Maybe Privacy
  -> Trace m a
  -> Trace m a
filterTraceByPrivacy (Just minPrivacy) = filterTrace $
    \(c, e) -> case lcPrivacy c of
                        Just s  -> fromEnum s >= fromEnum minPrivacy
                        Nothing -> True
filterTraceByPrivacy Nothing = id

-- | Sets privacy for the messages in this trace
setPrivacy :: Monad m => Privacy -> Trace m a -> Trace m a
setPrivacy p = T.contramap
  (\ (lc,v) -> if isJust (lcPrivacy lc)
                    then (lc,v)
                    else (lc {lcPrivacy = Just p}, v))

-- | Sets severities for the messages in this trace based on the selector function
withPrivacy :: Monad m => (a -> Privacy) -> Trace m a -> Trace m a
withPrivacy fs = T.contramap $
    \case
      (lc, Right e) -> if isJust (lcPrivacy lc)
                          then (lc, Right e)
                          else (lc {lcPrivacy = Just (fs e)}, Right e)
      (lc, Left  c) -> (lc, Left c)

-- | Sets detail level for the messages in this trace
setDetails :: Monad m => DetailLevel -> Trace m a -> Trace m a
setDetails p = T.contramap
  (\ (lc,v) -> if isJust (lcDetails lc)
                    then (lc,v)
                    else (lc {lcDetails = Just p}, v))

-- | Sets severities for the messages in this trace based on the selector function
withDetails :: Monad m => (a -> DetailLevel) -> Trace m a -> Trace m a
withDetails fs = T.contramap $
    \case
      (lc, Right e) -> if isJust (lcDetails lc)
                          then (lc, Right e)
                          else (lc {lcDetails = Just (fs e)}, Right e)
      (lc, Left  c) -> (lc, Left c)

-- | Folds the cata function with acc over a.
-- Uses an IORef to store the state
foldTraceM
  :: forall a acc m . MonadIO m
  => (acc -> a -> acc)
  -> acc
  -> Trace m (Folding a acc)
  -> m (Trace m a)
foldTraceM cata initial tr = do
  ref <- liftIO (newIORef initial)
  let trr = mkTracer ref
  pure (T.Tracer trr)
 where
    mkTracer ref = T.emit $
      \case
        (lc, Right v) -> do
          x' <- liftIO $ atomicModifyIORef' ref $ \x ->
            let ! accu = cata x v
            in join (,) accu
          T.traceWith tr (lc, Right (Folding x'))
        (lc, Left c) -> do
          T.traceWith tr (lc, Left c)

-- | Folds the monadic cata function with acc over a.
-- Uses an IORef to store the state
foldMTraceM
  :: forall a acc m . MonadIO m
  => (acc -> a -> m acc)
  -> acc
  -> Trace m (Folding a acc)
  -> m (Trace m a)
foldMTraceM cata initial tr = do
  ref <- liftIO (newIORef initial)
  let trr = mkTracer ref
  pure (T.arrow trr)
 where
    mkTracer ref = T.emit $
      \case
        (lc, Right v) -> do
          acc    <- liftIO $ readIORef ref
          ! acc' <- cata acc v
          liftIO $ writeIORef ref acc'
          T.traceWith tr (lc, Right (Folding acc'))
        (lc, Left c) -> do
          T.traceWith tr (lc, Left c)

-- | Allows to route to different tracers, based on the message being processed.
--   The second argument must mappend all possible tracers of the first
--   argument to one tracer. This is required for the configuration!
routingTrace
  :: forall m a . Monad m
  => (a -> Trace m a)
  -> Trace m a
  -> Trace m a
routingTrace rf rc = T.arrow $ T.emit $
    \case
      (lc, Right x) -> T.traceWith (rf x) (lc, Right x)
      (lc, Left  c) -> T.traceWith rc     (lc, Left  c)
