{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}


module Cardano.Logging.Trace where

import           Control.Arrow
import           Control.Monad (join)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.IO.Unlift
import qualified Control.Tracer as T
import qualified Control.Tracer.Arrow as TA
import           Data.Maybe (fromMaybe, isJust)
import           Data.Text (Text)
import           UnliftIO.MVar

import           Cardano.Logging.Types

-- | Convenience function for tracing a message with a name
--   As the simple name suggest, this should be the standard function
trace :: (Monad m) => Trace m a -> Text -> a -> m ()
trace tr n = traceWith (appendName n tr)

-- | Adds a message object to a trace
traceWith :: (Monad m) => Trace m a -> a -> m ()
traceWith (Trace tr) a = T.traceWith tr (emptyLoggingContext, Nothing, a)

--- | Don't process further if the result of the selector function
---   is False.
filterTrace :: (Monad m) =>
     ((LoggingContext, Maybe TraceControl, a) -> Bool)
  -> Trace m a
  -> Trace m a
filterTrace ff (Trace tr) = Trace $ T.squelchUnless
    (\case
      (lc, Just Reset, a) -> True
      (lc, Just Optimize, a) -> True
      (lc, mbC, a) -> ff (lc, mbC, a))
      tr

--- | Keep the Just values and forget about the Nothings
filterTraceMaybe :: Monad m =>
     Trace m a
  -> Trace m (Maybe a)
filterTraceMaybe (Trace tr) = Trace $
    T.squelchUnless
      (\case
        (lc, mbC, Just a)  -> True
        (lc, mbC, Nothing) -> False)
      (T.contramap
          (\case
            (lc, mbC, Just a)   -> (lc, mbC, a)
            (lc, mbC, Nothing)  -> error "filterTraceMaybe: impossible")
          tr)

--- | Only processes messages further with a severity equal or greater as the
--- given one
filterTraceBySeverity :: Monad m =>
     Maybe SeverityF
  -> Trace m a
  -> Trace m a
filterTraceBySeverity (Just minSeverity) =
    filterTrace $
      \case
        (lc, Just Reset, a) -> True
        (lc, Just Optimize, a) -> True
        (c, _, e) -> case lcSeverity c of
                    Just s  -> fromEnum s >= fromEnum minSeverity
                    Nothing -> True
filterTraceBySeverity Nothing = id

-- | Appends a name to the context.
-- E.g. appendName "specific" $ appendName "middle" $ appendName "general" tracer
-- give the result: `general.middle.specific`.
appendName :: Monad m => Text -> Trace m a -> Trace m a
appendName name (Trace tr) = Trace $
    T.contramap
      (\ case
        (lc, mbC, e) -> (lc {lcNamespace = name : lcNamespace lc}, mbC, e))
      tr

-- | Sets severity for the messages in this trace
setSeverity :: Monad m => SeverityS -> Trace m a -> Trace m a
setSeverity s (Trace tr) = Trace $ T.contramap
  (\ (lc, mbC, e) -> if isJust (lcSeverity lc)
                        then (lc, mbC, e)
                        else (lc {lcSeverity = Just s}, mbC, e))
  tr

-- | Sets severities for the messages in this trace based on the selector function
withSeverity :: Monad m => (a -> SeverityS) -> Trace m a -> Trace m a
withSeverity fs (Trace tr) = Trace $
    T.contramap
      (\case
        (lc, mbC, e) -> if isJust (lcSeverity lc)
                            then (lc, mbC, e)
                            else (lc {lcSeverity = Just (fs e)}, mbC, e))
      tr

--- | Only processes messages further with a privacy greater then the given one
filterTraceByPrivacy :: (Monad m) =>
     Maybe Privacy
  -> Trace m a
  -> Trace m a
filterTraceByPrivacy (Just minPrivacy) = filterTrace $
    \case
      (lc, Just Reset, a) -> True
      (lc, Just Optimize, a) -> True
      (c, mbC, e) -> case lcPrivacy c of
                        Just s  -> fromEnum s >= fromEnum minPrivacy
                        Nothing -> True
filterTraceByPrivacy Nothing = id

-- | Sets privacy for the messages in this trace
setPrivacy :: Monad m => Privacy -> Trace m a -> Trace m a
setPrivacy p (Trace tr) = Trace $
  T.contramap
    (\ (lc, mbC, v) -> if isJust (lcPrivacy lc)
                      then (lc, mbC, v)
                      else (lc {lcPrivacy = Just p}, mbC, v))
    tr

-- | Sets severities for the messages in this trace based on the selector function
withPrivacy :: Monad m => (a -> Privacy) -> Trace m a -> Trace m a
withPrivacy fs (Trace tr) = Trace $
    T.contramap
      (\case
        (lc, mbC, e) -> if isJust (lcPrivacy lc)
                            then (lc, mbC, e)
                            else (lc {lcPrivacy = Just (fs e)}, mbC, e))
      tr

-- | Sets detail level for the messages in this trace
setDetails :: Monad m => DetailLevel -> Trace m a -> Trace m a
setDetails p (Trace tr) = Trace $
    T.contramap
      (\ (lc, mbC, v) -> if isJust (lcDetails lc)
                        then (lc, mbC, v)
                        else (lc {lcDetails = Just p}, mbC, v))
      tr

-- | Sets severities for the messages in this trace based on the selector function
withDetails :: Monad m => (a -> DetailLevel) -> Trace m a -> Trace m a
withDetails fs (Trace tr) = Trace $
  T.contramap
    (\case
      (lc, mbC, e) -> if isJust (lcDetails lc)
                          then (lc, mbC, e)
                          else (lc {lcDetails = Just (fs e)}, mbC, e))
    tr

-- | Folds the cata function with acc over a.
-- Uses an MVar to store the state
foldTraceM
  :: forall a acc m . (MonadUnliftIO m, MonadIO m)
  => (acc -> a -> acc)
  -> acc
  -> Trace m (Folding a acc)
  -> m (Trace m a)
foldTraceM cata initial (Trace tr) = do
  ref <- liftIO (newMVar initial)
  let trr = mkTracer ref
  pure $ Trace (T.Tracer trr)
 where
    mkTracer ref = T.emit $
      \ (lc, mbC, v) -> do
          x' <- modifyMVar ref $ \x ->
            let ! accu = cata x v
            in pure $ join (,) accu
          T.traceWith tr (lc, mbC, Folding x')

-- | Folds the monadic cata function with acc over a.
-- Uses an IORef to store the state
foldMTraceM
  :: forall a acc m . (MonadUnliftIO m, MonadIO m)
  => (acc -> a -> m acc)
  -> acc
  -> Trace m (Folding a acc)
  -> m (Trace m a)
foldMTraceM cata initial (Trace tr) = do
  ref <- liftIO (newMVar initial)
  let trr = mkTracer ref
  pure $ Trace (T.arrow trr)
 where
    mkTracer ref = T.emit $
      \ (lc, mbC, v) -> do
          x' <- modifyMVar ref $ \x -> do
            ! accu <- cata x v
            pure $ join (,) accu
          T.traceWith tr (lc, mbC, Folding x')

-- | Allows to route to different tracers, based on the message being processed.
--   The second argument must mappend all possible tracers of the first
--   argument to one tracer. This is required for the configuration!
routingTrace
  :: forall m a . Monad m
  => (a -> Trace m a)
  -> Trace m a
  -> Trace m a
routingTrace rf rc = Trace $ T.arrow $ T.emit $
    \case
      (lc, Just Reset, a) -> T.traceWith (unpackTrace rc) (lc, Just Reset, a)
      (lc, Just Optimize, a) -> T.traceWith (unpackTrace rc) (lc, Just Optimize, a)
      (lc, mbC, a) -> T.traceWith (unpackTrace (rf a)) (lc, mbC, a)
