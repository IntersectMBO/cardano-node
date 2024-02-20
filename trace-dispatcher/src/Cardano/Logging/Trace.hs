{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Logging.Trace (
    traceWith
  , withLoggingContext

  , filterTrace
  , filterTraceMaybe
  , filterTraceBySeverity
  , filterTraceByPrivacy

  , setSeverity
  , withSeverity
  , privately
  , setPrivacy
  , withPrivacy
  , allPublic
  , allConfidential
  , setDetails
  , withDetails

  , contramapM
  , contramapMCond
  , contramapM'
  , foldTraceM
  , foldCondTraceM
  , routingTrace

  , withNames
  , appendPrefixName
  , appendPrefixNames
  , appendInnerName
  , appendInnerNames
  , withInnerNames
  ) where

import           Cardano.Logging.Types

import           Control.Monad (forM_, join)
import           Control.Monad.IO.Unlift
import qualified Control.Tracer as T
import           Data.Maybe (isJust)
import           Data.Text (Text)

import           UnliftIO.MVar

-- | Adds a message object to a trace
traceWith :: Monad m => Trace m a -> a -> m ()
traceWith (Trace tr) a = T.traceWith tr (emptyLoggingContext, Right a)

--- | Don't process further if the result of the selector function
---   is False.
filterTrace :: (Monad m)
  => ((LoggingContext, a) -> Bool)
  -> Trace m a
  -> Trace m a
filterTrace ff (Trace tr) = Trace $ T.squelchUnless
    (\case
      (_lc, Left _)     -> True
      (lc, Right a)     -> ff (lc, a))
      tr

--- | Keep the Just values and forget about the Nothings
filterTraceMaybe :: Monad m
  => Trace m a
  -> Trace m (Maybe a)
filterTraceMaybe (Trace tr) = Trace $
    T.squelchUnless
      (\case
        (_lc, Left _ctrl)     -> True
        (_lc, Right (Just _)) -> True
        (_lc, Right Nothing)  -> False)
      (T.contramap
          (\case
            ( lc, Right (Just a))    -> (lc, Right a)
            (_lc, Right Nothing)     -> error "filterTraceMaybe: impossible"
            ( lc, Left ctrl)         -> (lc, Left ctrl))
          tr)

--- | Only processes messages further a severity equal or greater as the
--- given one
filterTraceBySeverity :: Monad m
  => Maybe SeverityF
  -> Trace m a
  -> Trace m a
filterTraceBySeverity (Just minSeverity) =
    filterTrace
      (\(lc, _) -> case lcSeverity lc of
                      Just s  -> case minSeverity of
                                    SeverityF (Just fs) -> s >= fs
                                    SeverityF Nothing   -> False
                      Nothing -> True)

filterTraceBySeverity Nothing = id

-- | Sets a new logging context for this message
withLoggingContext :: Monad m => LoggingContext -> Trace m a -> Trace m a
withLoggingContext lc (Trace tr) = Trace $
    T.contramap
      (\
        (_lc, cont) -> (lc, cont))
      tr

-- | Appends a name to the context.
-- E.g. appendName "specific" $ appendName "middle" $ appendName "general" tracer
-- give the result: `general.middle.specific`.
appendPrefixName :: Monad m => Text -> Trace m a -> Trace m a
appendPrefixName name (Trace tr) = Trace $
    T.contramap
      (\
        (lc, cont) -> (lc {lcNSPrefix = name : lcNSPrefix lc}, cont))
      tr

appendInnerName :: Monad m => Text -> Trace m a -> Trace m a
appendInnerName name (Trace tr) = Trace $
    T.contramap
      (\
        (lc, cont) -> (lc {lcNSInner = name : lcNSInner lc}, cont))
      tr

-- | Appends all names to the context.
{-# INLINE appendPrefixNames #-}
appendPrefixNames :: Monad m => [Text] -> Trace m a -> Trace m a
appendPrefixNames names (Trace tr) = Trace $
    T.contramap
      (\
        (lc, cont) -> (lc {lcNSPrefix = names ++ lcNSPrefix lc}, cont))
      tr

-- | Appends all names to the context.
appendInnerNames :: Monad m => [Text] -> Trace m a -> Trace m a
appendInnerNames names (Trace tr) = Trace $
    T.contramap
      (\
        (lc, cont) -> (lc {lcNSInner = names ++ lcNSInner lc}, cont))
      tr

-- | Sets names for the messages in this trace based on the selector function
{-# INLINE withInnerNames #-}
withInnerNames :: forall m a. (Monad m, MetaTrace a) => Trace m a -> Trace m a
withInnerNames (Trace tr) = Trace $
    T.contramap
      (\case
        (lc, Right a) -> (lc {lcNSInner = nsInner (namespaceFor a)}, Right a)
        (lc, Left c)  -> (lc, Left c))
      tr

-- | Sets names for the messages in this trace based on the selector function
--   and appends the provided names to the context.
{-# INLINE withNames #-}
withNames :: forall m a. (Monad m, MetaTrace a) => [Text] -> Trace m a -> Trace m a
withNames names (Trace tr) = Trace $
    T.contramap
      (\case
        (lc, Right a) -> (lc {lcNSPrefix = names,
                              lcNSInner  = nsInner (namespaceFor a)}, Right a)
        (lc, Left c)  -> (lc {lcNSPrefix = names}, Left c))
      tr


-- | Sets severity for the messages in this trace
setSeverity :: Monad m => SeverityS -> Trace m a -> Trace m a
setSeverity s (Trace tr) = Trace $
    T.contramap
      (\ (lc, cont) -> if isJust (lcSeverity lc)
                            then (lc, cont)
                            else (lc {lcSeverity = Just s}, cont))
      tr

-- | Sets severities for the messages in this trace based on the MetaTrace class
{-# INLINE withSeverity #-}
withSeverity :: forall m a. (Monad m, MetaTrace a) => Trace m a -> Trace m a
withSeverity (Trace tr) = Trace $
    T.contramap
      (\case
        (lc, Right e) -> process lc (Right e)
        (lc, Left c@(TCConfig _)) -> process lc (Left c)
        (lc, Left d@(TCDocument _ _)) -> process lc (Left d)
        (lc, Left e) -> (lc, Left e))
      tr
  where
    process lc cont@(Right v) =
      if isJust (lcSeverity lc)
        then (lc,cont)
        else (lc {lcSeverity = severityFor (Namespace [] (lcNSInner lc)
                                              :: Namespace a) (Just v)} , cont)
    process lc cont@(Left _) =
      if isJust (lcSeverity lc)
        then (lc,cont)
        else (lc {lcSeverity = severityFor (Namespace [] (lcNSInner lc)
                                              :: Namespace a) Nothing}, cont)

--- | Only processes messages further with a privacy greater then the given one
filterTraceByPrivacy :: (Monad m) =>
     Maybe Privacy
  -> Trace m a
  -> Trace m a
filterTraceByPrivacy (Just minPrivacy) = filterTrace $
    \(lc, _cont) ->
        case lcPrivacy lc of
          Just s  -> fromEnum s >= fromEnum minPrivacy
          Nothing -> True
filterTraceByPrivacy Nothing = id

allPublic :: a -> Privacy
allPublic _ = Public

allConfidential :: a -> Privacy
allConfidential _ = Confidential


-- | Sets privacy Confidential for the messages in this trace
privately :: Monad m => Trace m a -> Trace m a
privately = setPrivacy Confidential

-- | Sets privacy for the messages in this trace
setPrivacy :: Monad m => Privacy -> Trace m a -> Trace m a
setPrivacy p (Trace tr) = Trace $
  T.contramap
    (\ (lc, cont) -> if isJust (lcPrivacy lc)
                      then (lc, cont)
                      else (lc {lcPrivacy = Just p}, cont))
    tr

-- | Sets privacy for the messages in this trace based on the MetaTrace class
withPrivacy :: forall m a. (Monad m, MetaTrace a) => Trace m a -> Trace m a
withPrivacy (Trace tr) = Trace $
    T.contramap
      (\case
        (lc, Right e) -> process lc (Right e)
        (lc, Left c@(TCConfig _)) -> process lc (Left c)
        (lc, Left d@(TCDocument _ _)) -> process lc (Left d)
        (lc, Left e) -> (lc, Left e))
      tr
  where
    process lc cont@(Right v) =
      if isJust (lcPrivacy lc)
        then (lc,cont)
        else (lc {lcPrivacy = privacyFor (Namespace [] (lcNSInner lc)
                                              :: Namespace a) (Just v)} , cont)
    process lc cont@(Left _) =
      if isJust (lcPrivacy lc)
        then (lc,cont)
        else (lc {lcPrivacy = privacyFor (Namespace [] (lcNSInner lc)
                                              :: Namespace a) Nothing}, cont)

-- | Sets detail level for the messages in this trace
setDetails :: Monad m => DetailLevel -> Trace m a -> Trace m a
setDetails p (Trace tr) = Trace $
    T.contramap
      (\ (lc, cont) -> if isJust (lcDetails lc)
                        then (lc, cont)
                        else (lc {lcDetails = Just p}, cont))
      tr

-- | Sets detail level for the messages in this trace based on the message
withDetails :: forall m a. (Monad m, MetaTrace a) => Trace m a -> Trace m a
withDetails (Trace tr) = Trace $
  T.contramap
      (\case
        (lc, Right e) -> process lc (Right e)
        (lc, Left c@(TCConfig _)) -> process lc (Left c)
        (lc, Left d@(TCDocument _ _)) -> process lc (Left d)
        (lc, Left e) -> (lc, Left e))
      tr
  where
    process lc cont@(Right v) =
      if isJust (lcDetails lc)
        then (lc,cont)
        else (lc {lcDetails = detailsFor (Namespace [] (lcNSInner lc)
                                              :: Namespace a) (Just v)} , cont)
    process lc cont@(Left _) =
      if isJust (lcDetails lc)
        then (lc,cont)
        else (lc {lcDetails = detailsFor (Namespace [] (lcNSInner lc)
                                              :: Namespace a) Nothing}, cont)

-- | Contramap a monadic function over a trace
{-# INLINE contramapM #-}
contramapM :: Monad m
  => Trace m b
  -> ((LoggingContext, Either TraceControl a)
      -> m (LoggingContext, Either TraceControl b))
  -> m (Trace m a)
contramapM (Trace tr) mFunc =
  pure $ Trace $ T.Tracer $ T.emit rFunc
    where
      rFunc arg = do
        res <- mFunc arg
        T.traceWith tr res

-- | Contramap a monadic function over a trace
--   Can as well filter out messages
{-# INLINE contramapMCond #-}
contramapMCond :: Monad m
  => Trace m b
  -> ((LoggingContext, Either TraceControl a)
      -> m (Maybe (LoggingContext, Either TraceControl b)))
  -> m (Trace m a)
contramapMCond (Trace tr) mFunc =
  pure $ Trace $ T.Tracer $ T.emit rFunc
    where
      rFunc arg = do
        condMes <- mFunc arg
        forM_ condMes (T.traceWith tr)

{-# INLINE contramapM' #-}
contramapM' :: Monad m
  => ((LoggingContext, Either TraceControl a)
      -> m ())
  -> Trace m a
contramapM' rFunc =
  Trace $ T.Tracer $ T.emit rFunc

-- | Folds the monadic cata function with acc over a.
-- Uses an MVar to store the state
foldTraceM :: forall a acc m . (MonadUnliftIO m)
  => (acc -> LoggingContext -> a -> m acc)
  -> acc
  -> Trace m (Folding a acc)
  -> m (Trace m a)
foldTraceM cata initial (Trace tr) = do
  ref <- liftIO (newMVar initial)
  contramapM (Trace tr)
      (\case
        (lc, Right v) -> do
          x' <- modifyMVar ref $ \x -> do
            !accu <- cata x lc v
            pure $ join (,) accu
          pure (lc, Right (Folding x'))
        (lc, Left control) -> do
          pure (lc, Left control))

-- | Like foldTraceM, but filter the trace by a predicate.
foldCondTraceM :: forall a acc m . (MonadUnliftIO m)
  => (acc -> LoggingContext -> a -> m acc)
  -> acc
  -> (a -> Bool)
  -> Trace m (Folding a acc)
  -> m (Trace m a)
foldCondTraceM cata initial flt (Trace tr) = do
  ref <- liftIO (newMVar initial)
  contramapMCond (Trace tr) (foldF ref)
 where
    foldF ref =
      \case
        (lc, Right v) -> do
          x' <- modifyMVar ref $ \x -> do
            !accu <- cata x lc v
            pure $ join (,) accu
          if flt v
            then pure $ Just (lc, Right (Folding x'))
            else pure Nothing
        (lc, Left control) -> do
          pure $ Just (lc, Left control)

-- | Allows to route to different tracers, based on the message being processed.
--   The second argument must mappend all possible tracers of the first
--   argument to one tracer. This is required for the configuration!
routingTrace :: forall m a. Monad m
  => (a -> m (Trace m a))
  -> Trace m a
  -> Trace m a
routingTrace rf rc = contramapM'
    (\case
      (lc, Right a) -> do
          nt <- rf a
          T.traceWith (unpackTrace nt) (lc, Right a)
      (lc, Left control) ->
          T.traceWith (unpackTrace rc) (lc, Left control))

