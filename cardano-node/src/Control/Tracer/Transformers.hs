{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Control.Tracer.Transformers
  ( counting
  , fanning
  , liftCounting
  , folding
  , liftFolding
  ) where

import           Cardano.Prelude hiding (atomically)

import           Data.IORef (IORef, newIORef, atomicModifyIORef')

import           Control.Tracer

import           Cardano.BM.Data.Aggregated (Measurable (..))
import           Cardano.BM.Data.LogItem (LOContent (..), LogObject (..),
                                          LOMeta, LoggerName)
import           Cardano.BM.Tracing

-- | A pure tracer combinator that allows to decide a further tracer to use,
--   based on the message being processed.
fanning
  :: forall m a
  . (a -> Tracer m a) -> Tracer m a
fanning fan = Tracer $ \x -> traceWith (fan x) x

newtype Counting a = Counting Int

-- | A stateful tracer transformer that substitutes messages with
--   a monotonically incrementing occurence count.
counting
  :: forall m a . (MonadIO m)
  => Tracer m (Counting a) -> m (Tracer m a)
counting tr =
  (liftIO $ newIORef 0)
  >>= pure . mkTracer
  where
    mkTracer :: IORef Int -> Tracer m a
    mkTracer ctrref = Tracer $ \_ -> do
      ctr <- liftIO $ atomicModifyIORef' ctrref $ \n -> join (,) (n + 1)
      traceWith tr (Counting ctr)

-- | Lift a 'Counting' tracer into a 'Trace' of 'PureI' messages.
liftCounting
  :: forall m a
  .  LOMeta -> [LoggerName] -> Text -> Trace m a
  -> Tracer m (Counting (LogObject a))
liftCounting meta name desc tr = Tracer (traceIncrement tr)
 where
   traceIncrement :: Trace m a -> Counting (LogObject a) -> m ()
   traceIncrement t (Counting n) = do
     traceWith t . LogObject name meta . LogValue desc . PureI $ fromIntegral n

newtype Folding a f = Folding f

-- | A generalised trace transformer that provides evolving state,
--   defined as a strict left fold.
folding
  :: forall m f a
  .  (MonadIO m)
  => (f -> a -> f) -> f -> Tracer m (Folding a f) -> m (Tracer m a)
folding cata initial tr =
  (liftIO $ newIORef initial)
  >>= pure . mkTracer
  where
    mkTracer :: IORef f -> Tracer m a
    mkTracer ref = Tracer $ \a -> do
      x' <- liftIO $ atomicModifyIORef' ref $ \x -> join (,) (cata x a)
      traceWith tr (Folding x')

-- | Lift a 'Folding' tracer into a 'Trace' of 'PureI' messages,
--   thereby specialising it to 'Integral'.
liftFolding
  :: forall m f a
  .  (Integral f) -- TODO:  generalise
  => LOMeta -> [LoggerName] -> Text -> Trace m a
  -> Tracer m (Folding (LogObject a) f)
liftFolding meta name desc tr = Tracer (traceIncrement tr)
 where
   traceIncrement :: Trace m a -> Folding (LogObject a) f -> m ()
   traceIncrement t (Folding f) = do
     traceWith t . LogObject name meta . LogValue desc . PureI $ fromIntegral f
