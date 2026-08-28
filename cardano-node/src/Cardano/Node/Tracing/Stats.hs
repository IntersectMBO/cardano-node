{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Cardano.Node.Tracing.Stats
  ( Stats (..)
  , mkStatsTracer
  ) where

import           "contra-tracer" Control.Tracer (Tracer (..))
import qualified "contra-tracer" Control.Tracer as Tracer
import           Control.Concurrent.Class.MonadMVar
import           Control.Monad.Class.MonadTime.SI
import           GHC.TypeLits (KnownNat, Nat, SNat)
import           Data.TDigest (TDigest)
import           Data.Window.DigestTimeBatched (TimedDigestWindow)
import qualified Data.Window.DigestTimeBatched as Window


-- | Stats type.
--
-- This type is parametrised by `comp :: Nat` which configures `TDigest`
-- compression, and a custom `tag :: Type`, which allows to create different
-- instances of `LogFormatting` and associated tracing type classes.  For
-- example can use `window-stat` API output various percentiles as metric
-- gauges.
--
-- TODO: we start adding samples when the node starts, rather than when the
-- node is in sync.
newtype Stats (comp :: Nat) tag = Stats { tdigestWindow :: TDigest comp }

mkStatsTracer
  :: forall m (comp :: Nat) tag a.
     ( KnownNat comp
     , MonadMonotonicTime m
     , MonadMVar m
     )
  => DiffTime
  -- ^ bucket duration
  -> Int
  -- ^ retention window
  -> SNat comp
  -- ^ tdigest compression
  -> Int
  -- ^ minimal number of samples before returning `TDigest`
  -> Double
  -- ^ Maximum threshold for first measurement.  This allows to ignore results
  -- when the node is syncing and the results are heavily inflated.
  -> (a -> Maybe Double)
  -- ^ function which extracts value for which we build a CDF
  -> Tracer m (Stats comp tag)
  -- ^ Tracer which outputs statistics using `window-stat` API.
  -> m (Tracer m a)
mkStatsTracer d r _ minSamples threshold fn tr = do
    v <- newMVar (Window.empty d r :: TimedDigestWindow Time comp)
    return $ Tracer.traceMaybeM (fnM v) tr
  where
    fnM :: MVar m (TimedDigestWindow Time comp)
        -> a
        -> m (Maybe (Stats comp tag))
    fnM v a = case fn a of
      Nothing -> return Nothing
      Just b ->
        modifyMVar v $ \st -> do
          t <- getMonotonicTime
          let st' = if b >= threshold && Window.null st
                      -- ignore results greater than `threashold` while the
                      -- state is null
                      then st
                      else Window.insert (t, b) st
          return (st', if Window.sampleCount st' >= minSamples
                         then Stats <$> Window.windowDigest st'
                         else Nothing)
