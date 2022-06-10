{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveTraversable #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Unlog.Resources
  ( ResAccums
  , mkResAccums
  , updateResAccums
  , extractResAccums
  , ResDistribProjections
  , computeResCDF
  , ResContinuity
  , discardObsoleteValues
  -- * Re-exports
  , Resources(..)
  ) where

import Cardano.Prelude

import Data.Accum
import Data.CDF
import Data.Time.Clock (UTCTime)

import Cardano.Logging.Resources.Types

deriving instance Foldable Resources
deriving instance Traversable Resources

-- | Resource accumulators
type ResAccums = Resources (Accum Word64 Word64)

mkResAccums :: ResAccums
mkResAccums =
  Resources
  { rCentiCpu    = mkAccumTicksShare
  , rCentiGC     = mkAccumTicksShare
  , rCentiMut    = mkAccumTicksShare
  , rGcsMajor    = mkAccumDelta
  , rGcsMinor    = mkAccumDelta
  , rRSS         = mkAccumNew   `divAccum` 1048576
  , rHeap        = mkAccumNew   `divAccum` 1048576
  , rLive        = mkAccumNew   `divAccum` 1048576
  , rAlloc       = mkAccumDelta `divAccum` 1048576
  , rCentiBlkIO  = mkAccumTicksShare
  , rThreads     = mkAccumNew
  }

updateResAccums :: UTCTime -> ResourceStats -> ResAccums -> ResAccums
updateResAccums now rs ra =
  updateAccum now <$> rs <*> ra

-- | Obtain the current values in resource accumulators.
extractResAccums :: ResAccums -> Resources Word64
extractResAccums = (aCurrent <$>)

type ResDistribProjections a = Resources (a -> Maybe Word64)

computeResCDF ::
  forall a
  .  [Centile]
  -> ResDistribProjections a
  -> [a]
  -> Resources (DirectCDF Word64)
computeResCDF centiles projs xs =
  compDist <$> projs
 where
   compDist :: (a -> Maybe Word64) -> DirectCDF Word64
   compDist proj = cdf centiles
     (catMaybes . toList $ proj <$> xs)

type ResContinuity a = Resources (a -> Maybe a)

discardObsoleteValues :: ResContinuity a
discardObsoleteValues =
  Resources
  { rCentiCpu    = Just
  , rCentiGC     = Just
  , rCentiMut    = Just
  , rGcsMajor    = const Nothing
  , rGcsMinor    = const Nothing
  , rRSS         = Just
  , rHeap        = Just
  , rLive        = Just
  , rAlloc       = const Nothing
  , rCentiBlkIO  = Just
  , rThreads     = Just
  }
