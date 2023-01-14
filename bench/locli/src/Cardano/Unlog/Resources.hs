{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveTraversable #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Unlog.Resources
  ( ResAccums
  , mkResAccums
  , updateResAccums
  , extractResAccums
  , computeResCDF
  , zeroObsoleteValues
  -- * Re-exports
  , Resources(..)
  ) where

import Cardano.Prelude

import Data.Accum
import Data.CDF
import Cardano.Util
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
  , rNetRd       = mkAccumDelta `divAccum` 1024
  , rNetWr       = mkAccumDelta `divAccum` 1024
  , rFsRd        = mkAccumDelta `divAccum` 1024
  , rFsWr        = mkAccumDelta `divAccum` 1024
  , rThreads     = mkAccumNew
  }

updateResAccums :: UTCTime -> ResourceStats -> ResAccums -> ResAccums
updateResAccums now rs ra =
  updateAccum now <$> rs <*> ra

-- | Obtain the current values in resource accumulators.
extractResAccums :: ResAccums -> Resources Word64
extractResAccums = (aCurrent <$>)

computeResCDF ::
  forall a
  .  [Centile]
  -> (a -> SMaybe (Resources Word64))
  -> [a]
  -> Resources (CDF I Word64)
computeResCDF centiles proj xs =
  cdf centiles
  <$> traverse identity (proj `mapSMaybe` xs)

zeroObsoleteValues :: Num a => Resources (a -> a)
zeroObsoleteValues =
  Resources
  { rCentiCpu    = identity
  , rCentiGC     = identity
  , rCentiMut    = identity
  , rGcsMajor    = const 0
  , rGcsMinor    = const 0
  , rRSS         = identity
  , rHeap        = identity
  , rLive        = identity
  , rAlloc       = const 0
  , rCentiBlkIO  = identity
  , rNetRd       = const 0
  , rNetWr       = const 0
  , rFsRd        = const 0
  , rFsWr        = const 0
  , rThreads     = identity
  }
