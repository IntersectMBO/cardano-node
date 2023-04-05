{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-partial-fields -Wno-unused-matches -Wno-deprecations -Wno-unused-local-binds -Wno-incomplete-record-updates #-}

{- HLINT ignore "Avoid lambda" -}
{- HLINT ignore "Eta reduce" -}
{- HLINT ignore "Use head" -}

module Data.Profile
  ( ProfileEntry (..)
  , ProfilingData (..)
  , mkProfilingData
  , traverseProfilingDataCDF
  , profilingDataCDF
  , collapseProfilingDataCDF
  )
where

import Cardano.Prelude hiding (Text, head, show)
import Prelude ()

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Set  qualified as Set

import Data.CDF
import Cardano.Util


data ProfileEntry f
  = ProfileEntry
  { peFunc   :: T.Text
  , peModule :: T.Text
  , peSrcLoc :: T.Text
  , peTime   :: f Double
  , peAlloc  :: f Double
  }
  deriving (Generic)

deriving instance (forall a. FromJSON a => FromJSON (f a)) => FromJSON (ProfileEntry f)
deriving instance (forall a.   ToJSON a =>   ToJSON (f a)) =>   ToJSON (ProfileEntry f)
deriving instance (forall a.   NFData a =>   NFData (f a)) =>   NFData (ProfileEntry f)
deriving instance (forall a.     Show a =>     Show (f a)) =>     Show (ProfileEntry f)

newtype ProfilingData f
  = ProfilingData
  { pdMap :: Map T.Text (ProfileEntry f)
  }
  deriving (Generic)
deriving instance (forall a. FromJSON a => FromJSON (f a)) => FromJSON (ProfilingData f)
deriving instance (forall a.   ToJSON a =>   ToJSON (f a)) =>   ToJSON (ProfilingData f)
deriving instance (forall a.   NFData a =>   NFData (f a)) =>   NFData (ProfilingData f)
deriving instance (forall a.     Show a =>     Show (f a)) =>     Show (ProfilingData f)

mkProfilingData :: [ProfileEntry I] -> ProfilingData I
mkProfilingData xs =
  xs
  <&> (peFunc &&& identity)
   & Map.fromList
   & ProfilingData

traverseProfilingDataCDF :: Applicative h
  => ([f Double] -> h (CDF g Double)) -> [ProfilingData f]
  -> h (ProfilingData (CDF g))
traverseProfilingDataCDF _ [] = error "traverseProfilingDataCDF:  empty list"
traverseProfilingDataCDF f xs =
  ProfilingData
  <$> flip Map.traverseWithKey (Map.unions $ pdMap <$> xs)
             \fName ProfileEntry{peTime=_, peAlloc=_, ..} ->
               let fNameEntries = xs <&> Map.lookup fName . pdMap & catMaybes
               in (\peTime peAlloc -> ProfileEntry {..})
                  <$> f (fmap peTime  fNameEntries)
                  <*> f (fmap peAlloc fNameEntries)

-- WARNING: assumes that peFunc equality implies everything else being equal.
profilingDataCDF :: [Centile] -> [ProfilingData I] -> ProfilingData (CDF I)
profilingDataCDF cs = unI . traverseProfilingDataCDF (I . cdf cs . fmap unI)

collapseProfilingDataCDF ::
  [Centile] -> [ProfilingData (CDF I)]
  -> Either CDFError (ProfilingData (CDF I))
collapseProfilingDataCDF cs =
  traverseProfilingDataCDF (collapseCDFs (stdCombine1 cs))
