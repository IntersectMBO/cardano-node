{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-partial-fields #-}
module Data.DataDomain
  ( module Data.DataDomain
  )
where

import Cardano.Prelude

import Witherable qualified as Wither
import Data.List.NonEmpty qualified as NE

import Cardano.Util
import Data.CDF


-- * DataDomain
--
data DataDomain f a
  = DataDomain
    { ddRaw           :: !(Interval (f a))
    , ddFiltered      :: !(Maybe (Interval (f a)))
    , ddRawCount      :: !(f Int)
    , ddFilteredCount :: !(f Int)
    }
  deriving (Generic, Functor)
-- Perhaps:  Plutus.V1.Ledger.Slot.SlotRange = Interval Slot
deriving instance (forall b. FromJSON b => FromJSON (f b), FromJSON a) => FromJSON (DataDomain f a)
deriving instance (forall b.   ToJSON b =>   ToJSON (f b),   ToJSON a) =>   ToJSON (DataDomain f a)
deriving instance (forall b.   NFData b =>   NFData (f b),   NFData a) =>   NFData (DataDomain f a)
deriving instance (forall b.     Show b =>     Show (f b),     Show a) =>     Show (DataDomain f a)

-- | Key decision of DataDomain merging policy.
data DataDomainComb
  = DataDomainComb
    { ddcMergeIntervals :: !(forall a. Ord a =>
                             [Interval (I a)] -> Interval (I a))
    , ddcMergeCounts    :: !([I Int] -> I Int)
    , ddcProjInterval   :: !(forall a.
                             Interval (CDF I a) -> Interval (I a))
    , ddcProjCount      :: !(CDF I Int -> I Int)
    }

summariseDataDomains :: Divisible a => [DataDomain I a] -> DataDomain (CDF I) a
summariseDataDomains = traverseDataDomain (cdf briefCentiles . fmap unI)

traverseDataDomain ::
  (Divisible a, Wither.Witherable h) =>
     (forall b. Divisible b => h (f b) -> g b)
  -> h (DataDomain f a)
  -> DataDomain g a
traverseDataDomain f = unI . traverseDataDomain' (I . f)

traverseDataDomain' ::
  (Divisible a, Wither.Witherable h, Applicative i) =>
     (forall b. Divisible b => h (f b) -> i (g b))
  -> h (DataDomain f a)
  -> i (DataDomain g a)
traverseDataDomain' f xs =
  DataDomain
  <$> (Interval <$> f (xs <&> low . ddRaw) <*> f (xs <&> high . ddRaw))
  <*> (let lohis = NE.unzip $ (fmap (low &&& high) . ddFiltered) `Wither.mapMaybe` xs
       in Just <$> (Interval <$> f (fst lohis) <*> f (snd lohis)))
  <*> f (xs <&> ddRawCount)
  <*> f (xs <&> ddFilteredCount)

dataDomainFilterRatio :: (f Int -> Int) -> DataDomain f a -> Double
dataDomainFilterRatio proj DataDomain{..} =
  fromIntegral (proj ddFilteredCount) / fromIntegral (proj ddRawCount)

mkDataDomainInj :: a -> a -> (a -> Int) -> DataDomain I a
mkDataDomainInj f l measure =
  DataDomain (Interval (I f) (I l)) (Just (Interval (I f) (I l))) (I delta) (I delta)
 where delta = measure l - measure f

mkDataDomain :: a -> a -> a -> a -> (a -> Int) -> DataDomain I a
mkDataDomain f l f' l' measure =
  DataDomain (Interval (I f) (I l)) (Just (Interval (I f') (I l')))
             (I $ measure l - measure f) (I $ measure l' - measure f')

unionDataDomains :: Ord a => [DataDomain I a] -> DataDomain I a
unionDataDomains xs =
  DataDomain
  { ddRaw           = unionIntv $ xs <&> ddRaw
  , ddFiltered      = foldEmpty Nothing (Just . unionIntv) $ ddFiltered `mapMaybe` xs
  , ddRawCount      =   I $ sum $ xs <&> unI . ddRawCount
  , ddFilteredCount =   I $ sum $ xs <&> unI . ddFilteredCount
  }

intersectDataDomains :: Ord a => [DataDomain I a] -> DataDomain I a
intersectDataDomains xs =
  DataDomain
  { ddRaw           = intersectIntv $ xs <&> ddRaw
  , ddFiltered      = foldEmpty Nothing (Just . intersectIntv) $ ddFiltered `mapMaybe` xs
  , ddRawCount      =   I $ sum $ xs <&> unI . ddRawCount
  , ddFilteredCount =   I $ sum $ xs <&> unI . ddFilteredCount
  }
