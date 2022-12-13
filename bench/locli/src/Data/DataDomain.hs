{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Data.DataDomain
  ( module Data.DataDomain
  )
where

import Cardano.Prelude

import Cardano.Util


-- * DataDomain
--
data DataDomain f a
  = DataDomain
    { ddRaw           :: !(Interval (f a))
    , ddFiltered      :: !(Maybe (Interval (f a)))
    , ddRawCount      :: !(f Int)
    , ddFilteredCount :: !(f Int)
    }
  deriving (Generic)
-- Perhaps:  Plutus.V1.Ledger.Slot.SlotRange = Interval Slot
deriving instance (forall b. FromJSON b => FromJSON (f b), FromJSON a) => FromJSON (DataDomain f a)
deriving instance (forall b.   ToJSON b =>   ToJSON (f b),   ToJSON a) =>   ToJSON (DataDomain f a)
deriving instance (forall b.   NFData b =>   NFData (f b),   NFData a) =>   NFData (DataDomain f a)
deriving instance (forall b.     Show b =>     Show (f b),     Show a) =>     Show (DataDomain f a)

traverseDataDomain ::
     (forall b. [f b] -> h b)
  -> [DataDomain f a]
  -> DataDomain h a
traverseDataDomain f xs =
  DataDomain
  { ddRaw           = Interval (f $ xs <&> low . ddRaw) (f $ xs <&> high . ddRaw)
  , ddFiltered      = Just $ uncurry Interval
                             . both f
                             . unzip
                           $ (fmap (low &&& high) . ddFiltered) `mapMaybe` xs
  , ddRawCount      = f $ xs <&> ddRawCount
  , ddFilteredCount = f $ xs <&> ddFilteredCount
  }

dataDomainFilterRatio :: DataDomain I a -> Double
dataDomainFilterRatio DataDomain{..} =
  fromIntegral (unI ddFilteredCount) / fromIntegral (unI ddRawCount)

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
  , ddRawCount      =       I $ sum $ xs <&> unI . ddRawCount
  , ddFilteredCount =       I $ sum $ xs <&> unI . ddFilteredCount
  }
