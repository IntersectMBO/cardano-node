{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Data.DataDomain
  ( module Data.DataDomain
  )
where

import Cardano.Prelude

import Cardano.Util


-- * DataDomain
--
data DataDomain a
  = DataDomain
    { ddRaw           :: !(Interval a)
    , ddFiltered      :: !(Maybe (Interval a))
    , ddRawCount      :: !Int
    , ddFilteredCount :: !Int
    }
  deriving (Generic, Show, ToJSON, FromJSON)
  deriving anyclass NFData
-- Perhaps:  Plutus.V1.Ledger.Slot.SlotRange = Interval Slot

dataDomainFilterRatio :: DataDomain a -> Double
dataDomainFilterRatio DataDomain{..} =
  fromIntegral ddFilteredCount / fromIntegral ddRawCount

mkDataDomainInj :: a -> a -> (a -> Int) -> DataDomain a
mkDataDomainInj f l measure =
  DataDomain (Interval f l) (Just (Interval f l)) delta delta
 where delta = measure l - measure f

mkDataDomain :: a -> a -> a -> a -> (a -> Int) -> DataDomain a
mkDataDomain f l f' l' measure =
  DataDomain (Interval f l) (Just (Interval f' l'))
             (measure l - measure f) (measure l' - measure f')

unionDataDomains :: Ord a => [DataDomain a] -> DataDomain a
unionDataDomains xs =
  DataDomain
  { ddRaw           = unionIntv $ xs <&> ddRaw
  , ddFiltered      = foldEmpty Nothing (Just . unionIntv) $ ddFiltered `mapMaybe` xs
  , ddRawCount      =       sum $ xs <&> ddRawCount
  , ddFilteredCount =       sum $ xs <&> ddFilteredCount
  }

intersectDataDomains :: Ord a => [DataDomain a] -> DataDomain a
intersectDataDomains xs =
  DataDomain
  { ddRaw           = intersectIntv $ xs <&> ddRaw
  , ddFiltered      = foldEmpty Nothing (Just . intersectIntv) $ ddFiltered `mapMaybe` xs
  , ddRawCount      =           sum $ xs <&> ddRawCount
  , ddFilteredCount =           sum $ xs <&> ddFilteredCount
  }
