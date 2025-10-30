{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Store.Flat(Flat) where

import           Control.Applicative ((<|>))
import           Data.List (foldl', nub)
import           Data.Store.Common
import           Data.Store.Generic
import           Data.Store.Point (Point (..), mostRecent)

type Flat a = [Point a]

extract :: Point a -> ([Labelled String], Timestamp, a)
extract Point{..} = (labels, timestamp, value)

isSubsetOf :: forall a. (Eq a) => [a] -> [a] -> Bool
isSubsetOf xs ys = all (`elem` ys) (nub xs)

toMaybe :: Bool -> a -> Maybe a
toMaybe False _ = Nothing
toMaybe True x = Just x

instance Store (Flat a) a where
  insert :: Flat a -> Point a -> Flat a
  insert xs x = x : xs

  recent :: Flat a -> Metric -> [Labelled String] -> Maybe ([Labelled String], Timestamp, a)
  recent store targetMetric targetLabels = extract <$> foldl' choose Nothing store where
    choose acc p = choose acc (toMaybe (satisfies p) p) where

      -- | Does that point match target metric name and target label subset?
      satisfies :: Point a -> Bool
      satisfies x = metric x == targetMetric && targetLabels `isSubsetOf` labels x

      -- | Pick the most recent of the two, if any.
      choose :: Maybe (Point a) -> Maybe (Point a) -> Maybe (Point a)
      choose a b = mostRecent <$> a <*> b <|> a <|> b

  within :: Flat a -> Metric -> [Labelled String] -> Interval -> [([Labelled String], Timestamp, a)]
  within store targetMetric targetLabels interval = extract <$> filter satisfies store where

    -- | Does the point match target metric name, target label subset and target interval?
    satisfies :: Point a -> Bool
    satisfies p =
         metric p == targetMetric
      && targetLabels `isSubsetOf` labels p
      && start interval <= timestamp p && end interval <= timestamp p

