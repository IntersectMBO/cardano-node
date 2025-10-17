module Data.Store.Point(Point(..), mostRecent) where

import           Data.Store.Common (Labelled, Metric, Timestamp)

data Point a = Point {
  metric :: Metric,
  labels :: [Labelled String],
  timestamp :: Timestamp,
  value :: a
}

mostRecent :: Point a -> Point a -> Point a
mostRecent u v = if timestamp u < timestamp v then v else u
