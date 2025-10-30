module Data.Store.Common(Metric, Label, Labelled, Timestamp, Interval(..)) where

import           Data.Word (Word64)

type Metric = String
type Label = String
type Labelled a = (Label, a)
type Timestamp = Word64

data Interval = Interval {
  start :: Timestamp,
  end :: Timestamp
}
