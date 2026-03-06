module Cardano.Timeseries.Domain.Types(MetricIdentifier, Label, Labelled, Timestamp, SeriesIdentifier) where

import           Prelude hiding (length)

import           Data.Set (Set)
import           Data.Text (Text)
import           Data.Word (Word64)

-- | Each series in the (metric) store can be identified by a metric name.
type MetricIdentifier = Text

type Label = Text

-- | Key-value pair of a label and its value.
type Labelled a = (Label, a)

-- | Series is identified by a set of labels. Hence the name.
type SeriesIdentifier = Set (Labelled Text)

type Timestamp = Word64
