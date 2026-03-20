module Cardano.Timeseries.Elab.Expr.Head(Head(..)) where
import           Cardano.Timeseries.Domain.Identifier (Identifier)
import           Cardano.Timeseries.Domain.Types (Label)

import           Data.Set (Set)

data Head = Fst
          | Snd
          | Min
          | Max
          | Avg
          | Filter
          | Join
          | Map
          | Round
          | Abs
          | Increase
          | Rate
          | AvgOverTime
          | SumOverTime
          | QuantileOverTime
          | Unless
          | QuantileBy (Set Label)
          | Earliest Identifier
          | Latest Identifier
          | ToScalar
          | Metrics deriving (Show)
