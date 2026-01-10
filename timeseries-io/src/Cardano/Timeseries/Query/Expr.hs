module Cardano.Timeseries.Query.Expr(Function(..), Expr(..)) where
import Cardano.Timeseries.Domain.Identifier (Identifier)
import Data.List.NonEmpty (NonEmpty)

{- f ::= <floating-point>
 - x ::= <identifier>
 - s ::= <string-in-double-quotes>
 - c ::= add_instant_vector
 -     | mul_instant_vector
 -     | eq_instant_vector
 -     | not_eq_instant_vector
 -     | lt_instant_vector
 -     | lte_instant_vector
 -     | gt_instant_vector
 -     | gte_instant_vector
 -     | true
 -     | false
 -     | or
 -     | and
 -     | not
 -     | eq_bool
 -     | add_scalar
 -     | sub_scalar
 -     | mul_scalar
 -     | div_scalar
 -     | eq_scalar
 -     | not_eq_scalar
 -     | lt_scalar
 -     | lte_scalar
 -     | gt_scalar
 -     | gte_scalar
 -     | bool_to_scalar
 -     | instant_vector_to_scalar
 -     | minutes
 -     | seconds
 -     | milliseconds
 -     | hours
 -     | eval
 -     | quantile
 -     | avg
 -     | avg_over_time
 -     | quantile
 -     | quantile_over_time
 -     | min
 -     | max
 -     | abs
 -     | duration_to_scalar
 -     | add_duration
 -     | now
 -     | epoch
 -     | rewind
 -     | fast_forward
 -     | timestamp_to_scalar
 -     | sum_over_time
 -     | rate
 -     | increase
 -     | fst
 -     | snd
 -     | range
 -     | filter_by_label
 -     | filter
 -     | join
 -     | map
 - e{1} ::= f | x | s | (e{≥0}) | c | (e{≥0}, e{≥0})
 - e{0} ::= e{≥1} e{≥1} e{≥1} ... e{≥1} | \x -> e{≥0} | let x = e{≥0} in e{≥0}
 -}

data Function = AddInstantVectorScalar
              | MulInstantVectorScalar
              | EqInstantVectorScalar
              | LtInstantVectorScalar
              | LteInstantVectorScalar
              | GtInstantVectorScalar
              | GteInstantVectorScalar
              | NotEqInstantVectorScalar

              | True
              | False
              | And
              | Or
              | Not
              | EqBool

              | AddScalar
              | SubScalar
              | MulScalar
              | DivScalar
              | EqScalar
              | LtScalar
              | LteScalar
              | GtScalar
              | GteScalar
              | NotEqScalar
              | BoolToScalar
              | InstantVectorToScalar
              | Abs

              | Milliseconds
              | Seconds
              | Minutes
              | Hours
              | DurationToScalar
              | AddDuration

              | Now
              | Epoch
              | Rewind
              | FastForward
              | TimestampToScalar

              | AvgOverTime
              | SumOverTime
              | Avg
              | Quantile
              | Max
              | Min
              | Rate
              | Increase
              | QuantileOverTime

              | Fst
              | Snd

              | Range

              | FilterByLabel
              | Filter
              | Join
              | Map
              deriving Show

-- | This expression has the following property, assumed in the interpreter:
-- |  every expression can be given at most one type and can have at most one interpretation.
-- | The property essentially means that expressions like (a + b) can't be part of the language unless
-- | `+` has only one possible meaning, which is not that case (It can be addition of scalars and addition of instant vectors)
data Expr = Number Double
          | Variable Identifier
          | Str String
          | Builtin Function
          | Application Expr (NonEmpty Expr)
          | Lambda Identifier Expr
          | Let Identifier Expr Expr
          | MkPair Expr Expr deriving Show
