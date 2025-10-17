module Cardano.Timeseries.Query.Expr(Function(..), Expr(..)) where

-- average(add($metric1, $metric2), min(5))

{- n ::= <integer>
 - x ::= <identifier>
 - s ::= <string-in-double-quotes>
 - c ::= add | mul | lte | and | mins | millis | eval | quantile | const | mean | mean_over_time | max
 - e ::= n | $x | c ( e, ..., e )
 -}


data Function = Add
              | Mul
              | Lte

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
              | BoolToScalar
              | Abs

              | Millis
              | Seconds
              | Mins
              | Hours
              | DurationToScalar

              | Now
              | Rewind
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
              | MkPair

              | Range
              | RangeWithRate

              | FilterByLabel
              | Filter
              | Label
              | Join
              | Map

              deriving Show

data Expr = Number Integer | Variable String | Str String | Application Function [Expr] deriving Show
