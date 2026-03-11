module Cardano.Timeseries.Query.Expr(HoleIdentifier, LabelConstraint(..), Expr(..)) where
import           Cardano.Timeseries.Domain.Identifier (Identifier)
import           Cardano.Timeseries.Domain.Types (Label, Labelled)

import           Data.Set (Set)
import           Data.Text (Text)
import           Data.Word (Word64)

type HoleIdentifier = Int

data LabelConstraint = LabelConstraintEq (Labelled Text) | LabelConstraintNotEq (Labelled Text)
  deriving (Show, Eq, Ord)

-- | This expression has the following property, assumed in the interpreter:
-- |  every expression can be given at most one type and can have at most one interpretation.
-- | The property essentially means that expressions like (a + b) can't be part of the language unless
-- | `+` has only one possible meaning, which is not that case (It can be addition of scalars and addition of instant vectors)
data Expr = Number Double
          | Variable Identifier
          | Str String
          | Application Expr Expr
          | Lambda Identifier Expr
          | Let Identifier Expr Expr

          | AddInstantVectorScalar Expr Expr
          | SubInstantVectorScalar Expr Expr
          | MulInstantVectorScalar Expr Expr
          | DivInstantVectorScalar Expr Expr
          | EqInstantVectorScalar Expr Expr
          | LtInstantVectorScalar Expr Expr
          | LteInstantVectorScalar Expr Expr
          | GtInstantVectorScalar Expr Expr
          | GteInstantVectorScalar Expr Expr
          | NotEqInstantVectorScalar Expr Expr

          | True
          | False
          | And Expr Expr
          | Or Expr Expr
          | Not Expr
          | EqBool Expr Expr
          | NotEqBool Expr Expr

          | Milliseconds Word64
          | Seconds Word64
          | Minutes Word64
          | Hours Word64
          | DurationToScalar Expr
          | AddDuration Expr Expr
          | Now
          | Epoch
          | Rewind Expr Expr
          | FastForward Expr Expr
          | TimestampToScalar Expr

          | AddScalar Expr Expr
          | SubScalar Expr Expr
          | MulScalar Expr Expr
          | DivScalar Expr Expr
          | EqScalar Expr Expr
          | LtScalar Expr Expr
          | LteScalar Expr Expr
          | GtScalar Expr Expr
          | GteScalar Expr Expr
          | NotEqScalar Expr Expr
          | BoolToScalar Expr
          | InstantVectorToScalar Expr
          | Abs Expr
          | RoundScalar Expr

          | MkPair Expr Expr
          | Fst Expr
          | Snd Expr

          | AvgOverTime Expr
          | SumOverTime Expr
          | Avg Expr
          | QuantileBy (Set Label) Expr Expr
          | Quantile Expr Expr
          | Max Expr
          | Min Expr
          | Rate Expr
          | Increase Expr
          | QuantileOverTime Expr Expr


          | Filter Expr Expr
          | Map Expr Expr
          | Join Expr Expr

          | Range Expr Expr Expr (Maybe Expr)

          | Unless Expr Expr

          | FilterByLabel (Set LabelConstraint) Expr

          | Metrics

          | Hole HoleIdentifier deriving Show
