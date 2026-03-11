{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Cardano.Timeseries.Surface.Expr(Expr(..), mkRange, mkApp, Loc, getLoc) where

import           Cardano.Timeseries.AsText
import           Cardano.Timeseries.Domain.Identifier (Identifier)
import           Cardano.Timeseries.Domain.Types (Label)
import           Cardano.Timeseries.Query.Expr (LabelConstraint)

import           Prelude hiding (Foldable (..))

import           Data.Foldable (Foldable (..))
import           Data.Set (Set)
import           Data.Text (Text, pack)
import           Data.Word (Word64)
import           Text.Megaparsec (SourcePos, sourcePosPretty)

-- ---- not ----
-- not < atom
-- ---- range ----
-- range < atom
-- ---- app ----
-- app < atom
-- app < not
-- app < range
-- ---- add ----
-- add < atom
-- add < not
-- add < range
-- add < app
-- ---- mul ----
-- mul < atom
-- mul < not
-- mul < range
-- mul < app
-- mul < add
-- ---- comp ----
-- comp < atom
-- comp < not
-- comp < range
-- comp < app
-- comp < add
-- comp < mul
-- ---- and ----
-- and < atom
-- and < not
-- and < range
-- and < app
-- and < add
-- and < mul
-- and < comp
-- ---- or ----
-- or < atom
-- or < not
-- or < range
-- or < app
-- or < add
-- or < mul
-- or < comp
-- or < and
-- ---- universe ----
-- universe < atom
-- universe < not
-- universe < range
-- universe < app
-- universe < add
-- universe < mul
-- universe < comp
-- universe < and
-- universe < or

-- s ::= <double-quoted-string>
-- lc ::= s = s | s != s         // label constraint
-- l̅c̅ ::= {lc, ..., .lc}
-- t{atom} ::= (t{≥ universe}, t{≥ universe})
--           | x
--           | epoch
--           | now
--           | true
--           | false
--           | <int>ms
--           | <int>s
--           | <int>m
--           | <int>h
--           | (t{≥ universe})
--           | <float>
--           | "<string>"
-- t{not} ::= !t{> not}
-- t{range} ::= t{> range} l̅c̅
--            | t{> range} [̅t̅{̅≥̅ ̅u̅n̅i̅v̅e̅r̅s̅e̅}̅;̅ ̅t̅{̅≥̅ ̅u̅n̅i̅v̅e̅r̅s̅e̅}̅]̅ ̅|̅ ̅t̅[̅t̅{̅≥̅ ̅u̅n̅i̅v̅e̅r̅s̅e̅}̅;̅ ̅t̅{̅≥̅ ̅u̅n̅i̅v̅e̅r̅s̅e̅}̅ ̅:̅ ̅t̅{̅≥̅ ̅u̅n̅i̅v̅e̅r̅s̅e̅}̅]̅
-- t{app} ::= fst t{> app} | snd t{> app}
--            | min t{> app} | max t{> app} | avg t{> app} | filter t{> app} t{> app}
--            | join t{> app} t{> app}
--            | map t{> app} t{> app}
--            | round t{> app}
--            | abs t{> app}
--            | increase t{> app}
--            | rate t{> app}
--            | avg_over_time t{> app}
--            | sum_over_time t{> app}
--            | quantile_over_time t{> app} t{> app}
--            | unless t{> app} t{> app}
--            | quantile_by (s, ..., s) t{> app} t{> app}
--            | earliest x
--            | latest x
--            | to_scalar t{> app}
--            | t{> app} t̅{̅>̅ ̅a̅p̅p̅}̅
-- t{mul} ::= t{> mul} *̅|̅/̅ ̅t̅{̅>̅ ̅m̅u̅l̅}̅
-- t{add} ::= t{> add} +̅|̅-̅ ̅t̅{̅>̅ ̅a̅d̅d̅}̅
-- t{comp} ::= t{> comp} == t{> comp} | t{> comp} != t{> comp} | t{> comp} < t{> comp} | t{> comp} <= t{> comp}
--            | t{> comp} > t{> comp} | t{> comp} >= t{> comp}
-- t{and} ::= t{> and} &̅&̅ ̅t̅{̅>̅ ̅a̅n̅d̅}̅
-- t{or}  ::= t{> or} |̅|̅ ̅t̅{̅>̅ ̅o̅r̅}̅
-- t{universe} ::= let x = t{> universe} in t{≥ universe} | \x -> t{≥ universe}

-- | Source location.
type Loc = SourcePos

instance AsText Loc where
  asText = pack . sourcePosPretty

data Expr =
    Let Loc Identifier Expr Expr
  | Lambda Loc Identifier Expr
  | Fst Loc Expr
  | Snd Loc Expr
  | MkPair Loc Expr Expr
  | Eq Loc Expr Expr
  | NotEq Loc Expr Expr
  | Lt Loc Expr Expr
  | Lte Loc Expr Expr
  | Gt Loc Expr Expr
  | Gte Loc Expr Expr
  | Add Loc Expr Expr
  | Sub Loc Expr Expr
  | Mul Loc Expr Expr
  | Div Loc Expr Expr
  | Not Loc Expr
  | Or Loc Expr Expr
  | And Loc Expr Expr
  | Milliseconds Loc Word64
  | Seconds Loc Word64
  | Minutes Loc Word64
  | Hours Loc Word64
  | Epoch Loc
  | Now Loc
  | Range Loc Expr Expr Expr (Maybe Expr)
  | FilterByLabel Loc (Set LabelConstraint) Expr
  | Max Loc Expr
  | Min Loc Expr
  | Avg Loc Expr
  | Filter Loc Expr Expr
  | Join Loc Expr Expr
  | Map Loc Expr Expr
  | Round Loc Expr
  | Abs Loc Expr
  | Increase Loc Expr
  | Rate Loc Expr
  | AvgOverTime Loc Expr
  | SumOverTime Loc Expr
  | QuantileOverTime Loc Expr Expr
  | Unless Loc Expr Expr
  | QuantileBy Loc (Set Label) Expr Expr
  | Earliest Loc Identifier
  | Latest Loc Identifier
  | Metrics Loc
  | ToScalar Loc Expr
  | Variable Loc Identifier
  | Str Loc Text
  | Number Loc Double
  | Truth Loc
  | Falsity Loc
  | App Loc Expr Expr deriving (Show)

getLoc :: Expr -> Loc
getLoc (Let l _ _ _) = l
getLoc (Lambda l _ _) = l
getLoc (Fst l _) = l
getLoc (Snd l _) = l
getLoc (MkPair l _ _) = l
getLoc (Eq l _ _) = l
getLoc (NotEq l _ _) = l
getLoc (Lt l _ _) = l
getLoc (Lte l _ _) = l
getLoc (Gt l _ _) = l
getLoc (Gte l _ _) = l
getLoc (Add l _ _) = l
getLoc (Sub l _ _) = l
getLoc (Mul l _ _) = l
getLoc (Div l _ _) = l
getLoc (Not l _) = l
getLoc (Or l _ _) = l
getLoc (And l _ _) = l
getLoc (Milliseconds l _) = l
getLoc (Seconds l _) = l
getLoc (Minutes l _) = l
getLoc (Hours l _) = l
getLoc (Epoch l) = l
getLoc (Now l) = l
getLoc (Range l _ _ _ _) = l
getLoc (FilterByLabel l _ _) = l
getLoc (Max l _) = l
getLoc (Min l _) = l
getLoc (Avg l _) = l
getLoc (Filter l _ _) = l
getLoc (Join l _ _) = l
getLoc (Map l _ _) = l
getLoc (Abs l _) = l
getLoc (Round l _) = l
getLoc (Increase l _) = l
getLoc (Rate l _) = l
getLoc (AvgOverTime l _) = l
getLoc (SumOverTime l _) = l
getLoc (QuantileOverTime l _ _) = l
getLoc (Unless l _ _) = l
getLoc (QuantileBy l _ _ _) = l
getLoc (Earliest l _) = l
getLoc (Latest l _) = l
getLoc (Metrics l) = l
getLoc (ToScalar l _) = l
getLoc (Variable l _) = l
getLoc (Str l _) = l
getLoc (Number l _) = l
getLoc (Truth l) = l
getLoc (Falsity l) = l
getLoc (App l _ _) = l

mkRange :: Loc -> Expr -> Either (Expr, Expr, Maybe Expr) (Set LabelConstraint) -> Expr
mkRange loc t (Left (a, b, c)) = Range loc t a b c
mkRange loc t (Right set) = FilterByLabel loc set t

mkApp :: Loc -> Expr -> [Expr] -> Expr
mkApp l = foldl' (App l)
