module Cardano.Api.Json
  ( toRationalJSON
  ) where

import           Data.Aeson
import           Data.Either
import           Data.Maybe
import           Data.Scientific
import           GHC.Real

-- Rationals and JSON are an awkward mix. We cannot convert rationals
-- like @1/3@ to JSON numbers. But _most_ of the numbers we want to use
-- in practice have simple decimal representations. Our solution here is
-- to use simple decimal representations where we can and representation
-- in a @{"numerator": 1, "denominator": 3}@ style otherwise.
--
toRationalJSON :: Rational -> Value
toRationalJSON r =
  case fromRationalRepetendLimited 20 r of
    Right (s, Nothing) -> toJSON s
    _                  -> toJSON r
