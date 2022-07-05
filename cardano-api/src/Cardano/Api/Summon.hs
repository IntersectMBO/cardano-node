module Cardano.Api.Summon
  ( Summon(..)
  ) where

import           Data.Either (Either (..))
import           Data.Text (Text)

-- Produce a value of the given type, or fail otherwise
class Summon a where
  summon :: Either Text a
