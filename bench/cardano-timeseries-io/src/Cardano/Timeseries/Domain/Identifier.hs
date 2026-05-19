module Cardano.Timeseries.Domain.Identifier(Identifier(..)) where

import           Cardano.Timeseries.AsText

import           Data.Text (Text, cons)

-- | Identifiers come in two sorts: Userspace and Machine-generated.
-- | The first kind comes from user-typed expressions.
-- | The second kind is used for automatic code-generation for hygienic scoping (i.e. to avoid unintentional variable capture)
data Identifier = User Text | Machine Int deriving (Show, Ord, Eq)

instance AsText Identifier where
  asText (User x)    = x
  asText (Machine i) = '$' `cons` showT i
