module Cardano.Timeseries.Domain.Identifier(Identifier(..)) where

-- | Identifiers come in two sorts: Userspace and Machine-generated.
-- | The first kind comes from user-typed expressions.
-- | The second kind is used for automatic code-generation for hygienic scoping (i.e. to avoid unintentional variable capture)
data Identifier = User String | Machine Int deriving (Show, Ord, Eq)
