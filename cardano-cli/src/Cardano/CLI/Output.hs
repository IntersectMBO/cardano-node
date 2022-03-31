module Cardano.CLI.Output
  ( OutputAs(..)
  ) where

import Data.Eq
import Text.Show

data OutputAs = OutputAsJson | OutputAsText deriving (Eq, Show)
