module Hedgehog.Extras.Stock.String
  ( strip
  , lastLine
  , firstLine
  ) where

import           Data.Function
import           Data.String

import qualified Data.List as L
import qualified Data.Text as T

-- | Strip whitepsace from the beginning and end of the string.
strip :: String -> String
strip = T.unpack . T.strip . T.pack

-- | Get the last line in the string
lastLine :: String -> String
lastLine = strip . L.unlines . L.reverse . L.take 1 . L.reverse . L.lines

-- | Get the first line in the string
firstLine :: String -> String
firstLine = strip . L.unlines . L.take 1 . L.lines
