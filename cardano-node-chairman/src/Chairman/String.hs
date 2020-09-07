module Chairman.String
  ( strip
  , lastLine
  ) where

import           Data.Function
import           Data.String

import qualified Data.List as L
import qualified Data.Text as T

strip :: String -> String
strip = T.unpack . T.strip . T.pack

-- | Get the last line in the string
lastLine :: String -> String
lastLine =  L.unlines . L.reverse . L.take 1 . L.reverse . L.lines
