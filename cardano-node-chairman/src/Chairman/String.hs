module Chairman.String
  ( strip
  ) where

import           Data.Function
import           Data.String

import qualified Data.Text as T

strip :: String -> String
strip = T.unpack . T.strip . T.pack
