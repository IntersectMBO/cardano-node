module Hedgehog.Extras.Stock.IO.File
  ( fileContains
  ) where

import           Data.Bool
import           Data.Functor
import           Data.String (String)
import           System.IO (FilePath, IO)

import qualified Data.List as L
import qualified System.IO as IO

-- | Determine if the given string is found in the given file.
fileContains :: String -> FilePath -> IO Bool
fileContains text path = (text `L.isInfixOf`) <$> IO.readFile path
