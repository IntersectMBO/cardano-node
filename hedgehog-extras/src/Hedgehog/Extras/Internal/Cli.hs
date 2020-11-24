module Hedgehog.Extras.Internal.Cli
  ( argQuote
  ) where

import           Data.Bool
import           Data.Semigroup
import           Data.String

import qualified Data.List as L

-- | Format argument for a shell CLI command.
--
-- This includes automatically embedding string in double quotes if necessary, including any necessary escaping.
--
-- Note, this function does not cover all the edge cases for shell processing, so avoid use in production code.
argQuote :: String -> String
argQuote arg = if ' ' `L.elem` arg || '"' `L.elem` arg || '$' `L.elem` arg
  then "\"" <> escape arg <> "\""
  else arg
  where escape :: String -> String
        escape ('"':xs) = '\\':'"':escape xs
        escape ('\\':xs) = '\\':'\\':escape xs
        escape ('\n':xs) = '\\':'n':escape xs
        escape ('\r':xs) = '\\':'r':escape xs
        escape ('\t':xs) = '\\':'t':escape xs
        escape ('$':xs) = '\\':'$':escape xs
        escape (x:xs) = x:escape xs
        escape "" = ""
