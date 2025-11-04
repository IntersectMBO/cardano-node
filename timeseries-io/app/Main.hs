{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import           Cardano.Timeseries.Query (interp)
import           Cardano.Timeseries.Query.Parser (expr)
import           Cardano.Timeseries.Store
import           Cardano.Timeseries.Store.Flat (Flat, Point (instant, name))
import           Cardano.Timeseries.Store.Parser (points)

import           Data.Attoparsec (skipMany)
import           Data.Attoparsec.Text (decimal, parseOnly, space)
import           Data.Text (pack)
import           GHC.List (foldl')
import           System.Exit (die)

main :: IO ()
main = do
  putStrLn "This is a prototype!"
  string <- readFile "dataset.txt"
  let result = parseOnly (points decimal <* skipMany space) (pack string)
  case result of
    Left err -> die err
    Right dataset -> do
      print (dataset :: [Point Integer])
      let store = foldl' (\s p -> insert s (name p) (instant p)) (new :: Flat Double) (fmap (fmap fromIntegral) dataset)
      string <- readFile "expression.txt"
      let result = parseOnly (expr <* skipMany space) (pack string)
      case result of
        Left err -> die err
        Right expr -> do
          print expr
          
          -- no instance Show Value
          -- print $ interp store expr 15

