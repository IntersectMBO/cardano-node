{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import           Cardano.Timeseries.Import.PlainCBOR
import           Cardano.Timeseries.Query (interp)
import           Cardano.Timeseries.Query.Parser (expr)
import           Cardano.Timeseries.Query.Value (Error, Value)
import           Cardano.Timeseries.Store
import           Cardano.Timeseries.Store.Flat (Flat, Point (instant, name))
import           Cardano.Timeseries.Store.Parser (points)

import           Control.Monad (forever)
import           Control.Monad.Except (runExceptT)
import           Control.Monad.State.Strict (evalState)
import           Data.Attoparsec (skipMany)
import           Data.Attoparsec.Text (decimal, endOfInput, parseOnly, space)
import           Data.Foldable (for_, traverse_)
import           Data.Text (pack)
import           GHC.List (foldl')
import           System.Exit (die)
import           System.IO (hFlush, stdout)

snapshotsFile :: String
snapshotsFile = "6nodes_4hours_1mininterval.cbor"

printStore :: Flat Double -> IO ()
printStore = traverse_ print

printQueryResult :: Either Error Value -> IO ()
printQueryResult (Left err) = putStrLn ("Error: " <> err)
printQueryResult (Right ok) = print ok

interactive :: Flat Double -> IO ()
interactive store = forever $ do
 putStr "> "
 hFlush stdout
 queryString <- getLine
 case parseOnly (expr <* skipMany space <* endOfInput) (pack queryString) of
   Left err -> putStrLn err
   Right query -> do
     putStrLn ("Expr: " <> show query)
     printQueryResult (evalState (runExceptT $ interp store mempty query 0) 0)

main :: IO ()
main = do
  content <- readFileSnapshots snapshotsFile
  let store = snapshotsToFlatStore content
  interactive store

main1 :: IO ()
main1 = do
  putStrLn "This is a prototype!"
  string <- readFile "dataset.txt"
  let result = parseOnly (points decimal <* skipMany space) (pack string)
  case result of
    Left err -> die err
    Right dataset -> do
      print (dataset :: [Point Integer])
      let store = foldl' (\s p -> insert s (name p) (instant p)) (new :: Flat Double) (fmap (fmap fromIntegral) dataset)
      string <- readFile "expression.txt"
      let result = parseOnly (expr <* skipMany space <* endOfInput) (pack string)
      case result of
        Left err -> die err
        Right expr -> do
          print expr

          -- no instance Show Value
          -- print $ interp store expr 15

