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
import Cardano.Timeseries.Store.Tree (fromFlat)
import Cardano.Logging.Resources (readResourceStats)
import Cardano.Logging (forHuman)
import Cardano.Logging.Resources (ResourceStats)
import Cardano.Logging.Resources (Resources(..))

snapshotsFile :: String
snapshotsFile = "6nodes_4hours_1mininterval.cbor"

printStore :: Flat Double -> IO ()
printStore = traverse_ print

printQueryResult :: Either Error Value -> IO ()
printQueryResult (Left err) = putStrLn ("Error: " <> err)
printQueryResult (Right ok) = print ok

printStats :: ResourceStats -> IO ()
printStats stats =
  putStrLn $ "Alloc: " <> show ((fromIntegral (rAlloc stats) :: Double) / 1024 / 1024) <> "MB\n"
          <> "Live: " <> show ((fromIntegral (rLive stats) :: Double) / 1024 / 1024) <> "MB\n"
          <> "Heap: " <> show ((fromIntegral (rHeap stats) :: Double) / 1024 / 1024) <> "MB\n"
          <> "RSS: " <> show ((fromIntegral (rRSS stats) :: Double) / 1024 / 1024) <> "MB"

interactive :: Store s Double => s -> IO ()
interactive store = forever $ do
 Just stats <- readResourceStats
 putStrLn "----------"
 printStats stats
 putStrLn $ "Number of store entries: " <> show (count store)
 putStrLn "----------"
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
  interactive (fromFlat store)
