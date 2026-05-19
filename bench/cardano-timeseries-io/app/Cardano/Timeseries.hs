{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import           Cardano.Timeseries.API
import           Cardano.Timeseries.CLI
import           Cardano.Timeseries.Common

import           Data.Foldable
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text.IO as Text
import           Options.Applicative

interpConfig :: Config
interpConfig = Config {defaultRangeSamplingRateMillis = 15 * 1000}

now :: Timestamp
now = 0

main :: IO ()
main = do
  cmd <- execParser parseCommand
  store <- readStore cmd.store
  putStrLn "Metrics:"
  for_ (Map.keys store) $ \k ->
    Text.putStrLn ("  — " <> k <> "[" <> showMaybe (earliest store k) <> "ms; " <> showMaybe (latest store k) <> "ms]")
  case cmd.mode of
    Interactive -> repl store interpConfig now
    ExecuteQuery query -> printExecutionResult $ execute store interpConfig now query
    ExecuteFile file -> do
      query <- Text.readFile file
      printExecutionResult $ execute store interpConfig now query
  where
   showMaybe :: Show a => Maybe a -> Text
   showMaybe Nothing  = "N/A"
   showMaybe (Just x) = showT x

