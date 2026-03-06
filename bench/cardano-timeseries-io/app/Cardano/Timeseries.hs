{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Cardano.Timeseries.Common
import Cardano.Timeseries.CLI
import Data.Text (Text)
import Options.Applicative
import Data.Foldable
import qualified Data.Map as Map
import qualified Data.Text.IO as Text
import Cardano.Timeseries.AsText (showT)
import Cardano.Timeseries.Store

main :: IO ()
main = do
  cmd <- execParser parseCommand
  store <- readStore cmd.store
  putStrLn "Metrics:"
  for_ (Map.keys store) $ \k ->
    Text.putStrLn ("  — " <> k <> "[" <> showMaybe (earliest store k) <> "ms; " <> showMaybe (latest store k) <> "ms]")
  case cmd.mode of
    Interactive -> repl store
    Execute query -> execute store query
  where
   showMaybe :: Show a => Maybe a -> Text
   showMaybe Nothing  = "N/A"
   showMaybe (Just x) = showT x

