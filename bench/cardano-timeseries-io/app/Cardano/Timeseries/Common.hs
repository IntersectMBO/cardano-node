{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{- HLINT ignore "Use print" -}

module Cardano.Timeseries.Common(readStore, repl, printInterpResult, printExecutionResult) where

import           Cardano.Logging.Resources (ResourceStats, Resources (..), readResourceStats)
import           Cardano.Timeseries.API
import           Cardano.Timeseries.Import.PlainCBOR

import           Control.DeepSeq (force)
import           Control.Monad (forever, when)
import           Control.Monad.Except (runExceptT)
import           Control.Monad.State.Strict (evalState)
import           Data.Functor (void)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           System.Exit (die)
import           System.FilePath (takeExtension)
import           System.IO (hFlush, stdout)
import           Text.Megaparsec hiding (count)
import           Text.Megaparsec.Char (newline, space, space1)

statsEnabled :: Bool
#ifdef STATS
statsEnabled = True
#else
statsEnabled = False
#endif

printStats :: ResourceStats -> IO ()
printStats stats =
  putStrLn $ "Alloc: " <> show ((fromIntegral (rAlloc stats) :: Double) / 1024 / 1024) <> "MB\n"
          <> "Live: " <> show ((fromIntegral (rLive stats) :: Double) / 1024 / 1024) <> "MB\n"
          <> "Heap: " <> show ((fromIntegral (rHeap stats) :: Double) / 1024 / 1024) <> "MB\n"
          <> "RSS: " <> show ((fromIntegral (rRSS stats) :: Double) / 1024 / 1024) <> "MB"

printInterpResult :: Either InterpError Value -> IO ()
printInterpResult (Left err) = Text.putStrLn $ asText err
printInterpResult (Right ok) = print ok

printExecutionResult :: Either ExecutionError Value -> IO ()
printExecutionResult (Left err) = Text.putStrLn $ asText err
printExecutionResult (Right ok) = print ok

repl :: Store s Double => s -> Config -> Timestamp -> IO ()
repl store interpCfg now = forever $ do
 putStrLn "----------"
 when statsEnabled $ do
   Just stats <- readResourceStats
   printStats stats
 putStrLn $ "Number of store entries: " <> show (count store)
 putStrLn "----------"
 putStr "> "
 hFlush stdout
 queryString <- Text.getLine
 case parse (expr <* space <* eof) "input" queryString of
   Left err -> putStrLn (errorBundlePretty err)
   Right surfaceQuery -> do
     case evalState (runExceptT (elab surfaceQuery)) initialSt of
       Left err   -> Text.putStrLn err
       Right query -> do
         Text.putStrLn (showT query)
         printInterpResult (evalState (runExceptT $ interp interpCfg store mempty query now) 0)

whitespace :: Parser ()
whitespace = skipMany (try space1 <|> void newline)

readStore :: FilePath -> IO (Tree Double)
readStore path | takeExtension path == ".cbor" = do
  content <- readFileSnapshots path
  putStrLn "Read the snapshots CBOR file!"
  let store = force $ fromFlat $ snapshotsToFlatStore content
  putStrLn "Created a store from CBOR!"
  pure store
readStore path | takeExtension path == ".txt" = do
  content <- Text.lines <$> Text.readFile path
  case traverse (parse (point double <* whitespace <* eof) "input") content of
    Left err -> die (errorBundlePretty err)
    Right store -> pure $ fromFlat store
readStore path = die $ "Unknown extension: " <> takeExtension path
