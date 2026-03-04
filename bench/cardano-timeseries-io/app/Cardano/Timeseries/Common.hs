{-# LANGUAGE FlexibleContexts #-}
{- HLINT ignore "Use print" -}

module Cardano.Timeseries.Common(readStore, repl, execute) where

import           Cardano.Logging.Resources (ResourceStats, Resources (..))
import           Cardano.Timeseries.AsText
import           Cardano.Timeseries.Elab (elab, initialSt)
import           Cardano.Timeseries.Import.PlainCBOR
import           Cardano.Timeseries.Interp (interp)
import           Cardano.Timeseries.Interp.Config (Config (..))
import           Cardano.Timeseries.Interp.Types (QueryError)
import           Cardano.Timeseries.Interp.Value (Value)
import           Cardano.Timeseries.Store
import           Cardano.Timeseries.Store.Flat (Flat)
import           Cardano.Timeseries.Store.Flat.Parser (double)
import qualified Cardano.Timeseries.Store.Flat.Parser as Flat.Parser
import           Cardano.Timeseries.Store.Tree (Tree, fromFlat)
import           Cardano.Timeseries.Surface.Expr.Parser (Parser)
import qualified Cardano.Timeseries.Surface.Expr.Parser as Surface.Parser

import           Control.DeepSeq (force)
import           Control.Monad (forever)
import           Control.Monad.Except (runExceptT)
import           Control.Monad.State.Strict (evalState)
import           Data.Foldable (traverse_)
import           Data.Functor (void)
import           Data.Text (Text, unpack)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           System.Exit (die)
import           System.FilePath (takeExtension)
import           System.IO (hFlush, stdout)
import           Text.Megaparsec hiding (count)
import           Text.Megaparsec.Char (newline, space, space1)

interpConfig :: Config
interpConfig = Config {defaultRangeSamplingRateMillis = 15 * 1000}

_printStore :: Flat Double -> IO ()
_printStore = traverse_ print

_printStats :: ResourceStats -> IO ()
_printStats stats =
  putStrLn $ "Alloc: " <> show ((fromIntegral (rAlloc stats) :: Double) / 1024 / 1024) <> "MB\n"
          <> "Live: " <> show ((fromIntegral (rLive stats) :: Double) / 1024 / 1024) <> "MB\n"
          <> "Heap: " <> show ((fromIntegral (rHeap stats) :: Double) / 1024 / 1024) <> "MB\n"
          <> "RSS: " <> show ((fromIntegral (rRSS stats) :: Double) / 1024 / 1024) <> "MB"

printQueryResult :: Either QueryError Value -> IO ()
printQueryResult (Left err) = Text.putStrLn $ asText err
printQueryResult (Right ok) = print ok

repl :: Store s Double => s -> IO ()
repl store = forever $ do
 -- Just stats <- readResourceStats
 -- putStrLn "----------"
 -- printStats stats
 putStrLn $ "Number of store entries: " <> show (count store)
 putStrLn "----------"
 putStr "> "
 hFlush stdout
 queryString <- Text.getLine
 case parse (Surface.Parser.expr <* space <* eof) "input" queryString of
   Left err -> putStrLn (errorBundlePretty err)
   Right surfaceQuery -> do
     -- putStrLn ("Surface expr: " <> show surfaceQuery)
     case evalState (runExceptT (elab surfaceQuery)) initialSt of
       Left err   -> Text.putStrLn err
       Right query -> do
         Text.putStrLn (showT query)
         printQueryResult (evalState (runExceptT $ interp interpConfig store mempty query 0) 0)

readStore :: FilePath -> IO (Tree Double)
readStore path | takeExtension path == ".cbor" = do
  content <- readFileSnapshots path
  putStrLn "Read the snapshots CBOR file!"
  let store = {-# SCC "XXX" #-} force $ fromFlat $ snapshotsToFlatStore content
  putStrLn "Created a store from CBOR!"
  pure store
readStore path | takeExtension path == ".txt" = do
  content <- Text.lines <$> Text.readFile path
  case traverse (parse (Flat.Parser.point double <* whitespace <* eof) "input") content of
    Left err -> die (errorBundlePretty err)
    Right store -> pure $ fromFlat store
readStore path = die $ "Unknown extension: " <> takeExtension path

whitespace :: Parser ()
whitespace = skipMany (try space1 <|> void newline)

execute :: Store s Double => s -> Text -> IO ()
execute store stringQuery = do
 case parse (Surface.Parser.expr <* space <* eof) "input" stringQuery of
   Left err -> putStrLn (errorBundlePretty err)
   Right surfaceQuery -> do
     putStrLn ("Expr: " <> show surfaceQuery)
     putStrLn "-----------"
     case evalState (runExceptT (elab surfaceQuery)) initialSt of
       Left err   -> die (unpack err)
       Right query -> do
         Text.putStrLn (showT query)
         printQueryResult (evalState (runExceptT $ interp interpConfig store mempty query 0) 0)
