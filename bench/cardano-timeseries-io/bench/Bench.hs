{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import           Cardano.Timeseries.AsText (showT)
import           Cardano.Timeseries.Elab (elab, initialSt)
import           Cardano.Timeseries.Import.PlainCBOR
import           Cardano.Timeseries.Interp (interp)
import           Cardano.Timeseries.Interp.Config
import           Cardano.Timeseries.Interp.Value (Value)
import           Cardano.Timeseries.Query.Expr (Expr)
import           Cardano.Timeseries.Store
import           Cardano.Timeseries.Store.Tree (fromFlat)
import qualified Cardano.Timeseries.Surface.Expr.Parser as Surface.Parser

import           Control.Monad.Except (runExceptT)
import           Control.Monad.State.Strict (evalState)
import           Data.Text (Text)
import qualified Data.Text.IO as Text
import           Text.Megaparsec hiding (count)
import           Text.Megaparsec.Char (space)

import           Criterion.Main

-- Given a snapshots file
-- Given a query string
-- Benchmark evaluation of the query on flat & tree stores.

snapshotsFile :: FilePath
snapshotsFile = "data/6nodes_4hours_1mininterval.cbor"

query :: Text
query = "\
  \let start = epoch + 1762173870000ms in \
  \let period = 10m in \
  \let F = Forge_forged_counter[start; start + period] in \
  \increase F"

interpConfig :: Config
interpConfig = Config {defaultRangeSamplingRateMillis = 15 * 1000}

action :: Store s Double => (s, Expr) -> Value
action (store, q) =
  let Right !x = evalState (runExceptT $ interp interpConfig store mempty q 0) 0 in x

main :: IO ()
main = do
  content <- readFileSnapshots snapshotsFile
  let flatStore = snapshotsToFlatStore content
  let treeStore = fromFlat flatStore
  case parse (Surface.Parser.expr <* space <* eof) "input" query of
    Left err -> putStrLn (errorBundlePretty err)
    Right surfaceQuery -> do
      case evalState (runExceptT (elab surfaceQuery)) initialSt of
        Left err   -> Text.putStrLn err
        Right !q -> do
          Text.putStrLn (showT q)
          defaultMain
            [
              bench "flat" $ nf action (flatStore, q),
              bench "tree" $ nf action (treeStore, q)
            ]
