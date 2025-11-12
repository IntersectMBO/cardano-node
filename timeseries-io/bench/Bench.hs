{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
import           Cardano.Logging (forHuman)
import           Cardano.Logging.Resources (ResourceStats, Resources (..), readResourceStats)
import           Cardano.Timeseries.Import.PlainCBOR
import           Cardano.Timeseries.Query (interp)
import           Cardano.Timeseries.Query.Expr (Expr)
import           Cardano.Timeseries.Query.Parser (expr)
import           Cardano.Timeseries.Query.Value (Error, Value)
import           Cardano.Timeseries.Store
import           Cardano.Timeseries.Store.Flat (Flat, Point (instant, name))
import           Cardano.Timeseries.Store.Parser (points)
import           Cardano.Timeseries.Store.Tree (fromFlat)

import           Control.Monad (forever)
import           Control.Monad.Except (runExceptT)
import           Control.Monad.State.Strict (evalState)
import           Data.Attoparsec (skipMany)
import           Data.Attoparsec.Text (decimal, endOfInput, parseOnly, space)
import           Data.Either (fromRight)
import           Data.Foldable (for_, traverse_)
import           Data.Text (pack)
import           GHC.List (foldl')
import           System.Exit (die)
import           System.IO (hFlush, stdout)

import           Criterion.Main

-- Given a snapshots file
-- Given a query string
-- Bench mark evaluation of the query

snapshotsFile :: String
snapshotsFile = "data/6nodes_4hours_1mininterval.cbor"

query :: String
query = "\
  \let start = milliseconds 1762173870000 in \
  \let period = minutes 10 in \
  \let F = range Forge_forged_counter \
  \(fast_forward epoch start) \
  \(fast_forward epoch (add_duration start period)) in \
  \increase F"

action :: Store s Double => (s, Expr) -> Value
action (store, query) =
  let Right !x = evalState (runExceptT $ interp store mempty query 0) 0 in x

main :: IO ()
main = do
  content <- readFileSnapshots snapshotsFile
  let flatStore = snapshotsToFlatStore content
  let treeStore = fromFlat flatStore
  case parseOnly (expr <* skipMany space <* endOfInput) (pack query) of
    Left err -> putStrLn err
    Right !query -> defaultMain
      [
        bench "flat" $ nf action (flatStore, query),
        bench "tree" $ nf action (treeStore, query)
      ]

