{-# LANGUAGE ScopedTypeVariables #-}

-- | Offline analyzer for a ghc-debug snapshot (no live process required).
--
--   cardano-ghc-debug-analyze census <snapshot> [out.tsv]
--       -hT-style closure-type census (count / total size / max per type).
--       Read off ARR_WORDS / STACK / TSO; diff two snapshots to isolate growth.
--
--   cardano-ghc-debug-analyze retain  <snapshot> [maxPaths] [minBytes]
--       Walk retainer chains of ARR_WORDS closures up to the GC roots and print
--       each path annotated with IPE source locations -- i.e. *what retains the
--       leaked byte buffers*. (ARR_WORDS is the dominant leak band; the same
--       shape extends to other closure constructors.)
module Main (main) where

import           GHC.Debug.Client
import           GHC.Debug.Profile (censusClosureType, writeCensusByClosureType)
import           GHC.Debug.Retainers (addLocationToStack, displayRetainerStack, findRetainers)

import qualified Data.Map as Map
import           System.Environment (getArgs)
import           System.Exit (die)
import           System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("census" : snap : rest) -> censusMode snap (listToMaybe rest)
    ("retain" : snap : rest) ->
      retainMode snap (maybe 20 read (listToMaybe rest))
                      (maybe 0 read (listToMaybe (drop 1 rest)))
    _ -> die usage
  where
    listToMaybe (x:_) = Just x
    listToMaybe []    = Nothing
    usage = unlines
      [ "usage:"
      , "  cardano-ghc-debug-analyze census <snapshot> [out.tsv]"
      , "  cardano-ghc-debug-analyze retain <snapshot> [maxPaths] [minBytes]"
      , "    minBytes: only ARR_WORDS payloads >= this many bytes (default 0)"
      ]

-- | Closure-type census written to a TSV (key:total:count:max:avg).
censusMode :: FilePath -> Maybe FilePath -> IO ()
censusMode snap mout = do
  let out = maybe (snap ++ ".census.tsv") id mout
  hPutStrLn stderr ("ghc-debug: loading snapshot " <> snap)
  snapshotRun snap $ \e -> do
    census <- run e $ do
      _     <- precacheBlocks
      gcrts <- gcRoots
      censusClosureType gcrts
    hPutStrLn stderr ("ghc-debug: distinct closure types = " <> show (Map.size census))
    writeCensusByClosureType out census
    hPutStrLn stderr ("ghc-debug: wrote closure-type census to " <> out)

-- | Retainer chains of ARR_WORDS closures, annotated with source locations.
-- Only ARR_WORDS whose payload is at least @minBytes@ are considered, so a
-- leaked heap full of small benign buffers doesn't crowd out the large leaked
-- ones in the (capped) sample.
retainMode :: FilePath -> Int -> Word -> IO ()
retainMode snap maxPaths minBytes = do
  hPutStrLn stderr ("ghc-debug: loading snapshot " <> snap)
  hPutStrLn stderr ("ghc-debug: finding up to " <> show maxPaths
                    <> " retainer chains of ARR_WORDS >= " <> show minBytes <> " bytes")
  stacks <- snapshotRun snap $ \e ->
    run e $ do
      _     <- precacheBlocks
      gcrts <- gcRoots
      -- Identify ARR_WORDS (raw byte buffers -- the dominant leak band),
      -- filtered to payloads of at least minBytes.
      paths <- findRetainers (Just maxPaths) gcrts $ \_ c ->
        pure $ case noSize c of
          ArrWordsClosure{ bytes = b } -> b >= minBytes
          _                            -> False
      mapM addLocationToStack paths
  hPutStrLn stderr ("ghc-debug: " <> show (length stacks) <> " retainer chains")
  displayRetainerStack
    (zipWith (\i s -> ("ARR_WORDS retainer #" <> show i, s)) [(1 :: Int) ..] stacks)
