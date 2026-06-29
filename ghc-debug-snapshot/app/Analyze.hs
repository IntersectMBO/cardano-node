{-# LANGUAGE ScopedTypeVariables #-}

-- | Offline analyzer for a ghc-debug snapshot (no live process required).
--
--   cardano-ghc-debug-analyze census  <snapshot> [out.tsv]
--       -hT-style closure-type census (count / total size / max per type).
--       Read off ARR_WORDS / STACK / TSO; diff two snapshots to isolate growth.
--
--   cardano-ghc-debug-analyze retain   <snapshot> [maxPaths] [minBytes]
--       Walk retainer chains of ARR_WORDS closures up to the GC roots, annotated
--       with IPE source locations -- i.e. what retains the leaked byte buffers.
--
--   cardano-ghc-debug-analyze threads  <snapshot>
--       Census every TSO grouped by (why_blocked | threadLabel) with count and
--       total stack size -- i.e. what the (leaked) threads ARE and what they are
--       blocked on. ouroboros labels its mini-protocol threads, so this names
--       per-connection threads directly.
module Main (main) where

import           GHC.Debug.Client
import           GHC.Debug.Profile (censusClosureType, closureCensusBy, writeCensusByClosureType)
import           GHC.Debug.Retainers (addLocationToStack, displayRetainerStack, findRetainers)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import           Data.List (sortBy)
import qualified Data.Map as Map
import           Data.Monoid (Sum (..))
import           Data.Ord (Down (..), comparing)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
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
    ("threads" : snap : _)   -> threadsMode snap
    _ -> die usage
  where
    listToMaybe (x:_) = Just x
    listToMaybe []    = Nothing
    usage = unlines
      [ "usage:"
      , "  cardano-ghc-debug-analyze census  <snapshot> [out.tsv]"
      , "  cardano-ghc-debug-analyze retain  <snapshot> [maxPaths] [minBytes]"
      , "  cardano-ghc-debug-analyze threads <snapshot>"
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

-- | Retainer chains of ARR_WORDS closures (payload >= minBytes), source-annotated.
retainMode :: FilePath -> Int -> Word -> IO ()
retainMode snap maxPaths minBytes = do
  hPutStrLn stderr ("ghc-debug: loading snapshot " <> snap)
  hPutStrLn stderr ("ghc-debug: finding up to " <> show maxPaths
                    <> " retainer chains of ARR_WORDS >= " <> show minBytes <> " bytes")
  stacks <- snapshotRun snap $ \e ->
    run e $ do
      _     <- precacheBlocks
      gcrts <- gcRoots
      paths <- findRetainers (Just maxPaths) gcrts $ \_ c ->
        pure $ case noSize c of
          ArrWordsClosure{ bytes = b } -> b >= minBytes
          _                            -> False
      mapM addLocationToStack paths
  hPutStrLn stderr ("ghc-debug: " <> show (length stacks) <> " retainer chains")
  displayRetainerStack
    (zipWith (\i s -> ("ARR_WORDS retainer #" <> show i, s)) [(1 :: Int) ..] stacks)

-- | Census every TSO by (why_blocked | threadLabel): count + total stack bytes.
threadsMode :: FilePath -> IO ()
threadsMode snap = do
  hPutStrLn stderr ("ghc-debug: loading snapshot " <> snap)
  m <- snapshotRun snap $ \e ->
    run e $ do
      _     <- precacheBlocks
      gcrts <- gcRoots
      closureCensusBy
        (\_ c -> case noSize c of
            TSOClosure{ why_blocked = wb, threadLabel = mlbl, tot_stack_size = sz } -> do
              lbl <- maybe (pure (T.pack "<unlabeled>")) readLabel mlbl
              pure (Just ( T.pack (show wb <> " | ") <> lbl
                         , (Sum (1 :: Int), Sum sz) ))
            _ -> pure Nothing)
        gcrts
  let rows  = sortBy (comparing (Down . getSum . fst . snd)) (Map.toList m)
      total = sum (map (getSum . fst . snd) rows)
  hPutStrLn stderr ("ghc-debug: " <> show total <> " TSOs in "
                    <> show (length rows) <> " (why_blocked | label) groups")
  putStrLn "count\tstack_KiB\twhy_blocked | threadLabel"
  mapM_ (\(k, (Sum n, Sum sz)) ->
           putStrLn (show n <> "\t" <> show (sz `div` 1024) <> "\t" <> T.unpack k))
        rows

-- | Read a thread label: deref the ClosurePtr (a ByteArray of UTF-8 bytes).
readLabel :: ClosurePtr -> DebugM Text
readLabel p = do
  sc <- dereferenceClosure p
  pure $ case noSize sc of
    ArrWordsClosure{ bytes = n, arrWords = ws } ->
      TE.decodeUtf8With TEE.lenientDecode (wordsToBS (fromIntegral n) ws)
    _ -> T.pack "<label?>"

-- | Reconstruct the first @n@ bytes from a little-endian [Word] payload.
wordsToBS :: Int -> [Word] -> BS.ByteString
wordsToBS n ws =
  BS.take n . BL.toStrict . BB.toLazyByteString
    $ foldMap (BB.word64LE . fromIntegral) ws
