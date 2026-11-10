{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | @cardano-debug@ — a tiny ghc-debug tool for cardano-node debug builds.
--
-- One executable, dispatching on its first argument:
--
--   cardano-debug snapshot <out> [sock]
--       Connect to a running, ghc-debug-instrumented process over its
--       @GHC_DEBUG_SOCKET@ unix socket, pause it, write a self-contained heap
--       snapshot to @out@, then close the connection (resuming the process)
--       and exit. This is the only subcommand that runs in the field; it
--       contains no analysis logic. The snapshot is portable and is analysed
--       later, offline, by the subcommands below.
--
--   cardano-debug census  <snap> [out.tsv]
--       -hT-style closure-type census (count / total size / max per type).
--       Read off ARR_WORDS / STACK / TSO; diff two snapshots to isolate growth.
--
--   cardano-debug retain  <snap> [maxPaths] [minBytes]
--       Walk retainer chains of ARR_WORDS closures up to the GC roots, annotated
--       with IPE source locations -- i.e. what retains the leaked byte buffers.
--
--   cardano-debug threads <snap>
--       Census every TSO grouped by (why_blocked | threadLabel) with count and
--       total stack size -- i.e. what the (leaked) threads ARE and what they are
--       blocked on. ouroboros labels its mini-protocol threads, so this names
--       per-connection threads directly.
--
-- The census/retain/threads subcommands run with *no live process* via
-- 'snapshotRun' (re-exported by "GHC.Debug.Client"); only @snapshot@ touches a
-- running node.
module Main (main) where

import           GHC.Debug.Client
import qualified GHC.Debug.Snapshot as Snapshot
import           GHC.Debug.Types.Ptr (InfoTablePtr (..))
import           GHC.Debug.Types.Version (Version (..))
import           GHC.Debug.Profile (censusClosureType, closureCensusBy, writeCensusByClosureType)
import           GHC.Debug.Retainers (addLocationToStack, displayRetainerStack, findRetainersOfArrWords, findRetainersOfInfoTable)

import           Control.Exception (bracket_)
import           Control.Monad (when)
import           Data.Aeson (encode, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import           Data.List (isPrefixOf, sortBy)
import qualified Data.Map as Map
import           Data.Monoid (Sum (..))
import           Data.Ord (Down (..), comparing)
import           Data.Word (Word64)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import           Data.Time.Clock (getCurrentTime)
import           GHC.Clock (getMonotonicTimeNSec)
import           System.Directory (doesPathExist, getFileSize)
import           System.Environment (getArgs, lookupEnv)
import           System.Exit (die)
import           System.IO (hPutStrLn, stderr)
import           Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["snapshot", "--help"]    -> putStrLn snapshotUsage
    ("snapshot" : out : rest)  -> snapshotMode out rest
    ("census"   : snap : rest) -> censusMode snap (listToMaybe rest)
    ("retain"   : snap : rest) ->
      retainMode snap (maybe 20 read (listToMaybe rest))
                      (maybe 0 read (listToMaybe (drop 1 rest)))
    ("threads"  : snap : _)    -> threadsMode snap
    ["retain-info", snap, addr] -> retainInfoMode snap addr "3"
    ["retain-info", snap, addr, limit] -> retainInfoMode snap addr limit
    ["census-info", snap] -> censusInfoMode snap
    _ -> die usage
  where
    listToMaybe (x:_) = Just x
    listToMaybe []    = Nothing
    usage = unlines
      [ "usage:"
      , "  cardano-debug snapshot <out> [sock] [options]  capture snapshot + .capture.json (field)"
      , "    See cardano-debug snapshot --help for optional capture context."
      , "  cardano-debug census   <snap> [out.tsv]  closure-type census (offline)"
      , "  cardano-debug retain   <snap> [maxPaths] [minBytes]  ARR_WORDS retainer chains (offline)"
      , "  cardano-debug threads  <snap>            TSO census by why_blocked|label (offline)"
      , "  cardano-debug retain-info <snap> <info-address> [maxPaths]  targeted retainer paths (offline; default 3)"
      , "  cardano-debug census-info <snap>         counts/bytes/source by snapshot info-table address"
      ]

snapshotUsage :: String
snapshotUsage = unlines
  [ "usage: cardano-debug snapshot <out> [sock] [options]"
  , "The socket defaults to GHC_DEBUG_SOCKET. Existing output files are refused."
  , "Writes <out>.capture.json after resuming, including pause-window seconds."
  , "Optional context (operator-supplied, not inferred from the node):"
  , "  --node-revision REV   deployed node's git revision"
  , "  --node-rts FLAGS      deployed node's RTS flags (quote as one argument)"
  , "  --tip TIP            chain tip at capture (e.g. slot/block/hash)"
  , "  --workload TEXT      load and peer context"
  , "  --reason TEXT        why this capture was taken"
  ]

-- | Connect to a live debuggee, pause it, write a self-contained snapshot, exit.
-- The sidecar deliberately contains no additional heap analysis.
snapshotMode :: FilePath -> [String] -> IO ()
snapshotMode out args = do
  let (mSockArg, opts) = case args of
        s : rest | not ("--" `isPrefixOf` s) -> (Just s, rest)
        _ -> (Nothing, args)
      sidecar = out <> ".capture.json"
  context <- parseContext Map.empty opts
  mapM_ (\path -> do
    exists <- doesPathExist path
    when exists (die ("Refusing to overwrite " <> path))) [out, sidecar]
  sock <- case mSockArg of
    Just s  -> pure s
    Nothing ->
      lookupEnv "GHC_DEBUG_SOCKET"
        >>= maybe (die "GHC_DEBUG_SOCKET is unset; pass the socket path as the 2nd argument") pure
  hPutStrLn stderr ("ghc-debug: connecting to debuggee socket " <> sock)
  (started, finished, seconds, ver) <- withDebuggeeConnect sock $ \e -> do
    ver <- run e version
    hPutStrLn stderr ("ghc-debug: pausing process, writing snapshot to " <> out)
    started <- getCurrentTime
    t0 <- getMonotonicTimeNSec
    bracket_ (pause e) (resume e) (run e (Snapshot.snapshot out))
    t1 <- getMonotonicTimeNSec
    finished <- getCurrentTime
    pure (started, finished, fromIntegral (t1 - t0) / 1e9 :: Double, ver)
  hPutStrLn stderr "ghc-debug: connection closed (process resumed)"
  hPutStrLn stderr ("ghc-debug: pause window = " <> show seconds <> " seconds (includes pause/resume handshakes)")
  bytes <- getFileSize out
  BL.writeFile sidecar $ encode $ object
    [ "schema_version" .= (1 :: Int)
    , "snapshot" .= out
    , "snapshot_bytes" .= bytes
    , "socket" .= sock
    , "pause_requested_at" .= started
    , "resumed_at" .= finished
    , "pause_window_seconds" .= seconds
    , "target_rts" .= object
        [ "version_major" .= v_major ver
        , "version_patch" .= v_patch ver
        , "profiling_mode" .= fmap show (v_profiling ver)
        , "tables_next_to_code" .= v_tntc ver
        ]
    , "operator_context" .= context
    ]
  hPutStrLn stderr ("ghc-debug: wrote capture metadata to " <> sidecar)
  where
    parseContext :: Map.Map String String -> [String] -> IO (Map.Map String String)
    parseContext ctx [] = pure ctx
    parseContext ctx (key : value : rest)
      | key `elem` ["--node-revision", "--node-rts", "--tip", "--workload", "--reason"] =
          if Map.member (drop 2 key) ctx
          then die ("Duplicate option: " <> key)
          else parseContext (Map.insert (drop 2 key) value ctx) rest
    parseContext _ _ = die snapshotUsage

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
      -- 0.8: findRetainers takes a ClosureFilter ADT; use the ready-made
      -- ArrWords helper (size arg is the `Size` newtype).
      paths <- findRetainersOfArrWords (Just maxPaths) gcrts (Size (fromIntegral minBytes))
      mapM addLocationToStack paths
  hPutStrLn stderr ("ghc-debug: " <> show (length stacks) <> " retainer chains")
  displayRetainerStack
    (zipWith (\i s -> ("ARR_WORDS retainer #" <> show i, s)) [(1 :: Int) ..] stacks)

-- | Find paths to closures allocated at one info table, including thunks.
-- Addresses belong to the captured binary; paths are examples, not exhaustive
-- ownership or retained-size estimates. Keep the result limit small.
retainInfoMode :: FilePath -> String -> String -> IO ()
retainInfoMode snap addrArg limitArg = do
  addr <- maybe (die "invalid info-table address (expected decimal or 0x-prefixed hex)") pure
                (readMaybe addrArg :: Maybe Word64)
  limit <- case readMaybe limitArg :: Maybe Int of
    Just n | n > 0 -> pure n
    _ -> die "maxPaths must be a positive integer"
  hPutStrLn stderr ("ghc-debug: loading snapshot " <> snap)
  hPutStrLn stderr ("ghc-debug: finding up to " <> show limit <> " paths to info table " <> addrArg)
  stacks <- snapshotRun snap $ \e -> run e $ do
    _ <- precacheBlocks
    roots <- gcRoots
    paths <- findRetainersOfInfoTable (Just limit) roots (InfoTablePtr addr)
    mapM addLocationToStack paths
  hPutStrLn stderr ("ghc-debug: " <> show (length stacks) <> " retainer paths")
  displayRetainerStack
    (zipWith (\i s -> ("Info-table retainer #" <> show i, s)) [(1 :: Int) ..] stacks)

-- | Resolve allocation sites using the snapshot itself, rather than assuming
-- an address from another artifact uses the same pointer convention/build.
censusInfoMode :: FilePath -> IO ()
censusInfoMode snap = snapshotRun snap $ \e -> do
  rows <- run e $ do
    _ <- precacheBlocks
    roots <- gcRoots
    counts <- closureCensusBy
      (\_ c -> let Size bytes = dcSize c
                in pure (Just (tableId (info (noSize c)), (Sum (1 :: Int), Sum bytes)))) roots
    mapM (\(ptr, counts') -> do
      loc <- getSourceInfo ptr
      pure (ptr, counts', loc)) (Map.toList counts)
  putStrLn "info_table\tcount\tbytes\tsource"
  mapM_ (\(ptr, (Sum n, Sum bytes), loc) ->
    putStrLn (show ptr <> "\t" <> show n <> "\t" <> show bytes <> "\t" <> show loc)) rows

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
