{-# LANGUAGE ScopedTypeVariables #-}

-- | Headless ghc-debug heap snapshotter.
--
-- Connects to a running, ghc-debug-instrumented process over its
-- @GHC_DEBUG_SOCKET@ unix socket, pauses it, writes a self-contained heap
-- snapshot to a file, then closes the connection (which resumes the process)
-- and exits.
--
-- The resulting snapshot file is portable: it can be copied to another machine
-- and analysed offline (no live process required) with
-- 'GHC.Debug.Snapshot.snapshotRun', e.g. to walk the retainer chains of the
-- @ARR_WORDS@ / @STACK@ closures that dominate the @-hi@ heap profile.
--
-- This deliberately contains *no* analysis logic. Capture is the only thing
-- that must run correctly in the field; all analysis happens later, offline,
-- where it can be re-run against the snapshot as many times as needed.
module Main (main) where

import           GHC.Debug.Client (withDebuggeeConnect)
import           GHC.Debug.Snapshot (makeSnapshot)
import           System.Environment (getArgs, lookupEnv)
import           System.Exit (die)
import           System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  (out, mSockArg) <- case args of
    [o]    -> pure (o, Nothing)
    [o, s] -> pure (o, Just s)
    _      -> die usage
  sock <- case mSockArg of
    Just s  -> pure s
    Nothing ->
      lookupEnv "GHC_DEBUG_SOCKET"
        >>= maybe (die "GHC_DEBUG_SOCKET is unset; pass the socket path as the 2nd argument") pure
  hPutStrLn stderr ("ghc-debug: connecting to debuggee socket " <> sock)
  withDebuggeeConnect sock $ \e -> do
    hPutStrLn stderr ("ghc-debug: pausing process, writing snapshot to " <> out)
    makeSnapshot e out
    hPutStrLn stderr "ghc-debug: snapshot written"
  hPutStrLn stderr "ghc-debug: connection closed (process resumed)"
  where
    usage =
      "usage: cardano-ghc-debug-snapshot <output-file> [socket-path]\n\
      \  output-file : path to write the heap snapshot to\n\
      \  socket-path : ghc-debug unix socket (defaults to $GHC_DEBUG_SOCKET)"
