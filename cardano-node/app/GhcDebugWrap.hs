{-# LANGUAGE CPP #-}

-- | ghc-debug integration shim.
--
-- Isolated in its own module on purpose: it needs CPP, and the C preprocessor
-- treats a trailing backslash as a line-continuation, which would mangle the
-- Haskell string gaps in @Main@ (app/cardano-node.hs). Keeping CPP out of that
-- file avoids the breakage; this module has no string literals to mangle.
module GhcDebugWrap (withGhcDebugIfEnabled) where

#ifdef GHC_DEBUG
import           GHC.Debug.Stub (withGhcDebug)
import           System.Environment (lookupEnv)
#endif

-- | When built with the @ghc-debug@ cabal flag, serve the ghc-debug protocol so
-- the heap can be snapshotted -- but only when @GHC_DEBUG_SOCKET@ is set. The
-- stub is always linked in ghc-debug builds; gating on the env var at runtime
-- means a node with no socket configured behaves exactly like an uninstrumented
-- one (no surprise socket created under the XDG data dir). Without the flag
-- this is the identity.
withGhcDebugIfEnabled :: IO a -> IO a
#ifdef GHC_DEBUG
withGhcDebugIfEnabled act = do
  mSock <- lookupEnv "GHC_DEBUG_SOCKET"
  case mSock of
    Just _  -> withGhcDebug act
    Nothing -> act
#else
withGhcDebugIfEnabled = id
#endif
