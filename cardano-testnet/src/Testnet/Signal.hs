{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

-- | All the OS-specific signalling logic of cardano-testnet, so that CPP is
-- confined to this module.
module Testnet.Signal
  ( hardKillProcess
  , interruptNodesOnSigINT
  , reloadNodeConfiguration
  ) where

#ifdef UNIX
import           Control.Monad
import           System.Posix.Signals (Handler (..), installHandler, raiseSignal, sigHUP,
                   sigINT, sigKILL, signalProcess)
import           System.Process (ProcessHandle, getPid, interruptProcessGroupOf)
#else
import           System.Process (ProcessHandle, terminateProcess)
#endif

import           Data.List.NonEmpty (NonEmpty)

import           Testnet.Types

interruptNodesOnSigINT :: [ProcessHandle] -> NonEmpty TestnetNode -> IO ()
#ifdef UNIX
interruptNodesOnSigINT extraProcesses testnetNodes =
  -- Interrupt cardano nodes (and any extra processes, e.g. cardano-tracer)
  -- when the main process is interrupted
  void $ flip (installHandler sigINT) Nothing $ CatchOnce $ do
    forM_ testnetNodes $ \TestnetNode{nodeProcessHandle} ->
      interruptProcessGroupOf nodeProcessHandle
    forM_ extraProcesses interruptProcessGroupOf
    raiseSignal sigINT
#else
interruptNodesOnSigINT _extraProcesses _testnetNodes = pure ()
#endif

-- | Send an unignorable kill to the process: @SIGKILL@ on unix, which cannot be
-- ignored or blocked, even by a stopped process. On Windows 'terminateProcess'
-- is already a hard TerminateProcess() call that cannot be refused, so it is
-- used directly.
hardKillProcess :: ProcessHandle -> IO ()
#ifdef UNIX
hardKillProcess hProcess = getPid hProcess >>= mapM_ (signalProcess sigKILL)
#else
hardKillProcess = terminateProcess
#endif

-- | Ask a running node to re-read its configuration: @SIGHUP@ on unix, which
-- cardano-node answers by reloading the topology, the RPC configuration and
-- the block forging credentials. Windows has no equivalent, so the call fails
-- there; tests that need it are ignored on Windows.
reloadNodeConfiguration :: ProcessHandle -> IO ()
#ifdef UNIX
reloadNodeConfiguration hProcess = getPid hProcess >>= mapM_ (signalProcess sigHUP)
#else
reloadNodeConfiguration _ =
  ioError (userError "reloadNodeConfiguration: SIGHUP is not available on Windows")
#endif
