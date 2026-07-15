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
  ) where

#ifdef UNIX
import           Control.Monad
import           System.Posix.Signals (Handler (..), installHandler, raiseSignal, sigINT,
                   sigKILL, signalProcess)
import           System.Process (ProcessHandle, getPid, interruptProcessGroupOf)
#else
import           System.Process (ProcessHandle, terminateProcess)
#endif

import           Data.List.NonEmpty (NonEmpty)

import           Testnet.Types

interruptNodesOnSigINT :: NonEmpty TestnetNode -> IO ()
#ifdef UNIX
interruptNodesOnSigINT testnetNodes =
  -- Interrupt cardano nodes when the main process is interrupted
  void $ flip (installHandler sigINT) Nothing $ CatchOnce $ do
    forM_ testnetNodes $ \TestnetNode{nodeProcessHandle} ->
      interruptProcessGroupOf nodeProcessHandle
    raiseSignal sigINT
#else
interruptNodesOnSigINT _testnetNodes = pure ()
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
