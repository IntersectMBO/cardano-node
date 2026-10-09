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
  , isProcessAlive
  ) where

#ifdef UNIX
import           Control.Monad
import           System.IO.Error (isDoesNotExistError, tryIOError)
import           System.Posix.Signals (Handler (..), installHandler, nullSignal, raiseSignal,
                   sigINT, sigKILL, signalProcess)
import           System.Process (Pid, ProcessHandle, getPid, interruptProcessGroupOf)
#else
import           System.Process (Pid, ProcessHandle, terminateProcess)
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

-- | Whether a process with the given pid exists: 'Just' the answer on unix,
-- where signal 0 checks this without sending anything, and 'Nothing' on
-- Windows. A dead process that nobody has waited for yet (a zombie) still
-- exists, and so does an unrelated process that reuses the pid.
isProcessAlive :: Pid -> IO (Maybe Bool)
#ifdef UNIX
isProcessAlive pid = do
  result <- tryIOError $ signalProcess nullSignal pid
  pure . Just $ case result of
    Left e | isDoesNotExistError e -> False -- no such process
    _ -> True -- signalled, or not allowed to: either way it exists
#else
isProcessAlive _ = pure Nothing
#endif
