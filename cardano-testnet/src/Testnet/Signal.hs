{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

-- | All the OS-specific signalling logic of cardano-testnet, so that CPP is
-- confined to this module.
module Testnet.Signal
  ( hardKillProcess
  , interruptNodesOnSigINT
  , isProcessAlive
  , pauseProcess
  , resumeProcess
  ) where

#ifdef UNIX
import           Control.Monad
import           Data.Foldable (traverse_)
import           System.IO.Error (illegalOperationErrorType, isDoesNotExistError, mkIOError,
                   tryIOError)
import           System.Posix.Process (ProcessStatus (..), getProcessStatus)
import           System.Posix.Signals (Handler (..), installHandler, nullSignal, raiseSignal,
                   sigCONT, sigINT, sigKILL, sigSTOP, signalProcess)
import           System.Process (Pid, ProcessHandle, getPid, interruptProcessGroupOf)
import           System.Timeout (timeout)
#else
import           System.IO.Error (illegalOperationErrorType, mkIOError)
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
isProcessAlive pid
  | pid <= 0 = pure (Just False) -- 0 and negative pids name process groups, not one process
  | otherwise = do
      result <- tryIOError $ signalProcess nullSignal pid
      pure . Just $ case result of
        Left e | isDoesNotExistError e -> False -- no such process
        _ -> True -- signalled, or not allowed to: either way it exists
#else
isProcessAlive _ = pure Nothing
#endif

-- | Freeze the process with @SIGSTOP@ and wait until the stop has taken effect.
-- Throws an 'IOError' if the process did not stop, and on Windows, which has no
-- equivalent.
pauseProcess :: ProcessHandle -> IO ()
#ifdef UNIX
pauseProcess hProcess = getPid hProcess >>= traverse_ stopAndWait
 where
  stopAndWait pid = do
    signalProcess sigSTOP pid
    status <- timeout 10_000_000 $ getProcessStatus True True pid
    case status of
      Nothing -> failWith "pauseProcess: timed out waiting for the process to stop"
      Just stopped -> unless (maybe False isStopped stopped) $ failWith "pauseProcess: process did not stop"
  failWith message = ioError $ mkIOError illegalOperationErrorType message Nothing Nothing
  isStopped (Stopped _) = True
  isStopped _ = False
#else
pauseProcess _ = ioError $ mkIOError illegalOperationErrorType "pauseProcess: unsupported on Windows" Nothing Nothing
#endif

-- | Continue a process frozen by 'pauseProcess' with @SIGCONT@. Throws an
-- 'IOError' on Windows.
resumeProcess :: ProcessHandle -> IO ()
#ifdef UNIX
resumeProcess hProcess = getPid hProcess >>= mapM_ (signalProcess sigCONT)
#else
resumeProcess _ = ioError $ mkIOError illegalOperationErrorType "resumeProcess: unsupported on Windows" Nothing Nothing
#endif
