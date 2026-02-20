{-# LANGUAGE CPP #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

module Testnet.Handlers
  ( interruptNodesOnSigINT
  ) where

#ifdef UNIX
import           Control.Monad
import           System.Posix.Signals (Handler (..), installHandler, raiseSignal, sigINT)
import           System.Process (interruptProcessGroupOf, ProcessHandle)
#endif

#ifdef UNIX
interruptNodesOnSigINT :: [ProcessHandle] -> IO ()
interruptNodesOnSigINT processHandlers =
  -- Interrupt cardano nodes when the main process is interrupted
  void $ flip (installHandler sigINT) Nothing $ CatchOnce $ do
    forM_ processHandlers $ \nodeProcessHandle ->
      interruptProcessGroupOf nodeProcessHandle
    raiseSignal sigINT
#else
interruptNodesOnSigINT :: processHandle -> IO ()
interruptNodesOnSigINT _testnetNodes = pure ()
#endif
