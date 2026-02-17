{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

module Testnet.Handlers
  ( interruptNodesOnSigINT
  ) where

#ifdef UNIX
import           Control.Monad
import           System.Posix.Signals (Handler (..), installHandler, raiseSignal, sigINT)
import           System.Process (interruptProcessGroupOf)
#endif

import           Testnet.Types

interruptNodesOnSigINT :: [TestnetNode] -> IO ()
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
