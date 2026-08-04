{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

module Test.Cardano.Node.TopLevel
  ( tests
  ) where

import           Cardano.Node.Handlers.TopLevel

import           Control.Concurrent (myThreadId)
#ifdef UNIX
import qualified Control.Concurrent as Concurrent
#endif
import           Control.Exception
import           System.Exit
#ifdef UNIX
import qualified System.Posix.Signals as Signals
#endif

import           Hedgehog (Property, discover, (===))
import qualified Hedgehog
import           Hedgehog.Internal.Property (failWith)

prop_sigTermExceptionIsAsync :: Property
prop_sigTermExceptionIsAsync =
  Hedgehog.property $
    case fromException @SomeAsyncException $ toException SigTermException of
      Just{} -> Hedgehog.success
      Nothing ->
        failWith Nothing "SigTermException should be a SomeAsyncException"

prop_topLevelSigTermExitsSuccessfully :: Property
prop_topLevelSigTermExitsSuccessfully =
  Hedgehog.property $ do
    result <-
      Hedgehog.evalIO $
        try @ExitCode $
          toplevelExceptionHandler $
            throwIO SigTermException
    result === (Left ExitSuccess :: Either ExitCode ())

prop_sigTermDuringStartupIsAsync :: Property
prop_sigTermDuringStartupIsAsync =
  Hedgehog.property $ do
    result <-
      Hedgehog.evalIO $
        try @SomeAsyncException $
          myThreadId >>= throwSigTerm SigTermDuringStartup
    case result of
      Left{} -> Hedgehog.success
      Right{} ->
        failWith Nothing "startup SIGTERM should throw an async exception"

prop_sigTermDuringRuntimeExitsSuccessfully :: Property
prop_sigTermDuringRuntimeExitsSuccessfully =
  Hedgehog.property $ do
    result <-
      Hedgehog.evalIO $
        try @ExitCode $
          myThreadId >>= throwSigTerm SigTermDuringRuntime
    result === (Left ExitSuccess :: Either ExitCode ())

prop_installedSigTermHandlerExitsSuccessfully :: Property
#ifdef UNIX
prop_installedSigTermHandlerExitsSuccessfully =
  Hedgehog.property $ do
    result <-
      Hedgehog.evalIO $
        try @ExitCode $
          toplevelExceptionHandler $ do
            installSigTermHandler SigTermDuringStartup
            Signals.raiseSignal Signals.sigTERM
            Concurrent.threadDelay 1000000
    result === (Left ExitSuccess :: Either ExitCode ())
#else
prop_installedSigTermHandlerExitsSuccessfully =
  Hedgehog.property Hedgehog.success
#endif

tests :: IO Bool
tests =
  Hedgehog.checkParallel $$discover
