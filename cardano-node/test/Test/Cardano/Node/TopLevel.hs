{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Node.TopLevel
  ( tests
  ) where

import           Cardano.Node.Handlers.TopLevel

import           Control.Concurrent (myThreadId)
import           Control.Exception
import           System.Exit

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

tests :: IO Bool
tests =
  Hedgehog.checkParallel $$discover
