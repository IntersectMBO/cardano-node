{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}

module Test.TxCentrifuge.SupervisionTest (tests) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async qualified as Async
import Control.Exception (AsyncException (ThreadKilled), Exception, fromException, throwIO)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (isJust)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import Cardano.Benchmarking.TxCentrifuge.Supervision (superviseTarget)

tests :: TestTree
tests = testGroup "target supervision"
  [ testCase "Async.cancel terminates a connected target" $
      checkCancellation Async.cancel Async.AsyncCancelled
  , testCase "ThreadKilled terminates a connected target" $
      checkCancellation (`Async.cancelWith` ThreadKilled) ThreadKilled
  , testCase "synchronous connection failures still reconnect" $
      checkRetry (ioError (userError "connection failed"))
  , testCase "normal connection termination still reconnects" $
      checkRetry (pure ())
  ]

checkCancellation :: Exception e => (Async.Async () -> IO ()) -> e -> IO ()
checkCancellation cancel exception = do
  started <- newEmptyMVar
  blocked <- newEmptyMVar
  worker <- Async.async $ superviseTarget "cancellation-test" $
    putMVar started () >> takeMVar blocked
  ready <- timeout 2_000_000 (takeMVar started)
  ready @?= Just ()
  -- Wait for cancellation in a separate async so a regression cannot hang
  -- the test runner inside cancelWith's wait for the target to terminate.
  canceller <- Async.async $ cancel worker
  completed <- timeout 2_000_000 (Async.wait canceller)
  completed @?= Just ()
  result <- Async.waitCatch worker
  case result of
    Left ex -> assertBool "original cancellation exception is propagated"
      (isJust (fromException ex `asTypeOf` Just exception))
    Right () -> assertFailure "supervisor returned normally"

checkRetry :: IO () -> IO ()
checkRetry firstAttempt = do
  attempts <- newIORef (0 :: Int)
  worker <- Async.async $ superviseTarget "retry-test" $ do
    attempt <- atomicModifyIORef' attempts (\n -> (n + 1, n))
    if attempt == 0 then firstAttempt else throwIO Async.AsyncCancelled
  result <- timeout 3_000_000 (Async.waitCatch worker)
  case result of
    Just (Left ex) -> assertBool "retry propagates cancellation"
      (isJust (fromException ex :: Maybe Async.AsyncCancelled))
    _ -> assertFailure "supervisor did not reconnect and propagate cancellation"
  readIORef attempts >>= (@?= 2)
