{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This top-level module is used by 'cardano-tracer' app.
module Cardano.Tracer.Utils
  ( concurrently2
  , concurrently3
  , runInLoop
  , showProblemIfAny
  ) where

import           Control.Concurrent.Async (concurrently_, withAsync, wait)
import           Control.Exception (IOException, SomeAsyncException (..),
                                    fromException, try, tryJust)
import           Control.Monad (void)
import "contra-tracer" Control.Tracer (showTracing, stdoutTracer, traceWith)
import           System.Time.Extra (sleep)

concurrently2 :: IO () -> IO () -> IO ()
concurrently2 = concurrently_

concurrently3 :: IO () -> IO () -> IO () -> IO ()
concurrently3 action1 action2 action3 =
  withAsync action1 $ \a1 ->
    withAsync action2 $ \a2 ->
      withAsync action3 $ \a3 -> do
        void $ wait a1
        void $ wait a2
        void $ wait a3

-- | Run monadic action in a loop. If there's an exception, it will re-run
--   the action again, after pause that grows.
runInLoop
  :: IO ()
  -> FilePath
  -> Word
  -> IO ()
runInLoop action localSocket prevDelay =
  tryJust excludeAsyncExceptions action >>= \case
    Left e -> do
      logTrace $ "cardano-tracer, connection with " <> show localSocket <> " failed: " <> show e
      sleep $ fromIntegral currentDelay
      runInLoop action localSocket currentDelay
    Right _ -> return ()
 where
  excludeAsyncExceptions e =
    case fromException e of
      Just SomeAsyncException {} -> Nothing
      _ -> Just e

  logTrace = traceWith $ showTracing stdoutTracer

  currentDelay =
    if prevDelay < 60
      then prevDelay * 2
      else 60 -- After we reached 60+ secs delay, repeat an attempt every minute.

showProblemIfAny :: IO () -> IO ()
showProblemIfAny action =
  try action >>= \case
    Left (e :: IOException) -> logTrace $ "cardano-tracer, cannot write trace objects: " <> show e
    Right _ -> return ()
 where
  logTrace = traceWith $ showTracing stdoutTracer
