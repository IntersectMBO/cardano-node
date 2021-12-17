{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}

module Cardano.Logging.Utils (
  runInLoop,
  uncurry3
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Exception (SomeAsyncException (..), fromException, tryJust)
import "contra-tracer" Control.Tracer (showTracing, stdoutTracer, traceWith)

-- | Run monadic action in a loop. If there's an exception, it will re-run
--   the action again, after pause that grows.
runInLoop :: IO () -> FilePath -> Word -> IO ()
runInLoop action localSocket prevDelayInSecs =
  tryJust excludeAsyncExceptions action >>= \case
    Left e -> do
      logTrace $ "connection with " <> show localSocket <> " failed: " <> show e
      threadDelay . fromIntegral $ currentDelayInSecs * 1000000
      runInLoop action localSocket currentDelayInSecs
    Right _ -> return ()
 where
  excludeAsyncExceptions e =
    case fromException e of
      Just SomeAsyncException {} -> Nothing
      _ -> Just e

  logTrace = traceWith $ showTracing stdoutTracer

  currentDelayInSecs =
    if prevDelayInSecs < 60
      then prevDelayInSecs * 2
      else 60 -- After we reached 60+ secs delay, repeat an attempt every minute.

-- | Converts a curried function to a function on a triple.
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a,b,c) = f a b c
