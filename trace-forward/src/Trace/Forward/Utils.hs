{-# LANGUAGE LambdaCase #-}

module Trace.Forward.Utils
  ( runActionInLoop
  ) where

import           Control.Exception (SomeAsyncException (..), fromException, tryJust)
import           Control.Tracer (showTracing, stdoutTracer, traceWith)
import           System.Time.Extra (sleep)

import           Trace.Forward.Configuration (HowToConnect)

-- | Run monadic action in a loop. If there's an exception, it will re-run
--   the action again, after pause that grows.
runActionInLoop
  :: IO ()
  -> HowToConnect
  -> Word
  -> IO ()
runActionInLoop action endpoint prevDelay =
  tryJust excludeAsyncExceptions action >>= \case
    Left e -> do
      logTrace $ "trace-forward, connection with " <> show endpoint <> " failed: " <> show e
      sleep $ fromIntegral currentDelay
      runActionInLoop action endpoint currentDelay
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
