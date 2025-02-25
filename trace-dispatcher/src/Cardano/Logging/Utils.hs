{-# LANGUAGE LambdaCase #-}

module Cardano.Logging.Utils
       ( module Cardano.Logging.Utils )
       where

import           Control.Concurrent (threadDelay)
import           Control.Exception (SomeAsyncException (..), fromException, tryJust)
import           Control.Tracer (stdoutTracer, traceWith)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL (toStrict)
import qualified Data.Text.Lazy.Builder as T (toLazyText)
import qualified Data.Text.Lazy.Builder.Int as T

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

  logTrace = traceWith stdoutTracer

  currentDelayInSecs =
    if prevDelayInSecs < 60
      then prevDelayInSecs * 2
      else 60 -- After we reached 60+ secs delay, repeat an attempt every minute.

-- | Convenience function for a Show instance to be converted to text immediately
{-# INLINE showT #-}
showT :: Show a => a -> T.Text
showT = T.pack . show

{-# INLINE showTHex #-}
showTHex :: Integral a => a -> T.Text
showTHex = TL.toStrict . T.toLazyText . T.hexadecimal
