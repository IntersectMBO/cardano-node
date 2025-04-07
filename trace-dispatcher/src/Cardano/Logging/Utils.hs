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
import qualified Data.Text.Lazy.Builder.RealFloat as T (realFloat)
import           GHC.Conc (labelThread, myThreadId)


-- | Run monadic action in a loop. If there's an exception, it will re-run
--   the action again, after pause that grows.
runInLoop :: IO () -> FilePath -> Word -> Word -> IO ()
runInLoop action localSocket prevDelayInSecs maxReconnectDelay =
  tryJust excludeAsyncExceptions action >>= \case
    Left e -> do
      logTrace $ "connection with " <> show localSocket <> " failed: " <> show e
      threadDelay . fromIntegral $ currentDelayInSecs * 1000000
      runInLoop action localSocket currentDelayInSecs maxReconnectDelay
    Right _ -> return ()
 where
  excludeAsyncExceptions e =
    case fromException e of
      Just SomeAsyncException {} -> Nothing
      _ -> Just e

  logTrace = traceWith stdoutTracer

  currentDelayInSecs =
    min (prevDelayInSecs * 2) maxReconnectDelay

-- | Convenience function for a Show instance to be converted to text immediately
{-# INLINE showT #-}
showT :: Show a => a -> T.Text
showT = T.pack . show

{-# INLINE showTHex #-}
showTHex :: Integral a => a -> T.Text
showTHex = TL.toStrict . T.toLazyText . T.hexadecimal

{-# INLINE showTReal #-}
showTReal :: RealFloat a => a -> T.Text
showTReal = TL.toStrict . T.toLazyText . T.realFloat

threadLabelMe :: String -> IO ()
threadLabelMe label = myThreadId >>= flip labelThread label
