{-# LANGUAGE NumericUnderscores #-}

module Cardano.Logging.Utils
       ( module Cardano.Logging.Utils )
       where


import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (concurrently_)
import           Control.Exception (SomeAsyncException (..), SomeException, fromException, tryJust)
import           Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL (toStrict)
import qualified Data.Text.Lazy.Builder as T (toLazyText)
import qualified Data.Text.Lazy.Builder.Int as T
import qualified Data.Text.Lazy.Builder.RealFloat as T (realFloat)
import           GHC.Conc (labelThread, myThreadId)


-- | Run an IO action which may throw an exception in a loop.
--   On exception, the action will be re-run after a pause.
--   That pause doubles which each exception, but is reset when the action runs long enough.
runInLoop :: IO () -> (SomeException -> IO ()) -> Word -> Word -> IO ()
runInLoop action handleInterruption initialDelay maxDelay
  | initialDelay == 0         = runInLoop action handleInterruption 1 maxDelay
  | maxDelay < initialDelay   = runInLoop action handleInterruption initialDelay initialDelay
  | otherwise                 = newIORef (fromIntegral initialDelay) >>= go
  where
    go :: IORef Int -> IO ()
    go currentDelay =
      tryJust excludeAsyncExceptions (actionResettingDelay currentDelay) >>= \case
        Left e -> do
          handleInterruption e
          waitForSecs <- atomicModifyIORef' currentDelay bumpDelay
          threadDelay $ 1_000_000 * waitForSecs
          go currentDelay
        Right _ -> return ()

    -- if the action runs at least maxDelay seconds, the pause is reset
    actionResettingDelay currentDelay = concurrently_ action $ do
      threadDelay $ fromIntegral $ 1_000_000 * maxDelay
      atomicWriteIORef currentDelay $ fromIntegral initialDelay

    excludeAsyncExceptions e =
      case fromException e of
        Just SomeAsyncException{} -> Nothing
        _ -> Just e

    bumpDelay current =
      ( min (current * 2) (fromIntegral maxDelay)
      , current
      )


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
