{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Tracer.Standard (
    standardTracer
) where

import           Cardano.Logging.DocuGenerator
import           Cardano.Logging.Types
import           Cardano.Logging.Utils (threadLabelMe)

import           Control.Concurrent.Async
import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Exception (BlockedIndefinitelyOnMVar (..), handle)
import           Control.Monad (forever, when)
import           Control.Monad.IO.Class
import qualified Control.Tracer as T
import           Data.IORef
import           Data.Maybe (isNothing)
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           System.IO (hFlush, stdout)

-- | The state of a standard tracer
newtype StandardTracerState =  StandardTracerState {
    stRunning :: Maybe (InChan Text, OutChan Text, Async ())
}

emptyStandardTracerState :: StandardTracerState
emptyStandardTracerState = StandardTracerState Nothing

-- | The standardTracer handles stdout logging in a thread-safe manner.
--   It is strongly advised to construct only one standardTracer for any application.
standardTracer :: forall m. (MonadIO m)
  => m (Trace m FormattedMessage)
standardTracer = do
    stateRef <- liftIO $ newIORef emptyStandardTracerState
    pure $ Trace $ T.arrow $ T.emit $ uncurry (output stateRef)
  where
    output ::
         IORef StandardTracerState
      -> LoggingContext
      -> Either TraceControl FormattedMessage
      -> m ()
    output stateRef LoggingContext{} (Right (FormattedHuman _c msg)) = liftIO $ do
      st  <- readIORef stateRef
      case stRunning st of
        Just (inChannel, _, _) -> writeChan inChannel msg
        Nothing                -> pure ()
    output stateRef LoggingContext{} (Right (FormattedMachine msg)) = liftIO $ do
      st  <- readIORef stateRef
      case stRunning st of
        Just (inChannel, _, _) -> writeChan inChannel msg
        Nothing                -> pure ()
    output stateRef LoggingContext{} (Left TCReset) = liftIO $ do
      st <- readIORef stateRef
      case stRunning st of
        Nothing -> when (isNothing $ stRunning st) $
                      startStdoutThread stateRef
        Just _  -> pure ()
    output _ lk c@(Left TCDocument {}) =
       docIt
        (Stdout MachineFormat) -- TODO Find out the right format
        (lk, c)
    output _stateRef LoggingContext {} _ = pure ()

-- | Forks a new thread, which writes messages to stdout
startStdoutThread :: IORef StandardTracerState -> IO ()
startStdoutThread stateRef = do
    (inChan, outChan) <- newChan 2048
    as <- async $ threadLabelMe "StdoutTrace" >> stdoutThread outChan
    link as
    atomicWriteIORef stateRef $ StandardTracerState (Just (inChan, outChan, as))

-- | The new thread, which does the actual write from the queue.
--   Will safely terminate when all producers have gone out of scope.
stdoutThread :: OutChan Text -> IO ()
stdoutThread outChan =
  handle (\BlockedIndefinitelyOnMVar -> pure ()) $
    forever $ do
      readChan outChan
        >>= TIO.putStrLn
      hFlush stdout
