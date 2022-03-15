{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Tracer.Standard (
    standardTracer
) where

import           Control.Concurrent.Async
import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Exception (catch, BlockedIndefinitelyOnMVar)
import           Control.Monad (forever, when)
import           Control.Monad.IO.Class
import           Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import           Data.Maybe (isNothing)
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           System.IO (hFlush, stdout)

import           Cardano.Logging.DocuGenerator
import           Cardano.Logging.Types
import           Cardano.Logging.Utils (uncurry3)

import qualified Control.Tracer as T

-- | The state of a standard tracer
newtype StandardTracerState =  StandardTracerState {
    stRunning :: Maybe (InChan Text, OutChan Text, Async ())
}

emptyStandardTracerState :: StandardTracerState
emptyStandardTracerState = StandardTracerState Nothing

standardTracer :: forall m. (MonadIO m)
  => m (Trace m FormattedMessage)
standardTracer = do
    stateRef <- liftIO $ newIORef emptyStandardTracerState
    pure $ Trace $ T.arrow $ T.emit $ uncurry3 (output stateRef)
  where
    output ::
         IORef StandardTracerState
      -> LoggingContext
      -> Maybe TraceControl
      -> FormattedMessage
      -> m ()
    output stateRef LoggingContext {} Nothing (FormattedHuman _c msg) = liftIO $ do
      st  <- readIORef stateRef
      case stRunning st of
        Just (inChannel, _, _) -> writeChan inChannel msg
        Nothing                -> pure ()
    output stateRef LoggingContext {} Nothing (FormattedMachine msg) = liftIO $ do
      st  <- readIORef stateRef
      case stRunning st of
        Just (inChannel, _, _) -> writeChan inChannel msg
        Nothing                -> pure ()
    output stateRef LoggingContext {} (Just Reset) _msg = liftIO $ do
      st <- readIORef stateRef
      case stRunning st of
        Nothing -> when (isNothing $ stRunning st) $
                      startStdoutThread stateRef
        Just _  -> pure ()
    output _ lk (Just c@Document {}) (FormattedHuman co msg) =
       docIt
        (Stdout (if co then HumanFormatColoured else HumanFormatUncoloured))
        (FormattedHuman co "")
        (lk, Just c, msg)
    output _ lk (Just c@Document {}) (FormattedMachine msg) =
       docIt (Stdout MachineFormat) (FormattedMachine "") (lk, Just c, msg)
    output _stateRef LoggingContext {} _ _a = pure ()

-- | Forks a new thread, which writes messages to stdout
startStdoutThread :: IORef StandardTracerState -> IO ()
startStdoutThread stateRef = do
    (inChan, outChan) <- newChan 2048
    as <- async (catch
      (stdoutThread outChan)
      (\(_ :: BlockedIndefinitelyOnMVar) -> pure ()))
    link as
    modifyIORef' stateRef (\ st ->
      st {stRunning = Just (inChan, outChan, as)})

-- | The new thread, which does the actual write from the queue.
-- runs forever, and never returns
stdoutThread :: OutChan Text -> IO ()
stdoutThread outChan = forever $ do
    readChan outChan
      >>= TIO.putStrLn
    hFlush stdout
