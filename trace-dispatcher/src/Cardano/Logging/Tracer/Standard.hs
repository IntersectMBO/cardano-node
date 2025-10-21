{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Cardano.Logging.Tracer.Standard (
    standardTracer
) where

import           Cardano.Logging.DocuGenerator
import           Cardano.Logging.Types
import           Cardano.Logging.Utils (threadLabelMe)

import           Control.Concurrent.Async
import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Monad (forever, when)
import           Control.Monad.IO.Class
import qualified Control.Tracer as T
import           Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import           Data.Maybe (isNothing)
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           System.IO (hFlush, stdout)

-- | The state of a standard tracer
newtype StandardTracerState =  StandardTracerState {
    _stRunning :: Maybe (InChan Text, OutChan Text, Async ())
}

emptyStandardTracerState :: StandardTracerState
emptyStandardTracerState = StandardTracerState Nothing

-- | It is mandatory to construct only one standard tracer in any application!
-- Throwing away a standard tracer and using a new one will result in an exception
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
    output _stateRef LoggingContext {} (Right (FormattedHuman _c msg)) = liftIO $ do
      writeStdout msg
      {-
      st  <- readIORef stateRef
      case stRunning st of
        Just (inChannel, _, _) -> writeChan inChannel msg
        Nothing                -> pure ()
      -}
    output _stateRef LoggingContext {} (Right (FormattedMachine msg)) = liftIO $ do
      writeStdout msg
      {-
      st  <- readIORef stateRef
      case stRunning st of
        Just (inChannel, _, _) -> writeChan inChannel msg
        Nothing                -> pure ()
      -}
    output _stateRef LoggingContext {} (Left TCReset) = liftIO $ do
      pure ()
      {-
      st <- readIORef stateRef
      case stRunning st of
        Nothing -> when (isNothing $ stRunning st) $
                      startStdoutThread stateRef
        Just _  -> pure ()
        -}
    output _ lk c@(Left TCDocument {}) =
       docIt
        (Stdout MachineFormat) -- TODO Find out the right format
        (lk, c)
    output _stateRef LoggingContext {} _ = pure ()

writeStdout :: Text -> IO ()
writeStdout t = TIO.putStrLn t >> hFlush stdout

{-
-- | Forks a new thread, which writes messages to stdout
startStdoutThread :: IORef StandardTracerState -> IO ()
startStdoutThread stateRef = do
    (inChan, outChan) <- newChan 2048
    as <- async $ threadLabelMe "StdoutTrace" >> stdoutThread outChan
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
-}