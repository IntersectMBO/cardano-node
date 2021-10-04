{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Tracer.Standard (
    standardTracer
) where

import           Control.Concurrent.Async
import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Monad (forever)
import           Control.Monad.IO.Class
import           Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Data.Void (Void)
import           System.IO (hFlush, stdout)

import           Cardano.Logging.DocuGenerator
import           Cardano.Logging.Types
import           Cardano.Logging.Utils (uncurry3)

import qualified Control.Tracer as T

-- | Do we log to stdout or to a file?
data LogTarget = LogStdout | LogFile FilePath
  deriving (Eq, Show)

-- | The state of a standard tracer
data StandardTracerState a =  StandardTracerState {
    stRunning :: Maybe (InChan Text, OutChan Text, Async Void)
  , stTarget  :: LogTarget
}

emptyStandardTracerState :: Maybe FilePath -> StandardTracerState a
emptyStandardTracerState Nothing   = StandardTracerState Nothing LogStdout
emptyStandardTracerState (Just fp) = StandardTracerState Nothing (LogFile fp)


standardTracer :: forall m. (MonadIO m)
  => Maybe FilePath
  -> m (Trace m FormattedMessage)
standardTracer mbFilePath = do
    stateRef <- liftIO $ newIORef (emptyStandardTracerState mbFilePath)
    pure $ Trace $ T.arrow $ T.emit $ uncurry3 (output stateRef)
  where
    output ::
         IORef (StandardTracerState a)
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
        Nothing -> case stRunning st of
                      Just (inChannel, _, _) -> pure ()
                      Nothing                -> startWriterThread stateRef
        Just _  -> pure ()
    output _ lk (Just c@Document {}) (FormattedHuman co msg) =
       docIt
        (Stdout (if co then HumanFormatColoured else HumanFormatUncoloured))
        (FormattedHuman co "")
        (lk, Just c, msg)
    output _ lk (Just c@Document {}) (FormattedMachine msg) =
       docIt (Stdout MachineFormat) (FormattedMachine "") (lk, Just c, msg)
    output _stateRef LoggingContext {} _ _a = pure ()

-- | Forks a new thread, which writes the messages either to stdout or a file
startWriterThread :: IORef (StandardTracerState a) -> IO ()
startWriterThread stateRef = do
    (inChan, outChan) <- newChan 2048
    as <- async (writerThread stateRef outChan)
    modifyIORef stateRef (\ st ->
      st {stRunning = Just (inChan, outChan, as)})


-- | The new thread, which does the actual write from the queue.
-- runs forever, and never returns
writerThread ::
     IORef (StandardTracerState a)
  -> OutChan Text
  -> IO Void
writerThread stateRef outChan = forever $ do
    msg   <- readChan outChan
    state <- readIORef stateRef
    case stTarget state of
        LogFile f -> do
                        TIO.appendFile f msg
                        TIO.appendFile f "\n"
        LogStdout -> do
                        TIO.putStrLn msg
                        hFlush stdout
