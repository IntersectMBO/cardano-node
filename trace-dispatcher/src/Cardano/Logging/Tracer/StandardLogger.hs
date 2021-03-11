{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Tracer.StandardLogger (
    standardMachineTracer
  , standardHumanTracer
) where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Monad (forever)
import           Control.Monad.IO.Class
import qualified Data.Aeson as AE
import           Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           GHC.Conc (ThreadId)
import           Network.HostName (getHostName)

import           Cardano.Logging.DocuGenerator
import           Cardano.Logging.Tracer.Formatting
import           Cardano.Logging.Types

import qualified Control.Tracer as T

-- | Do we log to stdout or to a file?
data LogTarget = LogStdout | LogFile FilePath
  deriving (Eq, Show)

-- | The state of a standard tracer
data StandardTracerState a =  StandardTracerState {
    stRunning :: Maybe (InChan Text, OutChan Text, ThreadId)
  , stTarget  :: LogTarget
}

emptyStandardTracerState :: StandardTracerState a
emptyStandardTracerState = StandardTracerState Nothing LogStdout

standardMachineTracer :: forall a m. (MonadIO m, LogFormatting a)
  => Text
  -> Maybe (DetailLevel -> a -> AE.Object)
  -> m (Trace m a)
standardMachineTracer tracerName mbFormatter = do
    standardTracer tracerName (formatMachine mbFormatter)

standardHumanTracer :: forall a m. (MonadIO m, LogFormatting a)
  => Text
  -> Maybe (a -> Text)
  -> m (Trace m a)
standardHumanTracer tracerName mbFormatter =
  standardTracer tracerName $ \ _ -> formatHuman mbFormatter

standardTracer :: forall a m. (MonadIO m)
  => Text
  -> (Maybe DetailLevel -> a -> Text)
  -> m (Trace m a)
standardTracer tracerName formatter = do
    stateRef <- liftIO $ newIORef emptyStandardTracerState
    hostname <- liftIO getHostName
    pure $ Trace $ T.arrow $ T.emit $ uncurry3 (output stateRef hostname)
  where
    output ::
         IORef (StandardTracerState a)
      -> String
      -> LoggingContext
      -> Maybe TraceControl
      -> a
      -> m ()
    output stateRef _ LoggingContext {} (Just Reset) _a = liftIO $ do
      st <- readIORef stateRef
      case stRunning st of
        Nothing -> initLogging stateRef
        Just _  -> pure ()
    output stateRef hostName lc@LoggingContext {..} Nothing a = liftIO $ do
      st  <- readIORef stateRef
      case stRunning st of
        Just (inChannel, _, _) -> do
          msg <- formatIt
                    (stTarget st == LogStdout)
                    lc
                    hostName
                    (formatter lcDetails a)
          writeChan inChannel msg
        Nothing                -> pure ()
    output _ _ lk (Just c@Document {}) a =
       docIt (StandardBackend tracerName) Machine (lk, Just c, a)
    output _stateRef _ LoggingContext {} _ _a = pure ()

initLogging :: IORef (StandardTracerState a) -> IO ()
initLogging stateRef = do
  (inChan, outChan) <- newChan 2048
  threadId <- forkIO $ forever $ do
    state <- readIORef stateRef
    msg   <- readChan outChan
    case stTarget state of
        LogFile f -> do
                        TIO.appendFile f msg
                        TIO.appendFile f "\n"
        LogStdout -> TIO.putStrLn msg
  modifyIORef stateRef (\ st ->
    st {stRunning = Just (inChan, outChan, threadId)})

-- | Converts a curried function to a function on a triple.
uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f ~(a,b,c) = f a b c
