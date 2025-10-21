{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Tracer.Standard (
    standardTracer
) where

import qualified Debug.Trace as DT
import           Cardano.Logging.DocuGenerator
import           Cardano.Logging.Types
-- import           Cardano.Logging.Utils (threadLabelMe)

-- import           Control.Concurrent.Async
import           Control.Concurrent.Chan.Unagi.Bounded
-- import           Control.Monad (forever)
import           Control.Monad.IO.Class
import qualified Control.Tracer as T
-- import           Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
-- import           Data.Maybe (isNothing)
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
-- import           System.IO (hFlush, stdout)

-- | The state of a standard tracer
-- newtype StandardTracerState =  StandardTracerState {
--     stRunning :: Maybe (InChan Text, OutChan Text, Async ())
-- }

-- emptyStandardTracerState :: StandardTracerState
-- emptyStandardTracerState = StandardTracerState Nothing

-- | It is mandatory to construct only one standard tracer in any application!
-- Throwing away a standard tracer and using a new one will result in an exception
standardTracer :: forall m. (MonadIO m)
  => m (Trace m FormattedMessage)
standardTracer = do
    -- no need for ioref's
    (inChan, _outChan) <- liftIO $ newChan 2048
    -- as <- liftIO . async $ threadLabelMe "StdoutTrace" >> stdoutThread outChan
    -- liftIO $ link as
    pure $ Trace $ T.arrow $ T.emit $ uncurry (output inChan)
  where
    output ::
         InChan Text
      -> LoggingContext
      -> Either TraceControl FormattedMessage
      -> m ()
    output _channel LoggingContext {} (Right (FormattedHuman _c msg)) = liftIO $ do
      -- DT.traceIO $ "output human: " <> unpack msg
      -- writeChan channel msg
      TIO.putStrLn msg
    output _channel LoggingContext {} (Right (FormattedMachine msg)) = liftIO $ do
      -- DT.traceIO $ "output machineformat: " <> unpack msg
      -- writeChan channel msg
      TIO.putStrLn msg
    output _channel LoggingContext {} (Left TCReset) = liftIO $ do
      DT.traceIO "output: tcreset"
      pure ()
    output _ lk c@(Left TCDocument {}) = liftIO (DT.traceIO "output: tcdocument") >>
       docIt
        (Stdout MachineFormat) -- TODO Find out the right format
        (lk, c)
    output _stateRef LoggingContext {} _ = liftIO (DT.traceIO "output: _") >> pure ()

-- | Forks a new thread, which writes messages to stdout
-- startStdoutThread :: IORef StandardTracerState -> IO ()
-- startStdoutThread stateRef = do
--     (inChan, outChan) <- newChan 2048
--     as <- async $ threadLabelMe "StdoutTrace" >> stdoutThread outChan
--     link as
--     atomicModifyIORef' stateRef (\ st ->
--       (st {stRunning = Just (inChan, outChan, as)}, ()))

-- | The new thread, which does the actual write from the queue.
-- runs forever, and never returns
-- stdoutThread :: OutChan Text -> IO ()
-- stdoutThread outChan = forever $ do
--     readChan outChan
--       >>= TIO.putStrLn
--     hFlush stdout
