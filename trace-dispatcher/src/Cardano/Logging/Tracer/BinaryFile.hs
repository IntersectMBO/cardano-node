{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Tracer.BinaryFile
      {-# WARNING "The binary file tracer is experimental; it's currently not viable for production use." #-}
      (binaryFileTracer) 
      where

import           Cardano.Logging.DocuGenerator
import           Cardano.Logging.Types
import           Cardano.Logging.Utils (threadLabelMe)

import           Codec.CBOR.Encoding (encodeBreak, encodeListLenIndef)
import           Codec.CBOR.Write (toStrictByteString)
import           Control.Concurrent.Async
import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Exception (BlockedIndefinitelyOnMVar (..), finally, handle)
import           Control.Monad -- (forever, forM_, when)
import           Control.Monad.IO.Class
import qualified Control.Tracer as T
import           Data.ByteString as B (ByteString, hPut)
import           Data.IORef
import           Data.Maybe (isNothing)
import           System.IO -- (IOMode (..), hFlush, withBinaryFile)

-- | The state of a binlog tracer
newtype BinaryFileTracerState = BinaryFileTracerState {
    stRunning :: Maybe (InChan ByteString, OutChan ByteString, Async ())
}

emptyBinLogTracerState :: BinaryFileTracerState
emptyBinLogTracerState = BinaryFileTracerState Nothing

-- | The binaryFileTracer handles file logging binary/CBOR data in a thread-safe manner.
--   It is strongly advised to construct only one binaryFileTracer for any application.
binaryFileTracer :: forall m. (MonadIO m)
  => FilePath
  -> m (Trace m FormattedMessage, m ())
binaryFileTracer fileName = do
    stateRef <- liftIO $ newIORef emptyBinLogTracerState
    let
      closeFile = do
        _as <- atomicModifyIORef' stateRef $ \case
          BinaryFileTracerState (Just (_, _, as)) -> (BinaryFileTracerState Nothing, Just as)
          _                                       -> (BinaryFileTracerState Nothing, Nothing)
        -- forM_ as cancel
        pure ()

    pure ( Trace $ T.arrow $ T.emit $ uncurry (output stateRef)
         , liftIO closeFile
         )
  where
    output ::
         IORef BinaryFileTracerState
      -> LoggingContext
      -> Either TraceControl FormattedMessage
      -> m ()
    output stateRef LoggingContext{} (Right (FormattedCBOR msg)) = liftIO $ do
      st  <- readIORef stateRef
      case stRunning st of
        Just (inChannel, _, _) -> writeChan inChannel msg
        Nothing                -> pure ()
    output stateRef LoggingContext{} (Left TCReset) = liftIO $ do
      st <- readIORef stateRef
      case stRunning st of
        Nothing -> when (isNothing $ stRunning st) $
                      startFileThread fileName stateRef
        Just _  -> pure ()
    output _ lk c@(Left TCDocument {}) =
       docIt
        (Stdout MachineFormat)
        (lk, c)
    output _stateRef LoggingContext {} _ = pure ()

-- | Forks a new thread, which writes CBOR to a binary file
startFileThread :: FilePath -> IORef BinaryFileTracerState -> IO ()
startFileThread fileName stateRef = do
    (inChan, outChan) <- newChan 2048
    as <- async $ threadLabelMe "BinLogWriter" >> fileThread fileName outChan
    link as
    atomicWriteIORef stateRef $ BinaryFileTracerState (Just (inChan, outChan, as))

-- | The new thread, which does the actual write from the queue.
--   Will safely terminate when all producers have gone out of scope.
fileThread :: FilePath -> OutChan ByteString -> IO ()
fileThread fileName outChan =
  handle (\BlockedIndefinitelyOnMVar -> putStrLn "ending fileThread" >> pure ()) $
    withBinaryFile fileName WriteMode $ \fileHandle ->
      let putAndFlush bs = B.hPut fileHandle bs >> hFlush fileHandle
      in do
        hSetBuffering fileHandle NoBuffering
        putAndFlush opener
        forever $ do
          readChan outChan
            >>= putAndFlush
        `finally`
          putAndFlush terminator
  where
    opener     = toStrictByteString encodeListLenIndef
    terminator = toStrictByteString encodeBreak
