{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}


module Cardano.Trace.Ingest
       ( Ingestor
       , IngestMode(..)
       , FailureMode(..)
       , IngestorReader(readLineIngestor)
       , mkIngestor
       , mkIngestorReader
       , ingestFileThreaded
       ) where

import           Control.Concurrent
import           Control.Concurrent.Chan.Unagi as Unagi
import           Control.Concurrent.STM.TVar
import           Control.Exception
import           Control.Monad                 (forM_, forever, unless, void,
                                                when)
import           Control.Monad.STM             (atomically)
import           Data.Aeson                    (FromJSON (..), decodeStrict',
                                                withObject, (.:))
import           Data.Bits                     (shiftL, xor)
import           Data.ByteString.Char8         as ByteString (ByteString,
                                                              hGetLine)
import           Data.Char                     (ord)
import           Data.List                     (foldl')
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (isJust)
import           Data.Time.Clock               (NominalDiffTime, UTCTime)
import           Data.Time.Clock.POSIX
import           Data.Word                     (Word64)
import           System.Directory              (doesFileExist)
import           System.IO
import           System.IO.Error               (isEOFError)


-- NOTES:
-- 1. The ingested files need to be produced using line buffering.
--    The ingestor logic currently does not support reading / incrementally parsing
--    partial trace messages.


data Ingestor = Ingestor
  { ingRetentionMs :: !Int
  , ingChanRef     :: !(InChan ByteString)
  , ingInBuffer    :: !(TVar (Map.Map LineKey ByteString))
  }

-- In the unlikely case there's a collision of timestamps coming from
-- different ingested files, we tag it with some hash of its file path
type LineKey = (UTCTime, Word64)

data IngestMode =
    FromFileStart
  | FromFileEnd
  deriving (Show, Eq)

data FailureMode =
    DieSilently
  | RethrowExceptions
  deriving (Show, Eq)

-- a blocking reader
newtype IngestorReader = IngestorReader { readLineIngestor :: IO ByteString }

newtype Timestamp = Timestamp UTCTime

instance FromJSON Timestamp where
  parseJSON = withObject "Timestamp" $ \o ->
    Timestamp <$> o .: "at"


-- The interval in ms for which ingested lines will remain
-- buffered, before being piped to the queue for consumption.
-- Any consumer will have that as "lag behind" to real-time.
mkIngestor :: Int -> IO Ingestor
mkIngestor (max 100 -> millisecs) = do
  (inChan, outChan) <- Unagi.newChan
  ingestor <- Ingestor millisecs inChan <$> newTVarIO Map.empty
  void . forkIO $
    go outChan ingestor
  pure ingestor
  where
    deltaT :: NominalDiffTime
    deltaT = fromIntegral millisecs / 1000

    flushChan outChan = do
      (Element hasNext, _) <- Unagi.tryReadChan outChan
      maybeNext <- hasNext
      when (isJust maybeNext) $
        flushChan outChan

    -- Process the in-buffer by removing everything older than the cutoff
    -- deltaT and writing it, sorted by timestamp, into the out-queue.
    go outChan Ingestor{..} = forever $ do
      threadDelay $ millisecs * 1000
      now <- getPOSIXTime
      let cutoff = posixSecondsToUTCTime $ now - deltaT
      !m <- atomically $
        stateTVar ingInBuffer $
          Map.spanAntitone ((< cutoff) . fst)
      Unagi.writeList2Chan ingChanRef $
        snd <$> Map.toAscList m
      -- We flush our own broadcast end of the channel immediately to not leak space
      flushChan outChan

-- For multiple / parallel consumers, each one requires its own IngestorReader
mkIngestorReader :: Ingestor -> IO IngestorReader
mkIngestorReader Ingestor{ingChanRef} = do
  !newOutChan <- Unagi.dupChan ingChanRef
  pure $ IngestorReader $ Unagi.readChan newOutChan

-- If the thread couldn't be started, there'll be a Just errormessage
ingestFileThreaded :: Ingestor -> FailureMode -> IngestMode -> FilePath -> IO (Maybe String)
ingestFileThreaded Ingestor{ingInBuffer, ingRetentionMs} failMode ingestMode fp =
  doesFileExist fp >>= \case
    False -> pure $ Just $ "ingestFileThreaded: file not found: " ++ fp
    True -> do
      void . forkIO $
        handle (\(ex :: SomeException) -> unless (failMode == DieSilently) (throwIO ex))
          thread
      pure Nothing
  where
    pollingDelay = 1000 * max 20 (ingRetentionMs `div` 40)
    filePathHash = djb2 fp

    thread = withFile fp ReadMode $ \hdl -> do
      when (ingestMode == FromFileEnd) $
        hSeek hdl SeekFromEnd 0
      forever $ do
        -- This whole polling logic exists to avoid lazy I/O
        threadDelay pollingDelay
        ingestLines hdl

    ingestLines :: Handle -> IO ()
    ingestLines hdl = try (ByteString.hGetLine hdl) >>= \case

      -- There's no requirement for each and every line to be a trace event.
      -- While with the Haskell node this is almost always the case, it can't be
      -- a general assumption.
      -- If the line doesn't look like a timestamped JSON object, we skip it.
      Right line -> do
        forM_ (decodeStrict' line) $ \(Timestamp ts) ->
          atomically $ modifyTVar' ingInBuffer $
            Map.insert (ts, filePathHash) line
        ingestLines hdl

      -- Swallow EOF while polling for new input, rethrow anything else
      Left (err :: IOException) | isEOFError err -> pure ()
                                | otherwise      -> ioError err

djb2 :: [Char] -> Word64
djb2 = foldl' (\acc (fromIntegral . ord -> i) -> ((acc `shiftL` 5) + acc) `xor` i) 5381


{-
-- example usage:
_testRunConsumer :: IO ()
_testRunConsumer = do
  ing <- mkIngestor 1000
  onErrorPrint =<< ingestFileThreaded ing DieSilently FromFileStart "log-small.txt"
  onErrorPrint =<< ingestFileThreaded ing DieSilently FromFileStart "something-else.txt"

  ingReader <- mkIngestorReader ing
  forever $
    readLineIngestor ingReader >>= print
  where
    onErrorPrint :: Maybe String -> IO ()
    onErrorPrint = mapM_ putStrLn
-}
