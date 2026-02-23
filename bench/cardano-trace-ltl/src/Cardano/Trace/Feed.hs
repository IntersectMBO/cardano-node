{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Trace.Feed(Filename, TemporalEvent(..), TemporalEventDurationMicrosec, read, readS, sanitize) where

import           Cardano.Logging.Types.TraceMessage

import           Prelude                            hiding (read)

import           Cardano.Trace.Ingest               (IngestorReader (..))
import           Data.Aeson                         (encode, throwDecodeStrict)
import qualified Data.ByteString.Char8              as BChar8
import qualified Data.Foldable                      as Foldable
import           Data.List                          (sortBy)
import           Data.Sequence                      (Seq, (|>))
import           Data.Time.Clock                    (UTCTime)
import           Data.Time.Clock.POSIX              (utcTimeToPOSIXSeconds)
import           Data.Word                          (Word64)
import           GHC.IO.Handle                      (Handle, hIsEOF)
import           GHC.IO.IOMode                      (IOMode (WriteMode))
import           Streaming
import           System.IO                          (IOMode (ReadMode),
                                                     openFile)

type Filename = String

utcToMicroseconds :: UTCTime -> Word64
utcToMicroseconds utcTime = round $ utcTimeToPOSIXSeconds utcTime * 1000000

deriving instance Eq TraceMessage
deriving instance Ord TraceMessage

-- | Temporal event represents multiple trace messages spanning some duration of time together with an index of the event.
data TemporalEvent = TemporalEvent {
  -- | Microseconds since epoch when the event begins.
  beg      :: Word64,
  messages :: [TraceMessage]
} deriving (Show, Eq, Ord)

-- | For performance considerations we group trace messages within the specified duration in one `TemporalEvent`.
type TemporalEventDurationMicrosec = Word

-- | Fill in one temporal event.
--   Returns the event, the starting time boundary of the next temporal event and the rest of the messages.
fill :: TemporalEventDurationMicrosec -> Seq TraceMessage -> Word64 -> [TraceMessage] -> (TemporalEvent, Word64, [TraceMessage])
fill duration acc t (x : xs) | utcToMicroseconds x.tmsgAt  <= t + fromIntegral duration = fill duration (acc |> x) t xs
fill duration acc t rest = (TemporalEvent t (Foldable.toList acc), t + fromIntegral duration, rest)

-- | Slice up the trace messages into consequtive temporal events.
slice :: TemporalEventDurationMicrosec -> [TraceMessage] -> [TemporalEvent]
slice _ [] = []
slice duration msg@(x : _) = go (utcToMicroseconds (tmsgAt x)) msg where
  go :: Word64 -> [TraceMessage] -> [TemporalEvent]
  go _ [] = []
  go t msg =
    let (e, !t', !msg') = fill duration mempty t msg in
    e : go t' msg'

-- | We assume its possible for the trace messages to come out of order. Remedy that here.
sortByTimestamp :: [TraceMessage] -> [TraceMessage]
sortByTimestamp = sortBy (\x y -> tmsgAt x `compare` tmsgAt y)

-- | Read a text file where every line is a json object representation of a `TraceMessage`.
--   Trace messages lying within the specified `TemporalEventDurationMicrosec` are grouped in `TemporalEvent`.
--   The trace messages are sorted by timestamp before any action.
read :: Filename -> TemporalEventDurationMicrosec -> IO [TemporalEvent]
read filename duration = do
  traces <- BChar8.lines <$> BChar8.readFile filename
  msgs <- sortByTimestamp <$> traverse throwDecodeStrict traces
  let events = slice duration msgs
  pure events

readLine :: Handle -> IO (Maybe TraceMessage)
readLine handle = hIsEOF handle >>= \case
  True -> pure Nothing
  False -> do
    line <- BChar8.hGetLine handle
    throwDecodeStrict line

writeLine :: Handle -> TraceMessage -> IO ()
writeLine handle msg = BChar8.hPutStrLn handle (BChar8.toStrict $ encode msg)

data TemporalEventBuilderSt = TemporalEventBuilderSt {
  -- | A message read from the file that hasn't been distributed yet (if any).
  nextBuffered :: !(Maybe TraceMessage),
  -- | The timestamp of the beginning of the next issued temporal event.
  nextBeg      :: !Word64,
  -- | The accumulation of trace messages to be issued in the next issued temporal event.
  nextMsgs     :: !(Seq TraceMessage),
  -- | Whether the file of trace messages has ended.
  nextTerminal :: !Bool
}

-- | Given a `Filename` input traverse the file line-by-line and
--   check if every line is a parsable `TraceMessage` with non-decreasing timestamp relative to the previous lines.
--   If a line in conflict with the above property is found, prompt the user to either keep it or delete the line.
--   The input file is not mutated. Rather, it is copied over line-by-line to the output file with offending lines dropped if so
--   desired by the user.
sanitize :: Filename -> Filename -> IO ()
sanitize input output = do
  inputH <- openFile input ReadMode
  outputH <- openFile output WriteMode
  readLine inputH >>= \case
    Nothing -> pure ()
    Just msg -> do
      writeLine outputH msg
      go inputH outputH (2 :: Word64) msg.tmsgAt

  where
    go inputH outputH lineNum prev = do
      readLine inputH >>= \case
        Nothing -> pure ()
        Just msg ->
          if utcToMicroseconds prev <= utcToMicroseconds msg.tmsgAt
          then do
            writeLine outputH msg
            go inputH outputH (lineNum + 1) msg.tmsgAt
          else prompt
           where
            prompt = do
              BChar8.putStrLn (BChar8.toStrict $ encode msg)
              putStrLn $ "  @ line " <> show lineNum <> ", remove (y/n)?"
              line <- getLine
              if
                | line == "" || line == "y" ->
                  go inputH outputH (lineNum + 1) prev
                | line == "n" -> do
                  writeLine outputH msg
                  go inputH outputH (lineNum + 1) msg.tmsgAt
                | otherwise -> prompt




readS :: IngestorReader -> TemporalEventDurationMicrosec -> Stream (Of TemporalEvent) IO ()
readS ingestor duration = do
  firstMsg <- lift (ingestor.readLineIngestor >>= throwDecodeStrict)
  unfold go $
   TemporalEventBuilderSt
     { nextBuffered = Just firstMsg
     , nextBeg = utcToMicroseconds firstMsg.tmsgAt
     , nextMsgs = mempty
     , nextTerminal = False
     }
  where
    go :: TemporalEventBuilderSt
       -> IO (Either () (Of TemporalEvent TemporalEventBuilderSt))
    go TemporalEventBuilderSt{nextTerminal = True} = pure (Left ())
    go TemporalEventBuilderSt{nextBuffered = Nothing, ..} = do
      msg <- readLineIngestor ingestor >>= throwDecodeStrict
      go (TemporalEventBuilderSt (Just msg) nextBeg nextMsgs False)
    go TemporalEventBuilderSt{nextBuffered = Just msg, ..} | utcToMicroseconds msg.tmsgAt <= nextBeg + fromIntegral duration =
        go (TemporalEventBuilderSt Nothing nextBeg (nextMsgs |> msg) False)
    go TemporalEventBuilderSt{nextBuffered = Just msg, ..} = pure $ Right $
      TemporalEvent nextBeg (Foldable.toList nextMsgs)
        :>
      TemporalEventBuilderSt (Just msg) (nextBeg + fromIntegral duration) mempty False
