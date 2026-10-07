{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Tracer.Test.ForwardingStressTest.Script
  ( TestSetup(..)
  , simpleTestConfig
  , getTestSetup
  , runScriptForwarding
  ) where

import           Cardano.Logging
import           Cardano.Tracer.Test.ForwardingStressTest.Config ()
import           Cardano.Tracer.Test.ForwardingStressTest.Messages
import           Cardano.Tracer.Test.ForwardingStressTest.Types
import           Cardano.Tracer.Test.TestSetup
import           Cardano.Tracer.Test.Utils
import qualified Cardano.Tracer.Test.Utils as Utils

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (forConcurrently_)
import           Control.Monad (when)
import           Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Char8 as StrictBS
import           Data.ByteString.Lazy.Char8 (ByteString, pack)
import           Data.Either (partitionEithers, rights)
import           Data.IORef
import           Data.List (nub)
import           Data.Map.Strict (fromList)
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           System.FilePath.Glob
import           System.IO (IOMode (ReadMode), SeekMode (AbsoluteSeek), hFileSize, hSeek,
                   withBinaryFile)
import           System.Timeout (timeout)

import           Test.QuickCheck

-- | configuration for testing
simpleTestConfig :: TraceConfig
simpleTestConfig = emptyTraceConfig {
  tcOptions = fromList
    [([],
         [ ConfSeverity (SeverityF (Just Debug))
         , ConfDetail DNormal
         , ConfBackend [Forwarder]
         ])
    ]
  }

-- | Run scripts using the configured number of concurrent producers.
--   The duration of the test is given by time in seconds
runScriptForwarding ::
     TestSetup Identity
  -> IORef [Int]
  -> IORef (Vector Message)
  -> IO (Trace IO Message)
  -> Property
runScriptForwarding TestSetup{..} msgCountersRef msgsRef tracerGetter = do
  let generator :: Gen (Vector Script)
      generator = Utils.sizedVectorOf (unI tsThreads)
        case unI tsMessages of
          Nothing -> scale (* 500) arbitrary
          Just numMsg -> Script <$> Utils.sizedVectorOf numMsg arbitrary
  forAll generator \(scripts :: Vector Script) -> ioProperty do
      let requestedSeconds = unI tsTime
          maxTimeoutSeconds = fromIntegral (maxBound :: Int) / 1_000_000
      when (isNaN requestedSeconds || isInfinite requestedSeconds
            || requestedSeconds > maxTimeoutSeconds - 50) $
        fail "The configured script duration must fit a finite timeout"
      let deadlineSeconds = max 60 (requestedSeconds + 50)
          deadlineMicros = ceiling (deadlineSeconds * 1_000_000)
      priorCounts <- readIORef msgCountersRef
      let barrier = Message2 (negate (length priorCounts + 1)) "end-to-end-barrier"
          expectedCount = sum (fmap scriptLength scripts) + 1
      progress <- newIORef Nothing
      -- Bound admission and the consumer acknowledgment together. A stalled
      -- consumer must fail, rather than leave a producer blocked forever.
      result <- timeout deadlineMicros do
          tr <- tracerGetter
          confState <- emptyConfigReflection
          configureTracers confState simpleTestConfig [tr]

          let scripts' = Vector.map (\(Script sc) -> Script (Utils.vectorSort sc)) scripts

              scripts'' = Vector.map (\(ind, Script sc) -> Script (withMessageIds (unI tsThreads) ind sc)) (Vector.indexed scripts')

              scripts''' = Vector.map (\(Script sc) -> Script
                              $ Vector.map (withTimeFactor (unI tsTime)) sc) scripts''

          let messages :: Vector Message
              messages = Vector.snoc (Vector.concatMap scriptMessages scripts''') barrier

          threadDelay 0_500_000 --wait 0,5 seconds
          forConcurrently_ scripts''' do
            playIt tr 0.0

          traceWith tr barrier

          let numMsg = sum (fmap (\ (Script sc) -> length sc) scripts''') + 1
          -- TODO multiple files
          let logfileGlobPattern = unI tsWorkDir <> "/logs/*sock_*/node-*.json"
          let prs :: ByteString -> Either String Message
              prs ""  = Left "empty line"
              prs str =
                case decode @Object str of
                  Nothing -> Left "no decode"
                  Just a ->
                    case KeyMap.lookup "data" a of
                    Nothing -> Left "no data"
                    Just deita ->
                      case fromJSON @Message deita of
                        Data.Aeson.Error str' -> Left str'
                        Data.Aeson.Success a' -> Right a'
          (logFile, contents) <- awaitBarrier logfileGlobPattern barrier prs progress

          let lineLength = length (lines contents)

          totalNumMsg :: [Int]
            <- atomicModifyIORef msgCountersRef \case
              []       -> let newLen = [numMsg]                  in (newLen, newLen)
              len:lens -> let newlen = (len + numMsg) : len:lens in (newlen, newlen)

          let parsedLines :: [Either String Message]
              parsedLines = map (prs . pack) (lines contents)

              failures       :: [String]
              parsedMessages :: [Message]
              (failures, parsedMessages) = partitionEithers parsedLines

          case nub failures of
            [] -> pure ()
            ["empty line"] -> pure ()
            _ -> error ".."

          totalMsgs :: Vector Message <- atomicModifyIORef msgsRef (\ac ->
            let nc = ac <> messages
            in (nc, nc))

          pure $ conjoin
            [ counterexample ("Number of messages (" ++ show (head totalNumMsg) ++ ") does not match log file " ++ logFile ++ " length: " ++ show (lineLength - 1)) do
                head totalNumMsg === (lineLength - 1)
            , counterexample "Messages do not match the Messages do not match." do
                Utils.vectorSort (Vector.fromList parsedMessages) === Utils.vectorSort totalMsgs
            ]
      case result of
        Just prop -> pure prop
        Nothing -> do
          observed <- readIORef progress
          pure $ counterexample
            ("Timed out after " ++ show deadlineSeconds
              ++ " seconds forwarding/draining " ++ show expectedCount
              ++ " messages including barrier " ++ show barrier
              ++ "; last observed logfile and byte size: " ++ show observed)
            (property False)

-- The barrier is admitted after all five producers finish. The single FIFO
-- acceptor writes and flushes a batch before requesting another, so observing
-- this final record acknowledges every earlier record without a blind sleep.
awaitBarrier :: FilePath
             -> Message
             -> (ByteString -> Either String Message)
             -> IORef (Maybe (FilePath, Integer))
             -> IO (FilePath, String)
awaitBarrier pattern' barrier parseMessage progress = go
 where
  go = do
    logs <- glob pattern'
    case logs of
      [] -> wait
      [logFile] -> do
        -- Only the final small barrier record is needed while observing; read
        -- the complete cumulative log once after it arrives for strict checks.
        (size, contents) <- withBinaryFile logFile ReadMode $ \handle -> do
          size <- hFileSize handle
          let bytes = min size 4096
          hSeek handle AbsoluteSeek (size - bytes)
          contents <- StrictBS.hGet handle (fromIntegral bytes)
          pure (size, contents)
        writeIORef progress (Just (logFile, size))
        if barrier `elem` rights (map (parseMessage . pack) (lines (StrictBS.unpack contents)))
          then do
            complete <- StrictBS.readFile logFile
            pure (logFile, StrictBS.unpack complete)
          else wait
      _ -> fail $ "More than one file matches the logfile glob pattern: " ++ pattern'
  wait = threadDelay 20_000 >> go

playIt :: Trace IO Message -> Double -> Script -> IO ()
playIt tr d (Script script) =
  case Vector.uncons script of
    Nothing -> pure ()
    Just (ScriptedMessage d1 m1, rest) -> do
      when (d < d1) do
        threadDelay (round ((d1 - d) * 1_000_000))
        -- this is in microseconds
      traceWith tr m1
      playIt tr d1 (Script rest)

-- | Adds a message id to every message.
-- MessageId gives the id to start with.
-- Returns a tuple with the messages with ids and
-- the successor of the last used messageId
withMessageIds :: Int -> MessageID -> Vector ScriptedMessage -> Vector ScriptedMessage
withMessageIds numThreads mid sMsgs = Vector.zipWith f sMsgs idVec where

  f :: ScriptedMessage -> Int -> ScriptedMessage
  f (ScriptedMessage time msg) mid' = ScriptedMessage time (setMessageID msg mid')

  idVec :: Vector Int
  idVec = Vector.iterateN len (+ numThreads) mid

  len :: Int
  len = Vector.length sMsgs

withTimeFactor :: Double -> ScriptedMessage -> ScriptedMessage
withTimeFactor factor (ScriptedMessage time msg) =
    ScriptedMessage (time * factor) msg
