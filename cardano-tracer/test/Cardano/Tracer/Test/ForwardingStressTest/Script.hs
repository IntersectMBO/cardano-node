{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

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
import           Data.ByteString.Lazy.Char8 (ByteString, pack)
import           Data.Either (partitionEithers)
import           Data.IORef
import           Data.List (nub)
import           Data.Map.Strict (fromList)
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           System.FilePath.Glob

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

-- | Run scripts in three threads in parallel.
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
      tr <- tracerGetter
      confState <- emptyConfigReflection
      configureTracers confState simpleTestConfig [tr]

      let scripts' = Vector.map (\(Script sc) -> Script (Utils.vectorSort sc)) scripts

          scripts'' = Vector.map (\(ind, Script sc) -> Script (withMessageIds (unI tsThreads) ind sc)) (Vector.indexed scripts')

          scripts''' = Vector.map (\(Script sc) -> Script
                          $ Vector.map (withTimeFactor (unI tsTime)) sc) scripts''

      let messages :: Vector Message
          messages = Vector.concatMap scriptMessages scripts'''

      threadDelay 0_500_000 --wait 0,5 seconds
      forConcurrently_ scripts''' do
        playIt tr 0.0

      let numMsg = sum (fmap (\ (Script sc) -> length sc) scripts''')
      if numMsg > 0 then do
        -- TODO mutiple files
        let logfileGlobPattern = unI tsWorkDir <> "/logs/*sock@*/node-*.json"
        logs <- glob logfileGlobPattern
        logFile <- case logs of
                     []    -> fail $ "No files match the logfile glob pattern: " <> logfileGlobPattern
                     _:_:_ -> fail $ "More than one file matches the logfile glob pattern: " <> logfileGlobPattern
                     x:_ -> pure x

        contents <- readFile logFile

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
      else
        pure (property True)

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
