{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Test.Script
  (
    runScriptSimple
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Monad (when)
import           Data.IORef (newIORef, readIORef)
import           Data.List (sort)
import           Test.QuickCheck

import           Cardano.Logging
import           Cardano.Logging.Test.Messages
import           Cardano.Logging.Test.Tracer
import           Cardano.Logging.Test.Types
import           Cardano.Logging.Test.Config()

-- import           Debug.Trace


-- | Run a script in a single thread and uses the oracle to test for correctness
--   The duration of the test is given by time in seconds
runScriptSimple ::
     Double
  -> (TraceConfig -> ScriptRes -> Property)
  -> Property
runScriptSimple time oracle = do
  let generator  :: Gen (Script, TraceConfig) = arbitrary
  forAll generator (\ (script,conf) -> ioProperty $ do
    scriptResult <- playScript time conf script
    -- trace ("stdoutTrRes " <> show (srStdoutRes scriptResult)
    --         <> " forwardTrRes " <> show (srForwardRes scriptResult)
    --         <> " ekgTrRes " <> show (srEkgRes scriptResult)) $
    pure $ oracle conf scriptResult)

-- | Plays a script in a single thread
playScript :: Double -> TraceConfig -> Script -> IO ScriptRes
playScript time config (Script msgs) = do
  stdoutTrRef     <- newIORef []
  stdoutTracer'   <- testTracer stdoutTrRef
  forwardTrRef    <- newIORef []
  forwardTracer'  <- testTracer forwardTrRef
  ekgTrRef        <- newIORef []
  ekgTracer'      <- testTracer ekgTrRef
  tr              <- mkCardanoTracer
                      "Test"
                      namesForMessage
                      severityForMessage
                      privacyForMessage
                      stdoutTracer'
                      forwardTracer'
                      (Just ekgTracer')
  let sortedMsgs = sort msgs
  let (msgsWithIds,_) = withMessageIds 0 sortedMsgs
  let timedMessages = map (withTimeFactor time) msgsWithIds

  configureTracers config docMessage [tr]
  -- trace ("playScript " <> show timedMessages) $
  playIt (Script timedMessages) tr 0.0
  r1 <- readIORef stdoutTrRef
  r2 <- readIORef forwardTrRef
  r3 <- readIORef ekgTrRef
  pure (ScriptRes
          (Script timedMessages)
          (reverse r1)
          (reverse r2)
          (reverse r3))

-- | Play the current script in one thread
-- The time is in milliseconds
playIt :: Script -> Trace IO Message -> Double -> IO ()
playIt (Script []) _tr _d = pure ()
playIt (Script (ScriptedMessage d1 m1 : rest)) tr d = do
  when (d < d1) $ threadDelay (round ((d1 - d) * 1000000))
    -- this is in microseconds
  traceWith tr m1
  playIt (Script rest) tr d1

-- | Adds a message id to every message.
-- MessageId gives the id to start with.
-- Returns a tuple with the messages with ids and
-- the successor of the last used messageId
withMessageIds :: MessageID -> [ScriptedMessage] -> ([ScriptedMessage], MessageID)
withMessageIds mid sMsgs = go mid sMsgs []
  where
    go mid' [] acc = (reverse acc, mid')
    go mid' (ScriptedMessage time msg : tl) acc =
      go (mid' + 1) tl (ScriptedMessage time (setMessageID msg mid') : acc)

withTimeFactor :: Double -> ScriptedMessage -> ScriptedMessage
withTimeFactor factor (ScriptedMessage time msg) =
    ScriptedMessage (time * factor) msg
