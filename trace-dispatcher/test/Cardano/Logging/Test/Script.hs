{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Test.Script
  (
    runScriptSimple
  , runScriptMultithreaded
  , runScriptMultithreadedWithReconfig
  , runScriptMultithreadedWithConstantReconfig
  ) where

import           Control.Concurrent (ThreadId, forkFinally, threadDelay)
import           Control.Concurrent.MVar
import           Control.Exception.Base (SomeException, throw)
import           Control.Monad (liftM2, when)
import           Data.IORef (newIORef, readIORef)
import           Data.List (sort)
import           Data.Maybe (mapMaybe)
import           Data.Time.Clock.System
import           Test.QuickCheck

import           Cardano.Logging
import           Cardano.Logging.Test.Config ()
import           Cardano.Logging.Test.Messages
import           Cardano.Logging.Test.Tracer
import           Cardano.Logging.Test.Types

-- | Run a script in a single thread and uses the oracle to test for correctness
--   The duration of the test is given by time in seconds
runScriptSimple ::
     Double
  -> (TraceConfig -> ScriptRes -> Property)
  -> Property
runScriptSimple time oracle = do
  let generator  :: Gen (Script, TraceConfig) = arbitrary
  forAll generator (\ (Script msgs,conf) -> ioProperty $ do
    stdoutTrRef     <- newIORef []
    stdoutTracer'   <- testTracer stdoutTrRef
    forwardTrRef    <- newIORef []
    forwardTracer'  <- testTracer forwardTrRef
    ekgTrRef        <- newIORef []
    ekgTracer'      <- testTracer ekgTrRef
    tr              <- mkCardanoTracer
                        stdoutTracer'
                        forwardTracer'
                        (Just ekgTracer')
                        "Test"
                        namesForMessage
                        severityForMessage
                        privacyForMessage
    configureTracers conf docMessage [tr]
    let sortedMsgs = sort msgs
    let (msgsWithIds,_) = withMessageIds 0 sortedMsgs
    let timedMessages = map (withTimeFactor time) msgsWithIds
    playIt (Script timedMessages) tr 0.0
    r1 <- readIORef stdoutTrRef
    r2 <- readIORef forwardTrRef
    r3 <- readIORef ekgTrRef
    let scriptResult =  ScriptRes
                          (Script timedMessages)
                          (reverse r1)
                          (reverse r2)
                          (reverse r3)
    -- trace ("stdoutTrRes " <> show (srStdoutRes scriptResult)
    --         <> " forwardTrRes " <> show (srForwardRes scriptResult)
    --         <> " ekgTrRes " <> show (srEkgRes scriptResult)) $
    pure $ oracle conf scriptResult)


-- | Run three scripts in three threads in parallel
--   and use the oracle to test for correctness.
--   The duration of the test is given by time in seconds
runScriptMultithreaded ::
     Double
  -> (TraceConfig -> ScriptRes -> Property)
  -> Property
runScriptMultithreaded time oracle = do
    let generator  :: Gen (Script, Script, Script, TraceConfig) = arbitrary
    forAll generator (\ (Script msgs1, Script msgs2, Script msgs3, conf)
      -> ioProperty $ do
      stdoutTrRef     <- newIORef []
      stdoutTracer'   <- testTracer stdoutTrRef
      forwardTrRef    <- newIORef []
      forwardTracer'  <- testTracer forwardTrRef
      ekgTrRef        <- newIORef []
      ekgTracer'      <- testTracer ekgTrRef
      tr              <- mkCardanoTracer
                          stdoutTracer'
                          forwardTracer'
                          (Just ekgTracer')
                          "Test"
                          namesForMessage
                          severityForMessage
                          privacyForMessage
      configureTracers conf docMessage [tr]
      let sortedMsgs1 = sort msgs1
      let (msgsWithIds1,_) = withMessageIds 0 sortedMsgs1
      let timedMessages1 = map (withTimeFactor time) msgsWithIds1
      let start1 = length timedMessages1
      let sortedMsgs2 = sort msgs2
      let (msgsWithIds2,_) = withMessageIds start1 sortedMsgs2
      let timedMessages2 = map (withTimeFactor time) msgsWithIds2
      let start2 = start1 + length timedMessages2
      let sortedMsgs3 = sort msgs3
      let (msgsWithIds3,_) = withMessageIds start2 sortedMsgs3
      let timedMessages3 = map (withTimeFactor time) msgsWithIds3

      children :: MVar [MVar (Either SomeException ())] <- newMVar []
      _ <- forkChild children (playIt (Script timedMessages1) tr 0.0)

      _ <- forkChild children (playIt (Script timedMessages2) tr 0.0)

      _ <- forkChild children (playIt (Script timedMessages3) tr 0.0)
      res <- waitForChildren children []
      let resErr = mapMaybe
                  (\case
                          Right _ -> Nothing
                          Left err -> Just err) res
      if not (null resErr)
        then throw (head resErr)
        else do
          r1 <- readIORef stdoutTrRef
          r2 <- readIORef forwardTrRef
          r3 <- readIORef ekgTrRef
          let timedMessages = timedMessages1 ++ timedMessages2 ++ timedMessages3
              scriptResult =  ScriptRes
                                (Script timedMessages)
                                (reverse r1)
                                (reverse r2)
                                (reverse r3)
          -- trace ("stdoutTrRes " <> show (srStdoutRes scriptResult)
          --         <> " forwardTrRes " <> show (srForwardRes scriptResult)
          --         <> " ekgTrRes " <> show (srEkgRes scriptResult)) $
          pure $ oracle conf scriptResult)

-- | Run three scripts in three threads in parallel
--   and use the oracle to test for correctness.
--   The duration of the test is given by time in seconds
runScriptMultithreadedWithReconfig ::
     Double
  -> (TraceConfig -> ScriptRes -> Property)
  -> Property
runScriptMultithreadedWithReconfig time oracle = do
    let generator  :: Gen (Script, Script, Script, TraceConfig, TraceConfig)
          = arbitrary
        reconfigTimeGen = choose (0.0, time)
        generator' = liftM2 (,) generator reconfigTimeGen
    forAll generator'
      (\ ((Script msgs1, Script msgs2, Script msgs3, conf, conf2), reconfigTime) ->
        ioProperty $ do
      stdoutTrRef     <- newIORef []
      stdoutTracer'   <- testTracer stdoutTrRef
      forwardTrRef    <- newIORef []
      forwardTracer'  <- testTracer forwardTrRef
      ekgTrRef        <- newIORef []
      ekgTracer'      <- testTracer ekgTrRef
      tr              <- mkCardanoTracer
                          stdoutTracer'
                          forwardTracer'
                          (Just ekgTracer')
                          "Test"
                          namesForMessage
                          severityForMessage
                          privacyForMessage
      configureTracers conf docMessage [tr]
      let sortedMsgs1 = sort msgs1
      let (msgsWithIds1,_) = withMessageIds 0 sortedMsgs1
      let timedMessages1 = map (withTimeFactor time) msgsWithIds1
      let start1 = length timedMessages1
      let sortedMsgs2 = sort msgs2
      let (msgsWithIds2,_) = withMessageIds start1 sortedMsgs2
      let timedMessages2 = map (withTimeFactor time) msgsWithIds2
      let start2 = start1 + length timedMessages2
      let sortedMsgs3 = sort msgs3
      let (msgsWithIds3,_) = withMessageIds start2 sortedMsgs3
      let timedMessages3 = map (withTimeFactor time) msgsWithIds3

      children :: MVar [MVar (Either SomeException ())] <- newMVar []
      _ <- forkChild children (playIt (Script timedMessages1) tr 0.0)
      _ <- forkChild children (playIt (Script timedMessages2) tr 0.0)
      _ <- forkChild children (playIt (Script timedMessages3) tr 0.0)
      _ <- forkChild children (playReconfigure reconfigTime conf2 tr)

      res <- waitForChildren children []
      let resErr = mapMaybe
                  (\case
                          Right _ -> Nothing
                          Left err -> Just err) res
      if not (null resErr)
        then throw (head resErr)
        else do
          r1 <- readIORef stdoutTrRef
          r2 <- readIORef forwardTrRef
          r3 <- readIORef ekgTrRef
          let timedMessages = timedMessages1 ++ timedMessages2 ++ timedMessages3
              scriptResult =  ScriptRes
                                (Script timedMessages)
                                (reverse r1)
                                (reverse r2)
                                (reverse r3)
          -- trace ("stdoutTrRes " <> show (srStdoutRes scriptResult)
          --         <> " forwardTrRes " <> show (srForwardRes scriptResult)
          --         <> " ekgTrRes " <> show (srEkgRes scriptResult)) $
          pure $ oracle conf scriptResult)

-- | Run three scripts in three threads in parallel
--   and use the oracle to test for correctness.
--   The duration of the test is given by time in seconds
runScriptMultithreadedWithConstantReconfig ::
     Double
  -> (TraceConfig -> ScriptRes -> Property)
  -> Property
runScriptMultithreadedWithConstantReconfig time oracle = do
    let generator  :: Gen (Script, Script, Script, TraceConfig, TraceConfig)
          = arbitrary
    forAll generator
      (\ (Script msgs1, Script msgs2, Script msgs3, conf1, conf2) ->
        ioProperty $ do
      stdoutTrRef     <- newIORef []
      stdoutTracer'   <- testTracer stdoutTrRef
      forwardTrRef    <- newIORef []
      forwardTracer'  <- testTracer forwardTrRef
      ekgTrRef        <- newIORef []
      ekgTracer'      <- testTracer ekgTrRef
      tr              <- mkCardanoTracer
                          stdoutTracer'
                          forwardTracer'
                          (Just ekgTracer')
                          "Test"
                          namesForMessage
                          severityForMessage
                          privacyForMessage
      configureTracers conf1 docMessage [tr]
      let sortedMsgs1 = sort msgs1
      let (msgsWithIds1,_) = withMessageIds 0 sortedMsgs1
      let timedMessages1 = map (withTimeFactor time) msgsWithIds1
      let start1 = length timedMessages1
      let sortedMsgs2 = sort msgs2
      let (msgsWithIds2,_) = withMessageIds start1 sortedMsgs2
      let timedMessages2 = map (withTimeFactor time) msgsWithIds2
      let start2 = start1 + length timedMessages2
      let sortedMsgs3 = sort msgs3
      let (msgsWithIds3,_) = withMessageIds start2 sortedMsgs3
      let timedMessages3 = map (withTimeFactor time) msgsWithIds3

      children :: MVar [MVar (Either SomeException ())] <- newMVar []
      _ <- forkChild children (playIt (Script timedMessages1) tr 0.0)
      _ <- forkChild children (playIt (Script timedMessages2) tr 0.0)
      _ <- forkChild children (playIt (Script timedMessages3) tr 0.0)
      _ <- forkChild children (playReconfigureContinuously time conf1 conf2 tr)

      res <- waitForChildren children []
      let resErr = mapMaybe
                  (\case
                          Right _ -> Nothing
                          Left err -> Just err) res
      if not (null resErr)
        then throw (head resErr)
        else do
          r1 <- readIORef stdoutTrRef
          r2 <- readIORef forwardTrRef
          r3 <- readIORef ekgTrRef
          let timedMessages = timedMessages1 ++ timedMessages2 ++ timedMessages3
              scriptResult =  ScriptRes
                                (Script timedMessages)
                                (reverse r1)
                                (reverse r2)
                                (reverse r3)
          -- trace ("stdoutTrRes " <> show (srStdoutRes scriptResult)
          --         <> " forwardTrRes " <> show (srForwardRes scriptResult)
          --         <> " ekgTrRes " <> show (srEkgRes scriptResult)) $
          pure $ oracle conf2 scriptResult)


forkChild :: MVar [MVar (Either SomeException ())] -> IO () -> IO ThreadId
forkChild children io = do
   mvar <- newEmptyMVar
   childs <- takeMVar children
   putMVar children (mvar:childs)
   forkFinally io (putMVar mvar)

waitForChildren :: MVar [MVar (Either SomeException ())]
  -> [Either SomeException ()]
  -> IO [Either SomeException ()]
waitForChildren children accum = do
 cs <- takeMVar children
 case cs of
   []   -> pure accum
   m:ms -> do
      putMVar children ms
      res <- takeMVar m
      waitForChildren children (res : accum)

-- | Plays a script in a single thread
playReconfigure :: Double -> TraceConfig -> Trace IO Message -> IO ()
playReconfigure time config tr = do

  threadDelay (round (time * 1000000))
  configureTracers config docMessage [tr]

playReconfigureContinuously ::
     Double
  -> TraceConfig
  -> TraceConfig
  -> Trace IO Message
  -> IO ()
playReconfigureContinuously time config1 config2 tr = do
    startTime <- systemTimeToSeconds <$> getSystemTime
    go startTime 0
  where
    go :: Double -> Int -> IO ()
    go startTime alt = do
      timeNow <- systemTimeToSeconds <$> getSystemTime
      if timeNow - startTime > time
        then pure ()
        else if alt == 0
              then do
                 configureTracers config1 docMessage [tr]
                 go startTime 1
              else do
                 configureTracers config2 docMessage [tr]
                 go startTime 0


    systemTimeToSeconds :: SystemTime -> Double
    systemTimeToSeconds MkSystemTime {..} =
      fromIntegral systemSeconds + fromIntegral systemNanoseconds * 1.0E-9


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

-- mergeResults :: [ScriptRes] -> ScriptRes
-- mergeResults results =
--   let script      = Script $
--                       concatMap
--                         (\r -> case srScript r of
--                                   Script scriptedList -> scriptedList) results
--       stdOutRes   = concatMap srStdoutRes results
--       forwardRes  = concatMap srForwardRes results
--       ekgRes      = concatMap srEkgRes results
--   in ScriptRes script stdOutRes forwardRes ekgRes
