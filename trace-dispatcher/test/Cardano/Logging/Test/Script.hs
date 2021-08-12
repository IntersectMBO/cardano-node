{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Cardano.Logging.Test.Script
  (
    runScriptSimple
  , runScriptMultithreaded
  ) where

import           Control.Concurrent (ThreadId, forkFinally, threadDelay)
import           Control.Concurrent.MVar
import           Control.Exception.Base (SomeException, throw)
import           Control.Monad (when)
import           Data.IORef (newIORef, readIORef)
import           Data.List (sort)
import           Data.Maybe (mapMaybe)
import           Test.QuickCheck

import           Cardano.Logging
import           Cardano.Logging.Test.Config ()
import           Cardano.Logging.Test.Messages
import           Cardano.Logging.Test.Tracer
import           Cardano.Logging.Test.Types

import           Debug.Trace


-- | Run a script in a single thread and uses the oracle to test for correctness
--   The duration of the test is given by time in seconds
runScriptSimple ::
     Double
  -> (TraceConfig -> ScriptRes -> Property)
  -> Property
runScriptSimple time oracle = do
  let generator  :: Gen (Script, TraceConfig) = arbitrary
  forAll generator (\ (script,conf) -> ioProperty $ do
    scriptResult <- playScript time conf 0 script
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
    forAll generator (\ (script1, script2, script3, conf) -> ioProperty $ do
      children :: MVar [MVar (Either SomeException ScriptRes)] <- newMVar []
      _ <- forkChild children (playScript time conf 0 script1)
      let start1 = scriptLength script1
      _ <- forkChild children (playScript time conf start1 script2)
      let start2 = start1 + scriptLength script2
      _ <- forkChild children (playScript time conf start2 script3)
      res <- waitForChildren children []
      let res' = mapMaybe
                  (\case
                          Right rR -> Just rR
                          Left _ -> Nothing) res
      let resErr = mapMaybe
                  (\case
                          Right _ -> Nothing
                          Left err -> Just err) res
      if not (null resErr)
        then throw (head resErr)
        else do
          let scriptResult = mergeResults res'
          trace ("stdoutTrRes " <> show (srStdoutRes scriptResult)
                  <> " forwardTrRes " <> show (srForwardRes scriptResult)
                  <> " ekgTrRes " <> show (srEkgRes scriptResult)) $
            pure $ oracle conf scriptResult)
  where
   forkChild :: MVar [MVar (Either SomeException ScriptRes)] -> IO ScriptRes -> IO ThreadId
   forkChild children io = do
       mvar <- newEmptyMVar
       childs <- takeMVar children
       putMVar children (mvar:childs)
       forkFinally io (putMVar mvar)
   waitForChildren :: MVar [MVar (Either SomeException ScriptRes)]
      -> [Either SomeException ScriptRes]
      -> IO [Either SomeException ScriptRes]
   waitForChildren children accum = do
     cs <- takeMVar children
     case cs of
       []   -> pure accum
       m:ms -> do
          putMVar children ms
          res <- takeMVar m
          waitForChildren children (res : accum)


-- | Plays a script in a single thread
playScript :: Double -> TraceConfig -> Int -> Script -> IO ScriptRes
playScript time config firstId (Script msgs) = do
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

  let sortedMsgs = sort msgs
  let (msgsWithIds,_) = withMessageIds firstId sortedMsgs
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

mergeResults :: [ScriptRes] -> ScriptRes
mergeResults results =
  let script      = Script $
                      concatMap
                        (\r -> case srScript r of
                                  Script scriptedList -> scriptedList) results
      stdOutRes   = concatMap srStdoutRes results
      forwardRes  = concatMap srForwardRes results
      ekgRes      = concatMap srEkgRes results
  in ScriptRes script stdOutRes forwardRes ekgRes
