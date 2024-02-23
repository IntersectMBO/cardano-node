{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
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

import           Control.Concurrent (ThreadId, forkFinally, threadDelay)
import           Control.Concurrent.MVar
import           Control.Exception.Base (SomeException, throw)
import           Control.Monad (when)
import           Data.IORef
import           Data.List (sort)
import           Data.Map.Strict (fromList)
import           Data.Maybe
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
  -> IORef Int
  -> IO (Trace IO Message)
  -> Property
runScriptForwarding TestSetup{..} msgCounter tracerGetter = do
      let generator :: Gen [Script] = vectorOf (unI tsThreads) $
            case unI tsMessages of
              Nothing -> scale (* 500) arbitrary
              Just numMsg -> Script <$> vectorOf numMsg arbitrary
      forAll generator (\ (scripts :: [Script])
        -> ioProperty $ do
          tr <- tracerGetter
          confState <- emptyConfigReflection
          configureTracers confState simpleTestConfig [tr]
          let scripts' = map (\ (Script sc) -> Script (sort sc))  scripts
              scripts'' = zipWith (\ (Script sc) ind -> Script (
                            withMessageIds (unI tsThreads) ind sc)) scripts' [0..]
              scripts''' = map (\ (Script sc) -> Script
                              $ map (withTimeFactor (unI tsTime)) sc) scripts''


          -- putStrLn ("runTest " ++ show scripts)
          children :: MVar [MVar (Either SomeException ())] <- newMVar []
          mapM_ (\sc -> forkChild children (playIt sc tr 0.0)) scripts'''
          res <- waitForChildren children []
          let resErr = mapMaybe
                      (\case
                              Right _ -> Nothing
                              Left err -> Just err) res
          threadDelay 500000 --wait 0,5 seconds
          if not (null resErr)
            then throw (head resErr)
            else        -- Oracle
              let numMsg = sum (map (\ (Script sc) -> length sc) scripts''')
              in if numMsg > 0 then do
                -- TODO mutiple files
                let logfileGlobPattern = unI tsWorkDir <> "/logs/*sock@*/node-*.json"
                logs <- glob logfileGlobPattern
                logFile <- case logs of
                             []    -> fail $ "No files match the logfile glob pattern: " <> logfileGlobPattern
                             _:_:_ -> fail $ "More than one file matches the logfile glob pattern: " <> logfileGlobPattern
                             x:_ -> pure x
                contents <- readFile logFile
                let lineLength = length (lines contents)
                totalNumMsg <- atomicModifyIORef msgCounter (\ac ->
                  let nc = ac + numMsg
                  in (nc, nc))
                pure (totalNumMsg == lineLength - 1)
              else do
                pure True

        )

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
withMessageIds :: Int -> MessageID -> [ScriptedMessage] -> [ScriptedMessage]
withMessageIds numThreads mid sMsgs = go mid sMsgs []
  where
    go _mid' [] acc = reverse acc
    go mid' (ScriptedMessage time msg : tl) acc =
      go (mid' + numThreads) tl (ScriptedMessage time (setMessageID msg mid') : acc)

withTimeFactor :: Double -> ScriptedMessage -> ScriptedMessage
withTimeFactor factor (ScriptedMessage time msg) =
    ScriptedMessage (time * factor) msg
