{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- | This module provides a test watchdog - an utility monitoring test cases and killing them if they don't
-- finish in time. To wrap an 'H.Integration' test case in a watchdog just use
-- @
-- runWithWatchdog watchdogConfig $ \watchdog -> do
--   -- body of your test case
-- @
module Testnet.Components.TestWatchdog
  ( runWithWatchdog_
  , runWithWatchdog
  , runWithDefaultWatchdog_
  , runWithDefaultWatchdog
  , Watchdog
  , kickWatchdog
  , poisonWatchdog
  ) where

import           Control.Concurrent (myThreadId, threadDelay, throwTo)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TChan (TChan, newTChanIO, tryReadTChan, writeTChan)
import           Control.Exception.Safe (Exception)
import           Control.Monad.IO.Class
import           Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime,
                   nominalDiffTimeToSeconds)
import           GHC.Conc (ThreadId)
import           GHC.Stack

import qualified Hedgehog.Extras as H

-- | Configuration for the watchdog.
newtype WatchdogConfig = WatchdogConfig
  { watchdogTimeout :: Int -- ^ Timeout in µs after which watchdog will kill the test case
  }

-- | Default watchdog config with 10 minutes timeout.
defaultWatchdogConfig :: WatchdogConfig
defaultWatchdogConfig = WatchdogConfig
  { watchdogTimeout = 600_000_000
  }

-- | A watchdog
data Watchdog = Watchdog
  { watchdogConfig :: !WatchdogConfig
  , watchedThreadId :: !ThreadId -- ^ monitored thread id
  , startTime :: !UTCTime -- ^ watchdog creation time
  , kickChan :: TChan WatchdogCommand -- ^ a queue of watchdog commands
  }

-- | Create a new watchdog
makeWatchdog :: MonadIO m
             => WatchdogConfig
             -> ThreadId -- ^ thread id which will get killed after timeouts expire
             -> m Watchdog
makeWatchdog config watchedThreadId' = liftIO $ do
  watchdog <- Watchdog config watchedThreadId' <$> getCurrentTime <*> newTChanIO
  kickWatchdog watchdog
  pure watchdog

-- | Run watchdog in a loop
runWatchdog :: MonadIO m
            => Watchdog
            -> m ()
runWatchdog w@Watchdog{watchedThreadId, startTime, kickChan} = liftIO $ do
  atomically (tryReadTChan kickChan) >>= \case
    Just PoisonPill ->
      -- deactivate watchdog
      pure ()
    Just (Kick timeout) -> do
      -- got a kick, wait for another period
      threadDelay timeout
      runWatchdog w
    Nothing -> do
      -- we are out of scheduled timeouts, kill the monitored thread
      currentTime <- getCurrentTime
      throwTo watchedThreadId . WatchdogException $ diffUTCTime currentTime startTime

-- | Watchdog command
data WatchdogCommand
  = Kick !Int -- ^ Add another delay
  | PoisonPill -- ^ Stop the watchdog

-- | Enqueue a kick for the watchdog. It will extend the timeout by another one defined in the watchdog
-- configuration.
kickWatchdog :: MonadIO m => Watchdog -> m ()
kickWatchdog Watchdog{watchdogConfig=WatchdogConfig{watchdogTimeout}, kickChan} = liftIO $
  atomically $ writeTChan kickChan (Kick watchdogTimeout)

-- | Enqueue a poison pill for the watchdog. It will stop the watchdog after all timeouts.
poisonWatchdog :: MonadIO m => Watchdog -> m ()
poisonWatchdog Watchdog{kickChan} = liftIO $
  atomically $ writeTChan kickChan PoisonPill


-- | Execute a test case with a watchdog.
runWithWatchdog :: HasCallStack
                => WatchdogConfig -- ^ configuration
                -> (HasCallStack => Watchdog -> H.Integration a) -- ^ a test case to be wrapped in watchdog
                -> H.Integration a
runWithWatchdog config testCase = do
  watchedThreadId <- liftIO myThreadId
  watchdog <- makeWatchdog config watchedThreadId
  H.withAsync (runWatchdog watchdog) $
    \_ -> testCase watchdog

-- | Execuate a test case with a watchdog.
runWithWatchdog_ :: HasCallStack
                => WatchdogConfig -- ^ configuration
                -> (HasCallStack => H.Integration a) -- ^ a test case to be wrapped in watchdog
                -> H.Integration a
runWithWatchdog_ config testCase = runWithWatchdog config (const testCase)

-- | Execute a test case with watchdog with default config.
runWithDefaultWatchdog :: HasCallStack
                       => (HasCallStack => Watchdog -> H.Integration a) -- ^ a test case to be wrapped in watchdog
                       -> H.Integration a
runWithDefaultWatchdog = runWithWatchdog defaultWatchdogConfig

-- | Execute a test case with watchdog with default config.
runWithDefaultWatchdog_ :: HasCallStack
                        => (HasCallStack => H.Integration a) -- ^ a test case to be wrapped in watchdog
                        -> H.Integration a
runWithDefaultWatchdog_ testCase = runWithDefaultWatchdog (const testCase)

-- | An exception thrown to the test case thread.
newtype WatchdogException = WatchdogException { timeElapsed :: NominalDiffTime }

instance Show WatchdogException where
  show WatchdogException{timeElapsed} =
    "WatchdogException: Test watchdog killed test case thread after " <> show @Int (round $ nominalDiffTimeToSeconds timeElapsed) <> " seconds."

instance Exception WatchdogException
