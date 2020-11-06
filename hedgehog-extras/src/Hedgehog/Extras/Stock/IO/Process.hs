{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Hedgehog.Extras.Stock.IO.Process
  ( maybeWaitForProcess
  , waitSecondsForProcess
  , waitUntilForProcess
  , TimedOut(..)
  ) where

import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Data.Either
import           Data.Eq
import           Data.Function
import           Data.Int
import           Data.Maybe
import           Data.Ord
import           Data.Semigroup
import           Data.Time.Clock
import           GHC.Num
import           GHC.Real
import           System.Exit
import           System.IO
import           System.Process
import           Text.Show

import qualified Control.Concurrent as IO
import qualified Control.Concurrent.Async as IO
import qualified Data.Time.Clock as DTC
import qualified System.IO as IO
import qualified System.Process as IO

data TimedOut = TimedOut deriving (Eq, Show)

maybeWaitForProcess
  :: ProcessHandle
  -> IO (Maybe ExitCode)
maybeWaitForProcess hProcess =
  catch (fmap Just (IO.waitForProcess hProcess)) $ \(_ :: AsyncCancelled) -> return Nothing

waitUntil
  :: UTCTime
  -> IO TimedOut
waitUntil deadline = do
  now <- DTC.getCurrentTime
  let timeLeft = max (DTC.diffUTCTime deadline now) 0 :: NominalDiffTime
  IO.appendFile "/tmp/moo" $ show @Int (round (toRational timeLeft * 1000000)) <> "\n"
  IO.threadDelay (round (toRational timeLeft * 1000000))
  return TimedOut

waitSecondsForProcess
  :: Int
  -> ProcessHandle
  -> IO (Either TimedOut (Maybe ExitCode))
waitSecondsForProcess seconds hProcess = IO.race
  (IO.threadDelay (seconds * 1000000) >> return TimedOut)
  (maybeWaitForProcess hProcess)

waitUntilForProcess
  :: UTCTime
  -> ProcessHandle
  -> IO (Either TimedOut (Maybe ExitCode))
waitUntilForProcess deadline hProcess = IO.race
  (waitUntil deadline)
  (maybeWaitForProcess hProcess)
