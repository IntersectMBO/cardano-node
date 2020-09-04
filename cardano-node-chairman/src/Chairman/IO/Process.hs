{-# LANGUAGE ScopedTypeVariables #-}

module Chairman.IO.Process
  ( maybeWaitForProcess
  , waitSecondsForProcess
  , TimedOut(..)
  ) where

import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Data.Either
import           Data.Function
import           Data.Int
import           Data.Maybe
import           GHC.Num
import           System.Exit
import           System.IO
import           System.Process

import qualified Control.Concurrent as IO
import qualified Control.Concurrent.Async as IO
import qualified System.Process as IO


data TimedOut = TimedOut

maybeWaitForProcess
  :: ProcessHandle
  -> IO (Maybe ExitCode)
maybeWaitForProcess hProcess =
  catch (fmap Just (IO.waitForProcess hProcess)) $ \(_ :: AsyncCancelled) -> return Nothing

waitSecondsForProcess
  :: Int
  -> ProcessHandle
  -> IO (Either TimedOut (Maybe ExitCode))
waitSecondsForProcess seconds hProcess = IO.race
  (IO.threadDelay (seconds * 1000000) >> return TimedOut)
  (maybeWaitForProcess hProcess)
