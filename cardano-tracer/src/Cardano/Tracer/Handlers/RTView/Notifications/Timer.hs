{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Tracer.Handlers.RTView.Notifications.Timer
  ( PeriodInSec
  , Timer
  , mkTimer
  , setCallPeriod
  , startTimer
  , stopTimer
  ) where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import           Control.Monad (forever, void)
import           Control.Monad.Extra (whenM)
import           Data.Word (Word32)
import           System.Time.Extra (sleep)

type PeriodInSec = Word32

data Timer = Timer
  { tCallPeriod  :: !(TVar PeriodInSec)
  , tElapsedTime :: !(TVar PeriodInSec)
  , tIsRunning   :: !(TVar Bool)
  }

mkTimer
  :: IO ()
  -> Bool
  -> PeriodInSec
  -> IO Timer
mkTimer ioAction state callPeriodInS = do
  callPeriod  <- newTVarIO callPeriodInS
  elapsedTime <- newTVarIO 0
  isRunning   <- newTVarIO state

  void . forkIO $ forever $ do
    sleep $ fromIntegral checkPeriod
    whenM (readTVarIO isRunning) $ do
      period  <- readTVarIO callPeriod
      elapsed <- readTVarIO elapsedTime
      if elapsed < period
        then
          -- Ok, just continue to wait.
          atomically $ modifyTVar' elapsedTime $ \current -> current + checkPeriod
        else do
          -- Done, we are ready to call the action.
          ioAction
          -- Reset elapsed time.
          atomically $ modifyTVar' elapsedTime . const $ 0

  return $
    Timer
      { tCallPeriod  = callPeriod
      , tElapsedTime = elapsedTime
      , tIsRunning   = isRunning
      }
 where
  checkPeriod :: PeriodInSec
  checkPeriod = 1

startTimer, stopTimer :: Timer -> IO ()
startTimer Timer{tIsRunning} = atomically $ modifyTVar' tIsRunning . const $ True
stopTimer  Timer{tIsRunning} = atomically $ modifyTVar' tIsRunning . const $ False

setCallPeriod :: Timer -> PeriodInSec -> IO ()
setCallPeriod Timer{tCallPeriod} p = atomically $ modifyTVar' tCallPeriod . const $ p

