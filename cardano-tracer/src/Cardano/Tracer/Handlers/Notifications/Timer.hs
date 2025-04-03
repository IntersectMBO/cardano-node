{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE StrictData #-}

module Cardano.Tracer.Handlers.Notifications.Timer
  ( PeriodInSec
  , Timer(..)
  , mkTimer
  , mkTimerStderr
  , mkTimerStderrDieOnFailure
  , mkTimerDieOnFailure
  ) where

import           "trace-dispatcher" Cardano.Logging.Types (Trace(..))
import           Cardano.Tracer.MetaTrace (TracerTrace(TracerError), traceWith, stderrShowTracer)
import           Control.Concurrent (forkIO, myThreadId, killThread)
import           Control.Exception
import           Control.Monad.Extra (whenM)
import           Data.IORef (newIORef, readIORef, modifyIORef')
import           Data.Kind (Type)
import           Data.Word (Word32)
import qualified Data.Text as Text (pack)
import           GHC.Conc (threadStatus, ThreadStatus (ThreadRunning))
import           System.Time.Extra (sleep)

type PeriodInSec :: Type
type PeriodInSec = Word32

checkPeriod :: PeriodInSec
checkPeriod = 1

traceOnly :: Trace IO TracerTrace -> String -> IO ()
traceOnly tracer = 
  traceWith tracer . TracerError . Text.pack

type Timer :: Type
data Timer = Timer
  { threadAlive   :: !(IO Bool)
  , threadKill    :: !(IO ())
  , setCallPeriod :: !(PeriodInSec -> IO ())
  , startTimer    :: !(IO ())
  , stopTimer     :: !(IO ())
  }

mkTimer
  :: Trace IO TracerTrace
  -> IO ()
  -> Bool
  -> PeriodInSec
  -> IO Timer
mkTimer = mkTimerOnFailure (pure ()) 

mkTimerStderr
  :: IO ()
  -> Bool
  -> PeriodInSec
  -> IO Timer
mkTimerStderr = mkTimer stderrShowTracer

mkTimerStderrDieOnFailure
  :: IO ()
  -> Bool
  -> PeriodInSec
  -> IO Timer
mkTimerStderrDieOnFailure = mkTimerDieOnFailure stderrShowTracer

mkTimerDieOnFailure
  :: Trace IO TracerTrace
  -> IO ()
  -> Bool
  -> PeriodInSec
  -> IO Timer
mkTimerDieOnFailure = mkTimerOnFailure (killThread =<< myThreadId)

mkTimerOnFailure
  :: IO ()
  -> Trace IO TracerTrace
  -> IO ()
  -> Bool
  -> PeriodInSec
  -> IO Timer
mkTimerOnFailure onFailure tracer io state callPeriod_sec = do
  callPeriod  <- newIORef callPeriod_sec
  elapsedTime <- newIORef 0
  isRunning   <- newIORef state

  let wait  :: IO () = modifyIORef' elapsedTime (+ checkPeriod)
  let reset :: IO () = modifyIORef' elapsedTime (const 0)
  let tryIO :: IO () = try @SomeException io >>= \case
        Left exception -> do
          traceOnly tracer (displayException exception)
          onFailure 
        _ -> reset

  let run ::  IO ()
      run = do
        sleep (fromIntegral checkPeriod)
        whenM (readIORef isRunning) do
          period  <- readIORef callPeriod
          elapsed <- readIORef elapsedTime
          if elapsed < period then wait else tryIO
          run

  threadId <- forkIO run

  pure Timer
    { threadAlive   = (== ThreadRunning) <$> threadStatus threadId
    , threadKill    = killThread threadId
    , setCallPeriod = modifyIORef' callPeriod . const
    , startTimer    = modifyIORef' isRunning (const True)
    , stopTimer     = modifyIORef' isRunning (const False)
    }

