{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Node.Clock
  ( LogicalClock(..)
  , NumTicks(..)
  , Tick(..)
  , new
  , sufficientTimeFor
  , tickWatcher
  ) where

import           Control.Monad
import           Data.Eq
import           Data.Function
import           Data.Functor
import           Data.Maybe
import           Data.Ord
import           Data.Time (NominalDiffTime)
import           Data.Word
import           GHC.Enum
import           GHC.Num
import           GHC.Real (fromIntegral)
import           GHC.Stack
import           Prelude (error)
import           Text.Show
import           System.Random (Random)

import qualified Ouroboros.Consensus.BlockchainTime as BTime
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM
import           Ouroboros.Consensus.Util.Time ( nominalDelay )

import qualified Control.Monad.Class.MonadSTM.Strict as SSTM
import qualified Data.List as L

-- | Logical time unit
newtype Tick = Tick { tickToWord64 :: Word64 }
  deriving stock   (Show, Eq, Ord)
  deriving newtype (Num, Enum, Random)

-- | Number of ticks the test will run for
newtype NumTicks = NumTicks Word64

-- | Logical clock (in terms of ticks rather than actual 'UTCTime')
data LogicalClock m = LogicalClock
    { -- | Get the current " time "
      getCurrentTick :: STM m Tick

    , -- | Wait for the end of time (each clock has a maximum number of ticks)
      waitUntilDone  :: m ()

    , -- | Translate the logical clock to mock 'SystemTime'
      mockSystemTime :: BTime.SystemTime m
    }

-- | Time of a single tick
--
-- The exact value of 'tickDelay' delay is not very important, but the scale
-- should be somewhat on par with the "real time" (for example, the relation
-- between GC delay and slot length, which would in turn depend on some kind of
-- relation between ticks and slots).
tickDelay :: NominalDiffTime
tickDelay = 0.5

{-------------------------------------------------------------------------------
  Scheduling actions
-------------------------------------------------------------------------------}

-- | Execute action on every clock tick
tickWatcher :: LogicalClock m
            -> (Tick -> m ())
            -> Watcher m Tick Tick
tickWatcher clock action =
    Watcher {
        wFingerprint = id
      , wInitial     = Nothing
      , wNotify      = action
      , wReader      = getCurrentTick clock
      }

new :: IOLike m => ResourceRegistry m -> NumTicks -> m (LogicalClock m)
new registry numTicks = newWithDelay registry numTicks tickDelay

{-------------------------------------------------------------------------------
  Internal
-------------------------------------------------------------------------------}

-- | Generalization of 'new' that allows to override the 'tickDelay'
--
-- NOTE: Tests using the logical clock really should not need to know what the
-- tick delay is; that's kind of the point of a /logical/ clock after all.
newWithDelay :: (IOLike m, HasCallStack)
             => ResourceRegistry m
             -> NumTicks
             -> NominalDiffTime
             -> m (LogicalClock m)
newWithDelay registry (NumTicks numTicks) tickLen = do
    current <- newTVarIO 0
    done    <- newEmptyMVar ()
    _thread <- forkThread registry "ticker" $ do
                 -- Tick 0 is the first tick, so increment @numTicks - 1@ times
                 replicateM_ (fromIntegral numTicks - 1) $ do
                   -- Give simulator chance to execute other threads
                   threadDelay (nominalDelay tickLen)
                   atomically $ modifyTVar current (+ 1)

                 -- Give tests that need to do some final processing on the last
                 -- tick a chance to do that before we indicate completion.
                 threadDelay (nominalDelay tickLen)
                 putMVar done ()

    return LogicalClock {
        getCurrentTick = Tick <$> readTVar current
      , waitUntilDone  = readMVar done
      , mockSystemTime = BTime.SystemTime {
            BTime.systemTimeCurrent = do
              tick <- SSTM.atomically $ SSTM.readTVar current
              return $ BTime.RelativeTime $ fromIntegral tick * tickLen
          , BTime.systemTimeWait =
              return ()
          }
      }

-- | Set 'NumTicks' such that we will have seen all of the specified 'Tick's
sufficientTimeFor :: HasCallStack => [Tick] -> NumTicks
sufficientTimeFor [] = error "sufficientTimeFor: empty list"
sufficientTimeFor ts = NumTicks . L.maximum . L.map (succ . tickToWord64) $ ts
