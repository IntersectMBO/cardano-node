{-# LANGUAGE RecordWildCards #-}

module System.Metrics.Store.Deltify
       ( Deltify(..)
       , mkDeltify
       ) where

import           Control.Concurrent.Class.MonadSTM.TVar
import           Control.Monad.Class.MonadSTM
import qualified Data.HashMap.Strict                    as HM
import           System.Metrics                         (Sample)


data Deltify m = Deltify
  { deltaPutAll   :: Sample -> m ()           -- ^ put an entire Sample into the backbuffer
  , deltaDeltify  :: Sample -> m Sample       -- ^ return delta to current backbuffer, and update backbuffer
  , deltaPutDelta :: Sample -> m ()           -- ^ apply delta to backbuffer
  }

newtype BackBuffer = BackBuffer Sample        -- ^ the backbuffer holds the latest sample that was base for a Response


{-# SPECIALIZE mkDeltify :: IO (Deltify IO) #-}
mkDeltify :: MonadSTM m => m (Deltify m)
mkDeltify = do
  backBuffer <- newTVarIO (BackBuffer HM.empty)

  let
    deltaPutAll     = atomically . writeTVar backBuffer . BackBuffer
    deltaDeltify    = atomically . stateTVar backBuffer . getDelta
    deltaPutDelta   = atomically . modifyTVar' backBuffer . putDelta
  pure Deltify{..}
  where
    putDelta :: Sample -> BackBuffer -> BackBuffer
    putDelta curr (BackBuffer prev) = BackBuffer (HM.union curr prev)

    getDelta :: Sample -> BackBuffer -> (Sample, BackBuffer)
    getDelta curr (BackBuffer prev) = (updateMetrics, BackBuffer curr)
      where
        updateMetrics = HM.differenceWith (\v' v -> if v' == v then Nothing else Just v') curr prev
