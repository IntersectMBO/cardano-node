module Hedgehog.Extras.Stock.Monad
  ( forceM
  ) where

import           Control.DeepSeq (NFData, force)
import           Control.Monad

-- | Force the evaluation of the return value in a monadic computation.
forceM :: (Monad m, NFData a) => m a -> m a
forceM = (force <$!>)
