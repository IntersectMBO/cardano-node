module Chairman.Monad
  ( forceM
  ) where

import           Control.DeepSeq (NFData, force)
import           Control.Monad

forceM :: (Monad m, NFData a) => m a -> m a
forceM = (force <$!>)
