{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Trace.Forward.Utils.ForwardSink
  ( ForwardSink (..)
  ) where

import           Control.Concurrent.STM.TBQueue
import           Control.Concurrent.STM.TVar

data ForwardSink lo = ForwardSink
  { forwardQueue     :: !(TVar (TBQueue lo))
  , disconnectedSize :: !Word
  , connectedSize    :: !Word
  , wasUsed          :: !(TVar Bool)
  , overflowCallback :: !([lo] -> IO ())
  }
