module Trace.Forward.Utils.ForwardSink
  ( ForwardSink (..)
  ) where

import Control.Concurrent.STM.TBQueue

data ForwardSink lo = ForwardSink
  { forwardQueue     :: !(TBQueue lo)
  , overflowCallback :: !([lo] -> IO ())
  }

