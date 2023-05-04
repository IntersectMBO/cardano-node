module Cardano.Api.IPC.Error
  ( AcquiringFailure(..)
  , toAcquiringFailure
  ) where

import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as Net.Query

data AcquiringFailure = AFPointTooOld
                      | AFPointNotOnChain
                      deriving (Eq, Show)

toAcquiringFailure :: Net.Query.AcquireFailure -> AcquiringFailure
toAcquiringFailure Net.Query.AcquireFailurePointTooOld = AFPointTooOld
toAcquiringFailure Net.Query.AcquireFailurePointNotOnChain = AFPointNotOnChain
