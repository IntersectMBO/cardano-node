module Chairman.CallStack
  ( callerModuleName
  ) where

import           Data.Function
import           Data.Maybe
import           Data.String
import           Data.Tuple
import           GHC.Stack (HasCallStack, callStack, getCallStack, srcLocModule)

callerModuleName :: HasCallStack => String
callerModuleName = maybe "<no-module>" (srcLocModule . snd) (listToMaybe (getCallStack callStack))
