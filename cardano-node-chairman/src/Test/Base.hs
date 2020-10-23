module Test.Base
  ( integration
  ) where

import           Data.Function
import           GHC.Stack (HasCallStack)
import           Hedgehog (Property)

import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H

integration :: HasCallStack => H.Integration () -> Property
integration = H.withTests 1 . H.propertyOnce
