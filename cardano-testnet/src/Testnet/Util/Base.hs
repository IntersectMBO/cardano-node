module Testnet.Util.Base
  ( integration
  , integrationRetryWorkspace
  , isLinux
  ) where

import           GHC.Stack (HasCallStack)
import           System.Info (os)

import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H


integration :: HasCallStack => H.Integration () -> H.Property
integration = H.withTests 1 . H.propertyOnce

integrationRetryWorkspace :: Int -> FilePath -> (FilePath -> H.Integration ()) -> H.Property
integrationRetryWorkspace n workspaceName f = integration $ H.retry n $ \i ->
  H.runFinallies $ H.workspace (workspaceName <> "-" <> show i) f

isLinux :: Bool
isLinux = os == "linux"
