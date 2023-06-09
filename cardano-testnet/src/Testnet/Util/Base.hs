module Testnet.Util.Base
  ( integration
  , integrationRetryWorkspace
  , integrationWorkspace
  , isLinux
  ) where

import           GHC.Stack (HasCallStack)
import           System.Info (os)

import qualified GHC.Stack as GHC
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H


integration :: HasCallStack => H.Integration () -> H.Property
integration f = GHC.withFrozenCallStack $ H.withTests 1 $ H.propertyOnce f

-- | The 'FilePath' in '(FilePath -> H.Integration ())' is the work space directory.
-- This is created (and returned) via 'H.workspace'.
integrationRetryWorkspace :: HasCallStack => Int -> FilePath -> (FilePath -> H.Integration ()) -> H.Property
integrationRetryWorkspace n workspaceName f = GHC.withFrozenCallStack $
  integration $ H.retry n $ \i ->
    H.runFinallies $ H.workspace (workspaceName <> "-" <> show i) f

-- | The 'FilePath' in '(FilePath -> H.Integration ())' is the work space directory.
-- This is created (and returned) via 'H.workspace'.
integrationWorkspace :: HasCallStack => FilePath -> (FilePath -> H.Integration ()) -> H.Property
integrationWorkspace workspaceName f = GHC.withFrozenCallStack $
  integration $ H.runFinallies $ H.workspace workspaceName f

isLinux :: Bool
isLinux = os == "linux"
