module Testnet.Util.Ignore
  ( ignoreOn
  , ignoreOnWindows
  , ignoreOnMac
  , ignoreOnMacAndWindows
  , disabled
  ) where

import           Data.Bool (bool)
import           Hedgehog (Property)
import           Hedgehog.Extras.Stock.OS (isWin32)
import           Prelude
import           Test.Tasty.ExpectedFailure (wrapTest)
import           Test.Tasty.Providers (testPassed)
import           Test.Tasty.Runners (Result (resultShortDescription), TestTree)

import qualified System.Info as SYS
import qualified Test.Tasty.Hedgehog as H

type Os = String

ignoreOnWindows :: String -> Property -> TestTree
ignoreOnWindows pName prop =
  bool id (ignoreOn "Windows") isWin32 $ H.testProperty pName prop

ignoreOnMac :: String -> Property -> TestTree
ignoreOnMac pName prop =
  bool id (ignoreOn "MacOS") isMacOS $ H.testProperty pName prop

ignoreOnMacAndWindows :: String -> Property -> TestTree
ignoreOnMacAndWindows pName prop =
  bool id (ignoreOn "MacOS and Windows") (isMacOS || isWin32) $ H.testProperty pName prop

isMacOS :: Bool
isMacOS = SYS.os == "darwin"

ignoreOn :: Os -> TestTree -> TestTree
ignoreOn os = wrapTest $ const $ return $
  (testPassed ("IGNORED on " <> os))
    { resultShortDescription = "IGNORED on " <> os
    }

disabled :: String -> Property -> TestTree
disabled pName prop = ignoreOn "Disabled" $ H.testProperty pName prop
