{-# OPTIONS_GHC -Wno-deprecations #-} -- TODO Fix deprecations

module Test.Util
  ( ignoreOn
  , ignoreOnWindows
  , ignoreOnMac
  , ignoreOnMacAndWindows
  ) where

import           Data.Bool (bool)
import           Hedgehog (Property, PropertyName)
import           Hedgehog.Extras.Stock.OS (isWin32)
import           Prelude
import           Test.Tasty (TestName)
import           Test.Tasty.ExpectedFailure (wrapTest)
import           Test.Tasty.Providers (testPassed)
import           Test.Tasty.Runners (TestTree, Result(resultShortDescription))

import qualified Test.Tasty.Hedgehog as H
import qualified System.Info as SYS

type Os = String

ignoreOnWindows :: TestName -> PropertyName -> Property -> TestTree
ignoreOnWindows name propName prop =
  bool id (ignoreOn "Windows") isWin32 $ H.testPropertyNamed name propName prop

ignoreOnMac :: TestName -> PropertyName -> Property -> TestTree
ignoreOnMac name propName prop =
  bool id (ignoreOn "MacOS") isMacOS $ H.testPropertyNamed name propName prop

ignoreOnMacAndWindows :: TestName -> PropertyName -> Property -> TestTree
ignoreOnMacAndWindows name propName prop =
  bool id (ignoreOn "MacOS and Windows") (isMacOS || isWin32) $ H.testPropertyNamed name propName prop

isMacOS :: Bool
isMacOS = SYS.os == "darwin"

ignoreOn :: Os -> TestTree -> TestTree
ignoreOn os = wrapTest $ const $ return $
  (testPassed ("IGNORED on " <> os))
    { resultShortDescription = "IGNORED on " <> os
    }
