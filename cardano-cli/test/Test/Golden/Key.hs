{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Key
  ( keyTests
  ) where

import qualified Test.Golden.Key.NonExtendedKey

import qualified Hedgehog as H

keyTests :: IO Bool
keyTests =
  H.checkSequential
    $ H.Group "Key command group"
        [ ("golden_KeyNonExtendedKey", Test.Golden.Key.NonExtendedKey.golden_KeyNonExtendedKey)
        ]
