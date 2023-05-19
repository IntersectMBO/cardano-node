{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Version
  ( golden_version
  ) where

import           Control.Monad (void)

import           Hedgehog (Property)
import           Test.Cardano.CLI.Util

{- HLINT ignore "Use camelCase" -}

golden_version :: Property
golden_version = propertyOnce $ do
  void $ execCardanoCLI
    [ "version"
    ]
