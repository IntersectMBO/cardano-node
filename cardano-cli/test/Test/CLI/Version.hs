{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Version
  ( golden_version
  ) where

import Cardano.Prelude

import Hedgehog (Property)

import qualified Test.OptParse as OP

{- HLINT ignore "Use camelCase" -}

golden_version :: Property
golden_version = OP.propertyOnce $ do
  void . OP.noteEvalM $ OP.execCardanoCLI
    [ "version"
    ]
