{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Tracer.Test.Logs.Rotator
  ( tests
  ) where

import           Test.Tasty
import           Test.Tasty.QuickCheck

tests :: TestTree
tests = localOption (QuickCheckTests 1) $ testGroup "Test.Logs.Rotator"
  [ 
  ]
