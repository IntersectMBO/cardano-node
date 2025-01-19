{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Test.Unlog.LogObjectDB where

import           Cardano.Prelude
import           Cardano.Unlog.LogObjectDB

import qualified Data.Set as Set (difference, empty)

import           Hedgehog


-- This property ensures there are converter implementations
-- for all LOBody constructors. These converters are used
-- to reliably reconstruct a LOBody value from a database result row.

prop_LOBody_converter_for_each_constructor = property $
  allLOBodyConstructors `Set.difference` knownLOBodyConstructors
  ===
  Set.empty

tests :: IO Bool
tests =
  checkSequential $$discover
