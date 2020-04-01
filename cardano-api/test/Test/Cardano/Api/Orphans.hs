{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Cardano.Api.Orphans
  (
  ) where

import           Cardano.Api

import           Cardano.Prelude

import           Test.Cardano.Crypto.Orphans ()

deriving instance Eq KeyPair
