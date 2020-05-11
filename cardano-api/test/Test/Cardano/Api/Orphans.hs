{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Cardano.Api.Orphans
  (
  ) where

import           Cardano.Api

import           Cardano.Prelude

import           Test.Cardano.Crypto.Orphans ()

deriving instance Eq SigningKey
deriving instance Eq PaymentVerificationKey
deriving instance Eq StakingVerificationKey
