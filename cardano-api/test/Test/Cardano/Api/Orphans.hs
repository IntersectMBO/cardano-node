{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Cardano.Api.Orphans
  (
  ) where

import           Cardano.Api

import           Cardano.Crypto.DSIGN.Class (SignKeyDSIGN)

import           Cardano.Prelude

import           Shelley.Spec.Ledger.Crypto (DSIGN)
import           Shelley.Spec.Ledger.Keys (SKey (..))

import           Test.Cardano.Crypto.Orphans ()

deriving instance Eq SigningKey
deriving instance Eq VerificationKey
deriving instance Eq (SignKeyDSIGN (DSIGN crypto)) => Eq (SKey crypto)
