{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Cardano.Config.Orphans
  (
  ) where

import           Cardano.Crypto.DSIGN.Class (SignKeyDSIGN)

import           Cardano.Prelude

import           Shelley.Spec.Ledger.Crypto (DSIGN)
import           Shelley.Spec.Ledger.Keys (SKey (..))

import           Test.Cardano.Crypto.Orphans ()

deriving instance Eq (SignKeyDSIGN (DSIGN crypto)) => Eq (SKey crypto)
