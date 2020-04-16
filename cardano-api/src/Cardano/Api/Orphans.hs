{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Api.Orphans () where

import           Cardano.Crypto.DSIGN.Class (SignKeyDSIGN, VerKeyDSIGN)

import           Cardano.Prelude (Generic, NFData, NoUnexpectedThunks)

import           Shelley.Spec.Ledger.Crypto (DSIGN)
import           Shelley.Spec.Ledger.Keys (KeyDiscriminator (..), DiscVKey (..), SKey (..))

deriving instance Generic KeyDiscriminator
deriving instance NoUnexpectedThunks KeyDiscriminator
instance NFData KeyDiscriminator

deriving instance Generic (DiscVKey kd c)
deriving instance NFData (VerKeyDSIGN (DSIGN c)) => NFData (DiscVKey kd c)

deriving instance Generic (SKey c)
deriving instance NFData (SignKeyDSIGN (DSIGN c)) => NFData (SKey c)
