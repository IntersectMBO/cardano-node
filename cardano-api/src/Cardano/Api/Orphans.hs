{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Api.Orphans () where

import           Cardano.Prelude (Generic, NFData, NoUnexpectedThunks)

import           Shelley.Spec.Ledger.Keys (KeyDiscriminator (..))

deriving instance Generic KeyDiscriminator
deriving instance NoUnexpectedThunks KeyDiscriminator
instance NFData KeyDiscriminator

