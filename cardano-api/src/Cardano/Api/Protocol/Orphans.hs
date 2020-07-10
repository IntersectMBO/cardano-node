{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Api.Protocol.Orphans () where

import           Cardano.Prelude

import           Cardano.Chain.Slotting (EpochSlots (..))

import           Ouroboros.Consensus.Cardano (SecurityParam (..))

deriving instance NFData EpochSlots
deriving instance NFData SecurityParam
