{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Config.Orphanage ()
  where

import Cardano.Prelude

import qualified Ouroboros.Consensus.BlockchainTime as Consensus


deriving instance Eq Consensus.SlotLength
deriving instance Num Consensus.SlotLength
