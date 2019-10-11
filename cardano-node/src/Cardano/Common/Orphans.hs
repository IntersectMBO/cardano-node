{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Common.Orphans
  ()
where

import           Codec.Serialise (Serialise(..))
import           GHC.Generics

import           Ouroboros.Consensus.Ledger.Byron
                 ( ByronBlockOrEBB, GenTx(..)
                 , decodeByronGenTx, encodeByronGenTx)
import           Ouroboros.Consensus.Ledger.Byron.Config (ByronConfig)

deriving instance Generic (GenTx (ByronBlockOrEBB ByronConfig))
instance Serialise (GenTx (ByronBlockOrEBB ByronConfig)) where
  decode = decodeByronGenTx
  encode = encodeByronGenTx
