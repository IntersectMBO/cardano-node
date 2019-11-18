{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Common.Orphans
  ()
where

import           Codec.Serialise (Serialise(..))

import           Ouroboros.Consensus.Ledger.Byron
                 ( ByronBlock, GenTx(..)
                 , decodeByronGenTx, encodeByronGenTx)

instance Serialise (GenTx ByronBlock) where
  decode = decodeByronGenTx
  encode = encodeByronGenTx
