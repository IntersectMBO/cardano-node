{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Node.Orphans
  ()
where

import           Codec.Serialise (Serialise(..))
import           Control.Exception
import           GHC.Generics

import qualified Cardano.Chain.Genesis as Genesis
import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Ledger.Byron.Config

-- TODO: consider not throwing this, or wrap it in a local error type here
-- that has proper error messages.
instance Exception Genesis.ConfigurationError

deriving instance Generic (GenTx (ByronBlockOrEBB ByronConfig))
instance Serialise (GenTx (ByronBlockOrEBB ByronConfig)) where
  decode = decodeByronGenTx
  encode = encodeByronGenTx
