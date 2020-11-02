
module Cardano.Api.Eras
  ( -- * Eras
    Byron
  , Shelley
  , Allegra
  , Mary
  ) where

-- ----------------------------------------------------------------------------
-- Cardano eras, sometimes we have to distinguish them
--

-- | A type used as a tag to distinguish the Byron era.
data Byron

-- | A type used as a tag to distinguish the Shelley era.
data Shelley

-- | A type used as a tag to distinguish the Allegra era.
data Allegra

  -- | A type used as a tag to distinguish the Mary era.
data Mary
