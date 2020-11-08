{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}


-- | Cardano eras, sometimes we have to distinguish them.
--
module Cardano.Api.Eras
  ( -- * Eras
    Byron
  , Shelley
  , Allegra
  , Mary
  , AsType(..)
  ) where

import Cardano.Api.HasTypeProxy


-- | A type used as a tag to distinguish the Byron era.
data Byron

-- | A type used as a tag to distinguish the Shelley era.
data Shelley

-- | A type used as a tag to distinguish the Allegra era.
data Allegra

  -- | A type used as a tag to distinguish the Mary era.
data Mary


instance HasTypeProxy Byron where
    data AsType Byron = AsByron
    proxyToAsType _ = AsByron

instance HasTypeProxy Shelley where
    data AsType Shelley = AsShelley
    proxyToAsType _ = AsShelley

instance HasTypeProxy Allegra where
    data AsType Allegra = AsAllegra
    proxyToAsType _ = AsAllegra

instance HasTypeProxy Mary where
    data AsType Mary = AsMary
    proxyToAsType _ = AsMary

