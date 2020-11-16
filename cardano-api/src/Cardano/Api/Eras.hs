{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}


-- | Cardano eras, sometimes we have to distinguish them.
--
module Cardano.Api.Eras
  ( -- * Eras
    ByronEra
  , ShelleyEra
  , AllegraEra
  , MaryEra

    -- * Deprecated aliases
  , Byron
  , Shelley
  , Allegra
  , Mary

    -- * Data family instances
  , AsType(AsByronEra, AsShelleyEra, AsAllegraEra, AsMaryEra,
           AsByron,    AsShelley,    AsAllegra,    AsMary)
  ) where

import           Cardano.Api.HasTypeProxy


-- | A type used as a tag to distinguish the Byron era.
data ByronEra

-- | A type used as a tag to distinguish the Shelley era.
data ShelleyEra

-- | A type used as a tag to distinguish the Allegra era.
data AllegraEra

-- | A type used as a tag to distinguish the Mary era.
data MaryEra


instance HasTypeProxy ByronEra where
    data AsType ByronEra = AsByronEra
    proxyToAsType _ = AsByronEra

instance HasTypeProxy ShelleyEra where
    data AsType ShelleyEra = AsShelleyEra
    proxyToAsType _ = AsShelleyEra

instance HasTypeProxy AllegraEra where
    data AsType AllegraEra = AsAllegraEra
    proxyToAsType _ = AsAllegraEra

instance HasTypeProxy MaryEra where
    data AsType MaryEra = AsMaryEra
    proxyToAsType _ = AsMaryEra


-- ----------------------------------------------------------------------------
-- Deprecated aliases
--

type Byron   = ByronEra
type Shelley = ShelleyEra
type Allegra = AllegraEra
type Mary    = MaryEra

{-# DEPRECATED Byron   "Use 'ByronEra' or 'ByronAddr' as appropriate" #-}
{-# DEPRECATED Shelley "Use 'ShelleyEra' or 'ShelleyAddr' as appropriate" #-}
{-# DEPRECATED Allegra "Use 'AllegraEra' instead" #-}
{-# DEPRECATED Mary    "Use 'MaryEra' instead" #-}

pattern AsByron   :: AsType ByronEra
pattern AsByron    = AsByronEra

pattern AsShelley :: AsType ShelleyEra
pattern AsShelley  = AsShelleyEra

pattern AsAllegra :: AsType AllegraEra
pattern AsAllegra  = AsAllegraEra

pattern AsMary    :: AsType MaryEra
pattern AsMary     = AsMaryEra

{-# DEPRECATED AsByron   "Use 'AsByronEra' instead" #-}
{-# DEPRECATED AsShelley "Use 'AsShelleyEra' instead" #-}
{-# DEPRECATED AsAllegra "Use 'AsAllegraEra' instead" #-}
{-# DEPRECATED AsMary    "Use 'AsMaryEra' instead" #-}

