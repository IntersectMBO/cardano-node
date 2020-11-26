{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
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
  , CardanoEra(..)
  , IsCardanoEra(..)

    -- * Deprecated aliases
  , Byron
  , Shelley
  , Allegra
  , Mary

    -- * Shelley-based eras
  , ShelleyBasedEra(..)
  , IsShelleyBasedEra(..)
  , ShelleyLedgerEra

    -- * Cardano eras, as Byron vs Shelley-based
  , CardanoEraStyle(..)
  , cardanoEraStyle

    -- * Data family instances
  , AsType(AsByronEra, AsShelleyEra, AsAllegraEra, AsMaryEra,
           AsByron,    AsShelley,    AsAllegra,    AsMary)
  ) where

import           Prelude

import           Data.Type.Equality (TestEquality(..), (:~:)(Refl))

import           Ouroboros.Consensus.Shelley.Eras as Ledger
                   (StandardShelley, StandardAllegra, StandardMary)

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

-- ----------------------------------------------------------------------------
-- Value level representation for Cardano eras
--

-- | This GADT provides a value-level representation of all the Cardano eras.
-- This enables pattern matching on the era to allow them to be treated in a
-- non-uniform way.
--
-- This can be used in combination with the 'IsCardanoEra' class to get access
-- to this value.
--
-- In combination this can often enable code that handles all eras, and does
-- so uniformly where possible, and non-uniformly where necessary.
--
data CardanoEra era where
     ByronEra   :: CardanoEra ByronEra
     ShelleyEra :: CardanoEra ShelleyEra
     AllegraEra :: CardanoEra AllegraEra
     MaryEra    :: CardanoEra MaryEra

deriving instance Eq   (CardanoEra era)
deriving instance Ord  (CardanoEra era)
deriving instance Show (CardanoEra era)

instance TestEquality CardanoEra where
    testEquality ByronEra   ByronEra   = Just Refl
    testEquality ShelleyEra ShelleyEra = Just Refl
    testEquality AllegraEra AllegraEra = Just Refl
    testEquality MaryEra    MaryEra    = Just Refl
    testEquality _          _          = Nothing


-- | The class of Cardano eras. This allows uniform handling of all Cardano
-- eras, but also non-uniform by making case distinctions on the 'CardanoEra'
-- constructors, or the 'CardanoEraStyle' constructors via `cardanoEraStyle`.
--
class HasTypeProxy era => IsCardanoEra era where
   cardanoEra      :: CardanoEra era

instance IsCardanoEra ByronEra where
   cardanoEra      = ByronEra

instance IsCardanoEra ShelleyEra where
   cardanoEra      = ShelleyEra

instance IsCardanoEra AllegraEra where
   cardanoEra      = AllegraEra

instance IsCardanoEra MaryEra where
   cardanoEra      = MaryEra


-- ----------------------------------------------------------------------------
-- Shelley-based eras
--

-- | While the Byron and Shelley eras are quite different, there are several
-- eras that are based on Shelley with only minor differences. It is useful
-- to be able to treat the Shelley-based eras in a mostly-uniform way.
--
-- Values of this type witness the fact that the era is Shelley-based. This
-- can be used to constrain the era to being a Shelley-based on. It allows
-- non-uniform handling making case distinctions on the constructor.
--
data ShelleyBasedEra era where
     ShelleyBasedEraShelley :: ShelleyBasedEra ShelleyEra
     ShelleyBasedEraAllegra :: ShelleyBasedEra AllegraEra
     ShelleyBasedEraMary    :: ShelleyBasedEra MaryEra

deriving instance Eq   (ShelleyBasedEra era)
deriving instance Ord  (ShelleyBasedEra era)
deriving instance Show (ShelleyBasedEra era)

-- | The class of eras that are based on Shelley. This allows uniform handling
-- of Shelley-based eras, but also non-uniform by making case distinctions on
-- the 'ShelleyBasedEra' constructors.
--
class IsCardanoEra era => IsShelleyBasedEra era where
   shelleyBasedEra :: ShelleyBasedEra era

instance IsShelleyBasedEra ShelleyEra where
   shelleyBasedEra = ShelleyBasedEraShelley

instance IsShelleyBasedEra AllegraEra where
   shelleyBasedEra = ShelleyBasedEraAllegra

instance IsShelleyBasedEra MaryEra where
   shelleyBasedEra = ShelleyBasedEraMary


-- ----------------------------------------------------------------------------
-- Cardano eras factored as Byron vs Shelley-based
--

-- | This is the same essential information as 'CardanoEra' but instead of a
-- flat set of alternative eras, it is factored into the legcy Byron era and
-- the current Shelley-based eras.
--
-- This way of factoring the eras is useful because in many cases the
-- major differences are between the Byron and Shelley-based eras, and
-- the Shelley-based eras can often be treated uniformly.
--
data CardanoEraStyle era where
     LegacyByronEra  ::                        CardanoEraStyle ByronEra
     ShelleyBasedEra :: ShelleyBasedEra era -> CardanoEraStyle era

deriving instance Eq   (CardanoEraStyle era)
deriving instance Ord  (CardanoEraStyle era)
deriving instance Show (CardanoEraStyle era)

-- | The 'CardanoEraStyle' for a 'CardanoEra'.
--
cardanoEraStyle :: CardanoEra era -> CardanoEraStyle era
cardanoEraStyle ByronEra   = LegacyByronEra
cardanoEraStyle ShelleyEra = ShelleyBasedEra ShelleyBasedEraShelley
cardanoEraStyle AllegraEra = ShelleyBasedEra ShelleyBasedEraAllegra
cardanoEraStyle MaryEra    = ShelleyBasedEra ShelleyBasedEraMary


-- ----------------------------------------------------------------------------
-- Conversion to Shelley ledger library types
--

-- | A type family that connects our era type tags to equivalent type tags used
-- in the Shelley ledger library.
--
-- This type mapping  connect types from this API with types in the Shelley
-- ledger library which allows writing conversion functions in a more generic
-- way.
--
type family ShelleyLedgerEra era where

  ShelleyLedgerEra ShelleyEra = Ledger.StandardShelley
  ShelleyLedgerEra AllegraEra = Ledger.StandardAllegra
  ShelleyLedgerEra MaryEra    = Ledger.StandardMary

