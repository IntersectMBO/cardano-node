{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wextra #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Generics.SOP.Some
  ( -- *
    HasTypeData(..)
  , Data(..)
  , ppData
  , Ctor(..)
  , Field(..)
  , mkData
  , Form(..)
  , stillData
  , SomeAccessors(..)
  , Accessors(..)
    -- * Re-exports
  , And
  , Code
  , Generic
  , HasDatatypeInfo
  , Proxy(..)
  )
where

import Data.Kind
import Data.Text
import qualified GHC.Generics as GHC
import Generics.SOP

import Generics.SOP.Mapping hiding (cFields)
import Data.SOP.NP


data Form = Fun | Still

data Data  (f :: Form) (c :: Type -> Constraint) u = Data
  { moduleName :: !Text
  , typeName   :: !Text
  , dCtors     :: ![Ctor f c u]
  }

ppData :: Data f c u -> Text
ppData Data{moduleName=modname, typeName, dCtors} =
  pack (printf "%s.%s\n" modname typeName) <>
  Data.Text.intercalate "\n" (ppCtor <$> dCtors)
 where
   ppCtor Ctor{cName, cFields} =
     pack (printf "  %s\n" cName) <>
     Data.Text.intercalate "\n" (ppField <$> cFields)
   ppField Field{fName, fRep} =
     pack (printf "    %25s :: %s" fName (show fRep))

class    ( Generic u, HasDatatypeInfo u, Typeable u
         , All2 c (Code u)
         , All2 (And Typeable c) (Code u))
  => HasTypeData (c :: * -> Constraint) (u :: *) where
  typeData :: Proxy c -> Proxy u -> Data 'Fun c u
  typeData c u = collect c u mkData

instance ( Generic u, HasDatatypeInfo u, Typeable u
         , All2 c (Code u)
         , All2 (And Typeable c) (Code u))
  => HasTypeData (c :: * -> Constraint) (u :: *)

-- XXX: factor
mkData
  :: forall (u :: *) c xss. (All2 (And Typeable c) xss, Typeable u)
  => ForgetADT u c xss (Data 'Fun c u)
mkData (ADTDesc _ _ (ADT modname ty _ _) ctors) =
  Data (pack modname) (pack ty)
       (collapse_NP
        (cliftA_NP (Proxy @(All (And Typeable c)))
         (K . mkCtor @u @c)
         (ctors :: NP (CtorDesc u c) xss)))
mkData (ADTDesc _ _ (Newtype modname ty _) ctors) =
  Data (pack modname) (pack ty)
       (collapse_NP
        (cliftA_NP (Proxy @(All (And Typeable c)))
         (K . mkCtor @u @c)
         (ctors :: NP (CtorDesc u c) xss)))

data Ctor  (f :: Form) (c :: Type -> Constraint) u = Ctor
  { cName      :: !Text
  , cFields    :: ![Field f c u]
  }

-- XXX: factor
mkCtor
  :: forall (u :: *) c (xs :: [*])
  . (All (And Typeable c) xs, Typeable u)
  => ForgetCtor u c xs (Ctor 'Fun c u)
mkCtor (CtorDesc _ _ (Constructor name) fields) =
  Ctor (pack name)
       (hcollapse $ hcliftA (Proxy @(And Typeable c))
         (K . mkField @u)
         fields)
mkCtor (CtorDesc _ _ (Record cname _finfos) fields) =
  Ctor (pack cname)
       (hcollapse $ hcliftA (Proxy @(And Typeable c))
         (K . mkField @u)
         fields)
mkCtor (CtorDesc _ _ (Infix name _ass _fix) fields) =
  Ctor (pack name)
       (hcollapse $ hcliftA (Proxy @(And Typeable c))
         (K . mkField @u)
         fields)

-- WTF:
-- forall f xs. NP f xs -> Int -> Int
-- !=
--              NP f xs -> Int -> Int
-- ????
_lenNP :: forall f xs. NP f xs -> Int -> Int
_lenNP Nil acc = acc
_lenNP (_ :* xs) acc = _lenNP xs (acc + 1)

data Field (f :: Form) (c :: Type -> Constraint) u = Field
  { fName      :: !Text
  , fParentRep :: !SomeTypeRep
  , fRep       :: !SomeTypeRep
  , fAccess    :: !(Formed f c u)
  }

mkField
  :: forall u c x. (Typeable u, Typeable x, c x)
  => ForgetField u c x (Field 'Fun c u)
mkField (FieldDesc _c (FieldInfo name) getter) =
  Field (pack name)
        (someTypeRep $ Proxy @u)
        (someTypeRep $ Proxy @x)
        (SomeAccessors (Accessors getter (error "Setters not defined yet.")))

data SomeAccessors (c :: Type -> Constraint) u = forall a. (c a, Typeable a) => SomeAccessors
  { access      :: !(Accessors u c a)
  }

data Accessors u (c :: Type -> Constraint) a = c a => Accessors
  { getter      :: u -> a
  , setter      :: a -> u -> u
  }

type family Formed f c u :: Type where
  Formed 'Fun   c u = SomeAccessors c u
  Formed 'Still c u = ()

stillData :: Data 'Fun c u -> Data 'Still c u
stillData d = d { dCtors = stillCtor <$> dCtors d }
  where stillCtor  c = c { cFields = stillField <$> cFields c }
        stillField f = f { fAccess = () }

deriving instance Eq          (Data  'Still c u)
deriving instance GHC.Generic (Data  'Still c u)
deriving instance Ord         (Data  'Still c u)
deriving instance Show        (Data  'Still c u)

deriving instance Eq          (Ctor  'Still c u)
deriving instance GHC.Generic (Ctor  'Still c u)
deriving instance Ord         (Ctor  'Still c u)
deriving instance Show        (Ctor  'Still c u)

deriving instance Eq          (Field 'Still c u)
deriving instance GHC.Generic (Field 'Still c u)
deriving instance Ord         (Field 'Still c u)
deriving instance Show        (Field 'Still c u)
