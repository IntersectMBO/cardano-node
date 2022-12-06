{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wextra #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Generics.SOP.Mapping
  ( -- *
    CollectADT
  , CollectCtor
  , CollectField
  , ADTDesc(..)
  , CtorDesc(..)
  , FieldDesc(..)
  , ForgetADT
  , ForgetCtor
  , ForgetField
  , collect
  , FieldFun(..)
  , liftAllFields
  , mapSOP
    -- * Re-exports
  , All(..)
  , And
  , Generic
  , Const(..)
  , ConstructorInfo(..)
  , DatatypeInfo(..)
  , FieldInfo(..)
  , NP(..)
  , Proxy(..)
  )
where

import Data.Functor.Const (Const(..))
import Data.Kind
import Data.SOP.NP
import Generics.SOP
import Generics.SOP qualified as SOP
import Generics.SOP.Traversal


-- | Generic ADT mappers used by 'mapSOP',
--   that extract typed structure information.
type CollectADT   (a :: (* -> Constraint) -> [[*]] -> *)
                  (r :: (* -> Constraint) -> [*] -> *)
                  (f :: (* -> Constraint) -> * -> *)
                   u
                   c
                  (xss :: [[*]])
  =  Proxy c
  -> Proxy u
  -> DatatypeInfo xss
  -> NP (r c) xss
  -> a c xss

type CollectCtor  (r :: (* -> Constraint) -> [*] -> *)
                  (f :: (* -> Constraint) -> * -> *)
                   u
                   c
                  (xs :: [*])
  =  Proxy c
  -> Proxy u
  -> ConstructorInfo xs
  -> NP (f c) xs
  -> r c xs

type CollectField (f :: (* -> Constraint) -> * -> *)
                   u
                   c
                  (x :: *)
  =  Proxy c
  -> FieldInfo x
  -> (u -> x)
  -> f c x

-- | Typed intermediate representation of structure.
data ADTDesc (u :: *) (c :: * -> Constraint) (xss :: [[*]]) = All2 c xss => ADTDesc
  { aProxy     :: Proxy c
  , aProxyU    :: Proxy u
  , aInfo      :: DatatypeInfo xss
  , aCtors     :: NP (CtorDesc u c) xss
  }

data CtorDesc (u :: *) (c :: * -> Constraint) (xs :: [*]) = All c xs => CtorDesc
  { cProxy     :: Proxy c
  , cProxyU    :: Proxy u
  , cInfo      :: ConstructorInfo xs
  , cFields    :: NP (FieldDesc u c) xs
  }

data FieldDesc (u :: *) (c :: * -> Constraint) x = c x => FieldDesc
  { fProxy     :: Proxy c
  , fInfo      :: FieldInfo x
  , fGetter    :: u -> x
  }

-- | Collapse typed evidence.
type ForgetADT   u c (xss :: [[*]]) fa
  =  ADTDesc     u c  xss ->        fa

type ForgetCtor  u c (xs :: [*])    fr
  =  CtorDesc    u c  xs  ->        fr

type ForgetField u c (x :: *)       ff
  =  FieldDesc   u c  x   ->        ff

-- | Collect collapsed information about a data type,
--   as a list of per-constructor lists of per-field extractions,
--   by mapping over typed evidence of structure, as represented
--   by FieldDesc.
collect
  :: forall u c xss fa {-fr ff-}
  . ( HasDatatypeInfo u
    , Code u ~ xss
    , All2 c xss)
  => Proxy c -> Proxy u {-> Proxy fr -> Proxy ff-}
  -> (          All2 c xss => ForgetADT   u c xss fa)
  -- -> (forall xs. All c xs  => ForgetCtor  u c xs  fr)
  -- -> (forall x.      c x   => ForgetField u c x   ff)
  -> fa
collect c u {-fr ff-} forgetADT {-forgetCtor forgetField-} =
  mapSOP c u (Proxy @(FieldDesc u)) {-fr ff-}
         ADTDesc
         CtorDesc
         FieldDesc
         forgetADT -- forgetCtor forgetField

-- ADT ::
--     ModuleName
--  -> DatatypeName
--  -> NP ConstructorInfo xss
--  -> POP StrictnessInfo xss
--  -> DatatypeInfo xss
-- Newtype ::
--     ModuleName
--  -> DatatypeName
--  -> ConstructorInfo '[x]
--  -> DatatypeInfo '[ '[x]]
--
-- Constructor ::
--      SListI xs
--   => ConstructorName
--   -> ConstructorInfo xs
-- Infix ::
--      ConstructorName
--   -> Associativity
--   -> Fixity
--   -> ConstructorInfo '[x, y]
-- Record ::
--      SListI xs
--   => ConstructorName
--   -> NP FieldInfo xs
--   -> ConstructorInfo xs
--
-- FieldInfo ::
--      FieldName
--   -> FieldInfo a

newtype FieldFun u r a =
  FieldFun
    (SOP.FieldInfo a
     -> (u -> a)
     -> r)

liftAllFields ::
  forall u xss r
  . ( Code u ~ xss
    , SOP.HasDatatypeInfo u)
  => Proxy u
  -> NP (NP (FieldFun u r)) xss
  -> [[r]]
liftAllFields (SOP.datatypeInfo -> (dti :: SOP.DatatypeInfo xss)) fss =
  case dti of
    SOP.ADT _ _ (cinfos :: NP SOP.ConstructorInfo xss) _ ->
      SOP.hcollapse $
      SOP.hliftA3
        (\fs (SOP.Record _ (finfos :: NP SOP.FieldInfo xs)) ctravs ->
            K $ SOP.hcollapse $
            SOP.hliftA3
              (\(FieldFun f) finfo trav ->
                 K $ f finfo (gtravget trav))
              fs
              finfos
              ctravs)
        fss
        cinfos
        (gtraversals :: NP (NP (GTraversal (->) (->) u)) xss)

-- | A parametrised version of 'collect', which potentially presents a choice of
--   the intermediate type, but currently, however, locked to 'FieldDesc'.
mapSOP
  :: forall
    (a :: (* -> Constraint) -> [[*]] -> *)   (fa :: *)
    (r :: (* -> Constraint) ->  [*]  -> *) {-(fr :: *)-}
    (f :: (* -> Constraint) ->   *   -> *) {-(ff :: *)-}
    u xss
    (c :: * -> Constraint)
  . ( Code u ~ xss
    , HasDatatypeInfo u
    , All2 c xss
    , a ~ ADTDesc   u
    , r ~ CtorDesc  u
    , f ~ FieldDesc u
    )
  => Proxy c -> Proxy u -> Proxy f -- > Proxy fr -> Proxy ff
  -> (          All2 c xss => CollectADT a r f u c xss)
  -> (forall xs. All c xs  => CollectCtor  r f u c xs)
  -> (forall x.      c x   => CollectField   f u c x)
  -> (          All2 c xss => ForgetADT        u c xss fa)
  -- -> (forall xs. All c xs  => ForgetCtor       u c xs  fr)
  -- -> (forall x.      c x   => ForgetField      u c x   ff)
  -> fa
mapSOP c u@(datatypeInfo -> (dti :: DatatypeInfo xss)) f {-fr-} {-ff-}
       cadt cctor cfield fadt =
  let polyfad :: All2 c xss => a c xss -> fa
      polyfad = fadt
  in case dti of
    SOP.ADT _moduleName _typeName _cInfos _ ->
      let typed :: a c xss
          typed = mapSum' c u f
                  cadt cctor cfield dti
      in polyfad typed
    SOP.Newtype _moduleName _typeName cInfo ->
      polyfad $
      ADTDesc c u dti $
      hcliftA2 (Proxy @(All c))
      (mapProduct c cctor cfield dti)
      (cInfo          :* Nil :: NP ConstructorInfo xss)
      (hd gtraversals :* Nil :: NP (NP (GTraversal (->) (->) u)) xss)

mapSum'
  :: forall (a :: (* -> Constraint) -> [[*]] -> *)
            (r :: (* -> Constraint) -> [*] -> *)
            (f :: (* -> Constraint) -> * -> *)
             u (c :: Type -> Constraint) xss
  . ( Generic u
    , Code u ~ xss
    , All2 c xss)
  => Proxy c -> Proxy u -> Proxy f
  -> (          All2 c xss => CollectADT a r f u c xss)
  -> (forall xs. All c xs  => CollectCtor  r f u c xs)
  -> (forall x.      c x   => CollectField   f u c x)
  -> DatatypeInfo xss
  -> a c xss
mapSum' c u _f adt ctor field dti@(SOP.ADT _ _ cinfos _) =
  adt c u dti $
    hcliftA2 (Proxy @(All c))
    (mapProduct c ctor field dti)
    (cinfos      :: NP ConstructorInfo               xss)
    (gtraversals :: NP (NP (GTraversal (->) (->) u)) xss)
mapSum' _ _ _ _ _ _ _ = error "Non-ADTs not supported."

mapProduct
  :: forall (r :: (* -> Constraint) -> [*] -> *)
            (f :: (* -> Constraint) -> * -> *)
             u (c :: Type -> Constraint)
            (xs :: [*]) (xss :: [[*]])
  . All c xs
  => Proxy c
  -> (      All c xs => CollectCtor  r f u c xs)
  -> (forall x. c x  => CollectField   f u c x)
  -> DatatypeInfo xss
  -> ConstructorInfo xs
  -> NP (GTraversal (->) (->) u) xs
  -> r c xs
mapProduct c ct fi _ consi@(Record _ finfos)  travs = mapFields c ct fi consi finfos travs
mapProduct c ct fi _ consi@(Constructor ctor) travs = mapFields c ct fi consi (hpure (FieldInfo $ "un"<>ctor)) travs
mapProduct _ _ _ (SOP.ADT _ ty _ _) _ _   = error $ "Infix ADTs not supported: type "<>ty
mapProduct _ _ _ (SOP.Newtype _ ty _) _ _ = error $ "Infix newtypes not supported: type "<>ty

mapFields
  :: forall (r :: (* -> Constraint) -> [*] -> *)
            (f :: (* -> Constraint) -> * -> *)
             u (c :: Type -> Constraint) (xs :: [*])
  . All c xs
  => Proxy c
  -> (           All c xs  => CollectCtor  r f u c xs)
  -> (forall x.      c x   => CollectField   f u c x)
  -> ConstructorInfo xs
  -> NP FieldInfo xs
  -> NP (GTraversal (->) (->) u) xs
  -> r c xs
mapFields c ctor field cinfo finfos traversals =
  ctor c (Proxy @u) cinfo $
  hcliftA2 c
  (mapField c field)
  finfos
  traversals

mapField
  :: forall (f :: (* -> Constraint) -> * -> *)
             u (c :: Type -> Constraint) (x :: *)
  . c x
  => Proxy c
  -> CollectField f u c x
  -> FieldInfo x
  -> GTraversal (->) (->) u x
  -> f c x
mapField c field finfo trav =
         field c finfo (gtravget trav :: u -> x)
