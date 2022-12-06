{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
-- | Generalized traversals
--
-- Intended to be imported qualified
--
-- > import Generics.SOP.Traversal as GTraversal
--
module Generics.SOP.Traversal (
    -- * Generalized traversals
    GTraversal
  , traversal
  , get
  , modify
  , set
    -- * Conversion
  , fromLens
  , fromIso
  , toLens
    -- * Generic computation of lenses for record type
  , gtraversals
    -- * Labels for the representation types
  , np
  , rep
  , sop
  , head
  , tail
  , i
  , gtravget
  ) where

import Prelude hiding (id, (.), curry, uncurry, const, head, tail)
import Control.Arrow
import Control.Category
import Data.Label.Mono (Lens)
import Data.Label.Point (Iso(..))
import qualified Data.Label.Mono as Lens

import Generics.SOP

{-------------------------------------------------------------------------------
  Generalized lens using two categories
-------------------------------------------------------------------------------}

-- | GTraversal generalizes a monomorphic lens by allowing for different categories
-- for the getter and modifier
data GTraversal r w z b = GTraversal (r z b) (w (w b b, z) z)

instance (Category r, ArrowApply w) => Category (GTraversal r w) where
  id = GTraversal id app
  (GTraversal f m) . (GTraversal g n) = GTraversal (f . g) (uncurry (curry n . curry m))

traversal :: r a b -> w (w b b, a) a -> GTraversal r w a b
traversal = GTraversal

get :: GTraversal r w a b -> r a b
get (GTraversal f _) = f

modify :: GTraversal r w a b -> w (w b b, a) a
modify (GTraversal _ g) = g

set :: Arrow w => GTraversal r w a b -> w (b, a) a
set l = modify l . first (arr const)

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

fromLens :: (Arrow r, ArrowApply w) => Lens (->) a b -> GTraversal r w a b
fromLens l =
  GTraversal (arr (Lens.get l))
        (uncurry $ \h -> arr (Lens.set l) . (h . arr (Lens.get l) &&& id))

fromIso :: (Arrow r, ArrowApply w) => Iso (->) a b -> GTraversal r w a b
fromIso (Iso f g) = GTraversal (arr f) (uncurry $ \h -> arr g . h . arr f)

toLens :: GTraversal cat cat a b -> Lens cat a b
toLens (GTraversal f g) = Lens.lens f g

{-------------------------------------------------------------------------------
  Generic computation of all traversals for a record type
-------------------------------------------------------------------------------}

gtraversals :: forall r w y xss . ( Generic y, Code y ~ xss
                                  , Arrow r, ArrowApply w
                                  , SListI xss
                                  , All SListI xss)
            => NP (NP (GTraversal r w y)) xss
gtraversals =
  hliftA
  (\(CTraversals p :: CTraversals r w xss xs)->
   hliftA
    (\l -> l . p . sop . rep)
    np)
  nptraversals

-- | Traversal pack selector for some constructor
data CTraversals r w xss xs
  = (SListI xss, All SListI xss, SListI xs)
  => CTraversals (GTraversal r w (NS (NP I) xss) (NP I xs))

nptraversals :: forall r w xss. (Arrow r, ArrowApply w, SListI xss, All SListI xss) => NP (CTraversals r w xss) xss
nptraversals = case sList :: SList xss of
  SNil  -> Nil
  SCons ->
    (CTraversals
      ((fromIso $ Iso (\(Z x) -> x) Z)
        :: GTraversal r w (NS (NP I) (xs1 ': xss')) (NP I xs1))
      :: (SListI xss', All SListI xss', SListI xs) =>       CTraversals r w (xs ': xss') xs)
    :*
    (hliftA
     ((\(CTraversals x) -> CTraversals (x . tail'))
      :: (SListI xss', All SListI xss', SListI xs)
      => CTraversals r w        xss'  x
      -> CTraversals r w (xs ': xss') x)
     (nptraversals :: (SListI xss', All SListI xss') => NP (CTraversals r w        xss')  xss')
     :: (SListI xss', All SListI xss', SListI xs) =>    NP (CTraversals r w (xs ': xss')) xss')

tail' :: (Arrow r, ArrowApply w, SListI xss, SListI xs) => GTraversal r w (NS (NP I) (xs ': xss)) (NS (NP I) xss)
tail' = fromLens $ Lens.lens (\(S xs) -> xs) (\(f, S xs) -> S (f xs))

tail  :: (Arrow r, ArrowApply w) =>                        GTraversal r w (NP f       (x ': xs))      (NP f  xs)
tail  = fromLens $ Lens.lens (\(_ :* xs) -> xs) (\(f, x :* xs) -> x :* f xs)

np :: forall r w xs. (Arrow r, ArrowApply w, SListI xs) => NP (GTraversal r w (NP I xs)) xs
np = case sList :: SList xs of
      SNil  -> Nil
      SCons -> (i . head            ::                   GTraversal r w (NP I (x ': xs1)) x)
               :*
               (hliftA (. tail) (np :: SListI xs1 => NP (GTraversal r w (NP I       xs1))  xs1)
                                    :: SListI xs1 => NP (GTraversal r w (NP I (x ': xs1))) xs1)

sop :: (Arrow r, ArrowApply w) => GTraversal r w (SOP I xss) (NS (NP I) xss)
sop = fromIso $ Iso (\(SOP x) -> x)  SOP

{-------------------------------------------------------------------------------
  Generalized lenses for representation types
-------------------------------------------------------------------------------}

i :: (Arrow r, ArrowApply w) => GTraversal r w (I a) a
i = fromIso $ Iso unI I

head :: (Arrow r, ArrowApply w) => GTraversal r w (NP f (x ': xs)) (f x)
head = fromLens $ Lens.lens
  (\(x :* _) -> x)
  (\(f, x :* xs) -> f x :* xs)

rep :: (Arrow r, ArrowApply w, Generic a) => GTraversal r w a (Rep a)
rep = fromIso $ Iso from to

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

const :: Arrow arr => c -> arr b c
const a = arr (const a)

curry :: Arrow cat => cat (a, b) c -> (a -> cat b c)
curry m a = m . (const a &&& id)

uncurry :: ArrowApply cat => (a -> cat b c) -> cat (a, b) c
uncurry a = app . arr (first a)

gtravget :: GTraversal r w a b -> r a b
gtravget (GTraversal f _) = f

--------------------------------------------------------------------------------
-- import           Generics.SOP                     as SOP
-- import           Generics.SOP.Traversal           as SOP

-- type SumChoiceT = Int

-- type ReadFieldT c t u a xss xs
--   = (All c xs, c a)
--   => Proxy c
--   -> Proxy (t, u, a)
--   -> DatatypeInfo xss
--   -> SumChoiceT
--   -> ConstructorInfo xs
--   -> FieldInfo a
--   -> (u -> a)
--   -> Result t a

-- recover
--   :: forall a (c :: Type -> Kind.Constraint) (t :: Type) xss.
--     ( Code a ~ xss, HasDatatypeInfo a
--     , All2 c xss)
--   => Proxy c
--   -> Proxy (t, a)
--   -> (forall f xs. (c f, All2 c xss) => ReadFieldT c t a f xss xs)
--   -> Result t a
-- recover pC pTA fieldf = let dti = datatypeInfo (Proxy @a) :: DatatypeInfo xss
--   in
--   case dti of
--     ADT _moduleName typeName cInfos -> do
--       let choice = 0
--       let pop        :: POP (Result t) xss     = recover' pC pTA fieldf $ (datatypeInfo (Proxy @a) :: DatatypeInfo xss)
--           ct         :: SOP (Result t) xss     = (!! choice) $ SOP.apInjs_POP pop
--           Comp mrsop :: (Result t) (SOP I xss) = hsequence ct
--       case SOP.sList :: SOP.SList xss of
--         SOP.SCons -> (SOP.to <$>) <$> mrsop
--     Newtype _moduleName typeName cInfo -> do
--       let nCInfos -- ~:: NP ((,) Int :.: ConstructorInfo) '[ '[x]]
--             = enumerate $ cInfo :* Nil
--           sop        :: SOP (Result t) xss =
--             SOP.SOP $ SOP.Z $
--             recoverCtor pC pTA fieldf dti
--             (SOP.hd nCInfos)
--             (SOP.hd ((gtraversals -- ~:: NP (NP (GTraversal (->) (->) s)) '[ '[x]]
--                         )))
--           Comp mdsop ::     (Result t) (SOP I xss) = hsequence sop
--       (SOP.to <$>) <$> mdsop

-- recover'
--   :: forall a (c :: Type -> Kind.Constraint) (t :: Type) xss.
--     ( SOP.Generic a, Code a ~ xss
--     , All2 c xss)
--   => Proxy c
--   -> Proxy (t, a)
--   -> (forall f xs. c f => ReadFieldT c t a f xss xs)
--   -> DatatypeInfo xss
--   -> POP (Result t) xss
-- recover' pC pTA fieldf dti@(ADT _ name cs) =
--   POP $ SOP.hcliftA2 (Proxy @(All c))
--         (recoverCtor pC pTA fieldf dti)
--         (enumerate cs)
--         (gtraversals :: NP (NP (GTraversal (->) (->) a)) xss)
-- recover' _ _ _ _ = error "Non-ADTs not supported."

-- -- * 1. Extract the constructor's product of field names
-- --   2. Feed that to the field-name->action interpreter
-- recoverCtor
--   :: forall a (c :: Type -> Kind.Constraint) (t :: Type) xss xs.
--     ( Code a ~ xss
--     , All c xs)
--   => Proxy c
--   -> Proxy (t, a)
--   -> (forall f. c f => ReadFieldT c t a f xss xs)
--   -> DatatypeInfo xss
--   -> (((,) SumChoiceT) :.: ConstructorInfo) xs
--   -> NP (GTraversal (->) (->) a) xs
--   -> NP (Result t) xs
-- recoverCtor pC pTA fieldf dti (Comp (consNr, consi@(Record _ finfos))) travs = recoverFields pC pTA fieldf dti consNr consi travs finfos
-- recoverCtor pC pTA fieldf dti (Comp (consNr, consi@Constructor{}))     travs = recoverFields pC pTA fieldf dti consNr consi travs (SOP.hpure (FieldInfo ""))
-- recoverCtor _ _ _ (ADT _ name _) _ _ =
--   error $ printf "Infix ADTs not supported: type %s." name

-- -- * Key part:  NP (K Text) xs -> NP m xs
-- --   convert a product of field names to a product of monadic actions yielding 'a'
-- recoverFields
--   :: forall (c :: Type -> Kind.Constraint) (t :: Type) u xss xs.
--     ( Code u ~ xss
--     , All c xs
--     , SListI xs)
--   => Proxy c
--   -> Proxy (t, u)
--   -> (forall a. ReadFieldT c t u a xss xs)
--   -> DatatypeInfo xss
--   -> SumChoiceT
--   -> ConstructorInfo xs
--   -> NP (GTraversal (->) (->) u) xs
--   -> NP (FieldInfo) xs
--   -> NP (Result t) xs
-- recoverFields pC _pTU fieldf dtinfo consNr cinfo traversals finfos =
--   hcliftA2 pC
--   (recoverField dtinfo consNr cinfo)
--   finfos
--   traversals
--   where
--     recoverField :: forall a. (c a)
--                  => DatatypeInfo xss
--                  -> SumChoiceT
--                  -> ConstructorInfo xs
--                  -> FieldInfo a
--                  -> GTraversal (->) (->) u a
--                  -> (Result t) a
--     recoverField dtinfo consNr cinfo finfo trav =
--       Comp $ fieldf (Proxy @c) (Proxy @(t, u, a))
--       dtinfo consNr cinfo finfo (gtravget trav :: u -> a)
