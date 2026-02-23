module Cardano.LTL.Lang.Fragment(findAtoms, normaliseFragment) where

import           Cardano.LTL.Lang.Formula
import qualified Cardano.LTL.Lang.Formula            as F
import           Cardano.LTL.Lang.Fragment.Fragment0 as F0
import           Cardano.LTL.Lang.Fragment.Fragment1 as F1
import           Cardano.LTL.Lang.Fragment.Fragment2 as F2
import           Data.Maybe
import           Data.Set                            (Set)
import qualified Data.Set                            as Set
import           Prelude                             hiding (abs, and, not, or)
import Control.Applicative ((<|>))

-- | Try to retract `GuardedFormula` into `Fragment0` taking the atom to be the given (x = v).
retract :: Eq ty => (PropVarIdentifier, PropValue) -> Formula event ty -> Maybe (Fragment0 event ty)
retract _ (F.Atom {})        = Nothing
retract _ (F.UntilN {})      = Nothing
retract _ (F.Forall {})      = Nothing
retract _ (F.ExistsN {})     = Nothing
retract _ (F.ForallN {})     = Nothing
retract _ (F.NextN {})       = Nothing
retract _ (F.Next {})        = Nothing
retract abs (F.And a b)      = F0.And <$> retract abs a <*> retract abs b
retract abs (F.Or a b)       = F0.Or <$> retract abs a <*> retract abs b
retract abs (F.Implies a b)  = F0.Implies <$> retract abs a <*> retract abs b
retract abs (F.Not a)        = F0.Not <$> retract abs a
retract _ F.Top              = Just F0.Top
retract _ F.Bottom           = Just F0.Bottom
retract _ (F.PropForall {})  = Nothing
retract _ (F.PropForallN {}) = Nothing
retract (!x, !v) (F.PropEq rel (Var x') v') | x == x' && v == v'
                             = Just (F0.Atom rel)
retract _ (F.PropEq {})      = Nothing

-- | Evaluate `Fragment0` to `Fragment1`
toFragment1 :: Fragment0 event ty -> Fragment1 event ty
toFragment1 (F0.Atom ty)     = F1.Atom ty
toFragment1 (F0.Not x)       = F1.not (toFragment1 x)
toFragment1 (F0.And a b)     = F1.And (toFragment1 a) (toFragment1 b)
toFragment1 (F0.Or a b)      = F1.Or (toFragment1 a) (toFragment1 b)
toFragment1 (F0.Implies a b) = toFragment1 (F0.Not a `F0.Or` b)
toFragment1 F0.Top           = F1.Top
toFragment1 F0.Bottom        = F1.Bottom

-- | Evaluate `Fragment1` to `Fragment2`
toFragment2 :: (Ord event, Ord ty) => Fragment1 event ty -> Fragment2 event ty
toFragment2 (F1.Atom ty)    = F2.Atom ty
toFragment2 (F1.NotAtom ty) = F2.NotAtom ty
toFragment2 F1.Bottom       = F2.Bottom
toFragment2 F1.Top          = F2.Top
toFragment2 (F1.And a b)    = F2.and (toFragment2 a) (toFragment2 b)
toFragment2 (F1.Or a b)     = F2.or (toFragment2 a) (toFragment2 b)

-- | Embed `Fragment2` into `GuardedFormula` interpretting the given pair as (x = v).
toFormula :: (PropVarIdentifier, PropValue) -> Fragment2 event ty -> Formula event ty
toFormula (!x, !v) (F2.Atom ty)    = F.PropEq ty (Var x) v
toFormula (!x, !v) (F2.NotAtom ty) = F.Not (F.PropEq ty (Var x) v)
toFormula _ F2.Bottom              = F.Bottom
toFormula _ F2.Top                 = F.Top

-- | Find all `Fragment0` atoms in the form of (x = v) in the formula "now".
findAtoms :: Formula event ty -> Set (PropVarIdentifier, PropValue) -> Set (PropVarIdentifier, PropValue)
findAtoms (F.Atom {}) set             = set
findAtoms (F.UntilN {}) set           = set
findAtoms (F.Forall {}) set           = set
findAtoms (F.ExistsN {}) set          = set
findAtoms (F.ForallN {}) set          = set
findAtoms (F.NextN {}) set            = set
findAtoms (F.Next _) set              = set
findAtoms (F.And phi psi) set         = findAtoms phi (findAtoms psi set)
findAtoms (F.Or phi psi) set          = findAtoms phi (findAtoms psi set)
findAtoms (F.Implies phi psi) set     = findAtoms phi (findAtoms psi set)
findAtoms (F.Not phi) set             = findAtoms phi set
findAtoms F.Bottom set                = set
findAtoms F.Top set                   = set
findAtoms (F.PropEq _ (Var x) v) set  = Set.insert (x, v) set
findAtoms (F.PropEq {}) set           = set
findAtoms (F.PropForall _ phi) set    = findAtoms phi set
findAtoms (F.PropForallN _ _ phi) set = findAtoms phi set

-- | Given a set of propositional equalities {xᵢ = vᵢ}ᵢ and a formula, if the formula can be retracted into
--   `Fragment0` where the atom is taken to be one of the equalities (xᵢ = vᵢ), computes normal form in `Fragment0` and
--   embeds the result back into `Formula`. By construction the formula can be retracted for at most one (xᵢ = vᵢ) from the set.
normaliseFragment :: (Ord event, Ord ty) => Set (PropVarIdentifier, PropValue) -> Formula event ty -> Maybe (Formula event ty)
normaliseFragment set phi = go (Set.toList set) where
 go [] = Nothing
 go (atom : atoms) = toFormula atom . toFragment2 . toFragment1 <$> retract atom phi <|> go atoms
