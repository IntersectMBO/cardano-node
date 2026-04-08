module Cardano.ReCon.LTL.Internal.IR.HomogeneousFormula (
    HomogeneousFormula(..)
  , Extended(..)
  , toFormula
  , lower
  , eval
  , quote
  , equiv
  , retract
  , normaliseHomogeneous) where

import           Cardano.ReCon.Common.Types (BinRel (..))
import           Cardano.ReCon.LTL.Formula (Formula, IntTerm (..), IntValue, TextTerm (..),
                   VariableIdentifier)
import qualified Cardano.ReCon.LTL.Formula as F
import           Cardano.ReCon.LTL.Internal.IR.HomogeneousFormula.FinFree (Extended (..))
import qualified Cardano.ReCon.LTL.Internal.IR.HomogeneousFormula.FinFree as FinFree

import           Data.Function (on)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)

-- | A `Formula` with no temporal operators.
--   Equivalence of two `HomogeneousFormula`s is decidable.
data HomogeneousFormula =
   ------------ Connective -------------
     Or HomogeneousFormula HomogeneousFormula
   | And HomogeneousFormula HomogeneousFormula
   | Not HomogeneousFormula
   | Implies HomogeneousFormula HomogeneousFormula
   | Top
   | Bottom
   -------------------------------------

   ----------- Event property ----------
   | PropIntForall   VariableIdentifier HomogeneousFormula
   | PropTextForall  VariableIdentifier HomogeneousFormula
   | PropIntForallN  VariableIdentifier (Set IntValue)  HomogeneousFormula
   | PropTextForallN VariableIdentifier (Set Text) HomogeneousFormula
   | PropIntExists   VariableIdentifier HomogeneousFormula
   | PropTextExists  VariableIdentifier HomogeneousFormula
   | PropIntExistsN  VariableIdentifier (Set IntValue)  HomogeneousFormula
   | PropTextExistsN VariableIdentifier (Set Text) HomogeneousFormula
   | PropIntBinRel BinRel IntTerm  IntTerm
   | PropTextEq    TextTerm  Text
   deriving (Show, Eq, Ord)
   -------------------------------------

toFormula :: HomogeneousFormula -> Formula event ty
toFormula (And a b)                    = F.And (toFormula a) (toFormula b)
toFormula (Or a b)                     = F.Or (toFormula a) (toFormula b)
toFormula (Implies a b)                = F.Implies (toFormula a) (toFormula b)
toFormula (Not a)                      = F.Not (toFormula a)
toFormula Bottom                       = F.Bottom
toFormula Top                          = F.Top
toFormula (PropIntBinRel rel a b)       = F.PropIntBinRel rel Set.empty a b
toFormula (PropTextEq a b)             = F.PropTextEq Set.empty a b
toFormula (PropIntForall x phi)        = F.PropIntForall x (toFormula phi)
toFormula (PropTextForall x phi)       = F.PropTextForall x (toFormula phi)
toFormula (PropIntForallN x dom phi)   = F.PropIntForallN x dom (toFormula phi)
toFormula (PropTextForallN x dom phi)  = F.PropTextForallN x dom (toFormula phi)
toFormula (PropIntExists x phi)        = F.PropIntExists x (toFormula phi)
toFormula (PropTextExists x phi)       = F.PropTextExists x (toFormula phi)
toFormula (PropIntExistsN x dom phi)   = F.PropIntExistsN x dom (toFormula phi)
toFormula (PropTextExistsN x dom phi)  = F.PropTextExistsN x dom (toFormula phi)

-- | Lower a `HomogeneousFormula` to `FinFree` by eliminating all finite-domain
--   quantifiers.  Finite universal (∀x ∈ dom) unfolds to a conjunction; finite
--   existential (∃x ∈ dom) unfolds to a disjunction.
--
--   @'eval' = 'FinFree.eval' . 'lower'@
lower :: HomogeneousFormula -> FinFree.FinFree
lower (Or phi psi)      = FinFree.Or (lower phi) (lower psi)
lower (And phi psi)     = FinFree.And (lower phi) (lower psi)
lower (Not phi)         = FinFree.Not (lower phi)
lower (Implies phi psi) = FinFree.Implies (lower phi) (lower psi)
lower Top               = FinFree.Top
lower Bottom            = FinFree.Bottom
lower (PropIntForall  x phi) = FinFree.PropIntForall  x (lower phi)
lower (PropTextForall x phi) = FinFree.PropTextForall x (lower phi)
lower (PropIntExists  x phi) = FinFree.PropIntExists  x (lower phi)
lower (PropTextExists x phi) = FinFree.PropTextExists x (lower phi)
lower (PropIntBinRel rel t v) = FinFree.PropIntBinRel rel t v
lower (PropTextEq t v)        = FinFree.PropTextEq t v
-- ∀x ∈ {v₁,...,vₙ}. φ  ≡  φ[v₁/x] ∧ ... ∧ φ[vₙ/x]   (⊤ when dom is empty)
lower (PropIntForallN x dom phi) =
  foldr (FinFree.And . \v -> FinFree.substInt  v x (lower phi)) FinFree.Top    (Set.toList dom)
lower (PropTextForallN x dom phi) =
  foldr (FinFree.And . \v -> FinFree.substText (Val v) x (lower phi)) FinFree.Top    (Set.toList dom)
-- ∃x ∈ {v₁,...,vₙ}. φ  ≡  φ[v₁/x] ∨ ... ∨ φ[vₙ/x]   (⊥ when dom is empty)
lower (PropIntExistsN x dom phi) =
  foldr (FinFree.Or  . \v -> FinFree.substInt  v x (lower phi)) FinFree.Bottom (Set.toList dom)
lower (PropTextExistsN x dom phi) =
  foldr (FinFree.Or  . \v -> FinFree.substText (Val v) x (lower phi)) FinFree.Bottom (Set.toList dom)

-- | Evaluate the `HomogeneousFormula` onto `Bool`.
eval :: HomogeneousFormula -> Bool
eval = FinFree.eval . lower

-- | This is the "easy" part of the iso: `HomogeneousFormula` ≅ `Bool`
quote :: Bool -> HomogeneousFormula
quote True  = Top
quote False = Bottom

-- | Check equivalence of two `HomogeneousFormula`s.
equiv :: HomogeneousFormula -> HomogeneousFormula -> Bool
equiv = on (==) eval

retract :: Formula event ty -> Maybe HomogeneousFormula
retract = go Set.empty where
  go :: Set VariableIdentifier -> Formula event ty -> Maybe HomogeneousFormula
  go _     (F.ForallN {})                   = Nothing
  go _     (F.ExistsN {})                   = Nothing
  go _     (F.Forall {})                    = Nothing
  go _     (F.NextN {})                     = Nothing
  go _     (F.UntilN {})                    = Nothing
  go _     (F.Atom {})                      = Nothing
  go _     (F.Next _)                       = Nothing
  go bound (F.And phi psi)                  = And <$> go bound phi <*> go bound psi
  go bound (F.Or phi psi)                   = Or <$> go bound phi <*> go bound psi
  go bound (F.Implies phi psi)              = Implies <$> go bound phi <*> go bound psi
  go bound (F.Not phi)                      = Not <$> go bound phi
  go _     F.Bottom                         = Just Bottom
  go _     F.Top                            = Just Top
  go bound (F.PropIntBinRel rel _ lhs rhs)
    | groundTerm bound lhs && groundTerm bound rhs = Just (PropIntBinRel rel lhs rhs)
  go _     F.PropIntBinRel {}               = Nothing
  go _     (F.PropTextEq _ (TextConst c) v) = Just (PropTextEq (TextConst c) v)
  go bound (F.PropTextEq _ (TextVar x) v)
                | Set.member x bound        = Just (PropTextEq (TextVar x) v)
  go _     (F.PropTextEq _ (TextVar _) _)   = Nothing
  go bound (F.PropIntForall x phi)           = PropIntForall x   <$> go (Set.insert x bound) phi
  go bound (F.PropTextForall x phi)          = PropTextForall x  <$> go (Set.insert x bound) phi
  go bound (F.PropIntForallN x dom phi)      = PropIntForallN  x dom <$> go (Set.insert x bound) phi
  go bound (F.PropTextForallN x dom phi)     = PropTextForallN x dom <$> go (Set.insert x bound) phi
  go bound (F.PropIntExists x phi)           = PropIntExists x   <$> go (Set.insert x bound) phi
  go bound (F.PropTextExists x phi)          = PropTextExists x  <$> go (Set.insert x bound) phi
  go bound (F.PropIntExistsN x dom phi)      = PropIntExistsN  x dom <$> go (Set.insert x bound) phi
  go bound (F.PropTextExistsN x dom phi)     = PropTextExistsN x dom <$> go (Set.insert x bound) phi
  groundTerm :: Set VariableIdentifier -> IntTerm -> Bool
  groundTerm _      (IntConst _)   = True
  groundTerm bound' (IntVar _ x)   = Set.member x bound'
  groundTerm bound' (IntSum a b)   = groundTerm bound' a && groundTerm bound' b

normaliseHomogeneous :: Formula event ty -> Maybe (Formula event ty)
normaliseHomogeneous phi =
  toFormula . quote . eval <$> retract phi
