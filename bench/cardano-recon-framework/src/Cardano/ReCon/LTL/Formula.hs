module Cardano.ReCon.LTL.Formula (
    PropName
  , VariableIdentifier
  , IntValue
  , TextValue
  , IntTerm(..)
  , TextTerm(..)
  , BinRel(..)
  , PropConstraint(..)
  , Formula(..)
  , unfoldForall
  , unfoldForallN
  , unfoldExistsN
  , unfoldNextN
  , unfoldUntilN
  , relevance
  , Relevance
  , and
  , interpTimeunit
  , Event(..)) where

import           Prelude hiding (and)

import           Cardano.ReCon.Common.Types (BinRel (..), IntValue, VariableIdentifier)
import           Cardano.ReCon.Integer.Polynomial.Term (IntTerm (..))

import           Data.Map.Strict (Map)
import           Data.Set (Set, union)
import           Data.Text (Text)
import           Data.Word (Word64)

-- | A property name (e.g. "thread", "node", etc.).
type PropName = Text

-- | Text event property value.
type TextValue = Text

-- | Text term: a constant string or a variable ranging over strings.
data TextTerm = TextConst TextValue | TextVar VariableIdentifier deriving (Show, Eq, Ord)

-- | Default name: c.
--   A constraint inside an `Atom`, matching an event property against a term.
data PropConstraint
  = IntPropConstraint  PropName IntTerm
  | TextPropConstraint PropName TextTerm
  deriving (Show, Eq, Ord)

-- | Set of relevant events.
type Relevance event ty = Set (event, ty)

-- | Default name: φ.
--
--   Formulas of a discrete-time Linear Temporal Logic (LTL) enriched with
--   first-order quantification over event properties.
--
--   __Time model.__
--   Time is a sequence of abstract /units/ indexed by natural numbers.
--   Each unit carries a finite multiset of typed events (see 'Event').
--   The temporal connectives advance through this sequence:
--
--   * @◯ φ@ — next unit
--   * @☐ ᪲ₖ φ@ — every @(k+1)@-th unit from now, forever
--   * @☐ⁿ φ@ — for each of the next @n@ units
--   * @♢ⁿ φ@ — for some unit within the next @n@
--   * @φ |ⁿ ψ@ — @φ@ until @ψ@, within @n@ units
--
--   __Parameterisation.__
--   The type parameter @ty@ is the vocabulary of /event types/; @event@ is the
--   concrete event representation (see 'Event').  Atoms (@'Atom' ty cs@) assert
--   that the current unit contains an event of type @ty@ whose properties
--   satisfy the constraints @cs@.
--
--   __First-order layer.__
--   Quantifiers @∀x ∈ ℤ@, @∃x ∈ ℤ@, @∀x ∈ Text@, @∃x ∈ Text@, and their
--   finite-domain variants, bind variables that can appear in property
--   constraints, integer comparison atoms (@'PropIntBinRel'@), and text
--   equality atoms (@'PropTextEq'@).  The resulting fragment is decidable:
--   integer arithmetic is handled by Cooper's quantifier elimination;
--   text quantifiers are decided by finite enumeration (see
--   "Cardano.ReCon.LTL.Internal.IR.HomogeneousFormula.FinFree").
data Formula event ty =
   ------------ Temporal -------------
     -- | ☐ ᪲ₖ φ ≡ φ ∧ ◯ (◯ᵏ (☐ ᪲ₖ))
     --   For every (k+1)-th unit of time from now, φ
     --   For example:
     --     ☐ ᪲₁ φ means for every other unit of time from now, φ
     --     ☐ ᪲₃ φ means for every 4-th unit of time from now, φ
     --     ☐ ᪲₀ φ means for every unit of time from now, φ
     Forall Word (Formula event ty)
     -- | ☐ⁿ φ
     --   ☐⁰ φ ≡ ⊤
     --   ☐¹⁺ᵏ φ ≡ φ ∧ ◯ (☐ᵏ φ)
     --   For each of the n units of time from now, φ
   | ForallN Word (Formula event ty)
     -- | ♢ⁿ φ
     --   ♢⁰ φ ≡ ⊥
     --   ♢¹⁺ᵏ φ ≡ φ ∨ ◯ (♢ᵏ φ)
     --   For one of the n units of time from now, φ
   | ExistsN Word (Formula event ty)
     -- | ◯ φ
     --   For the next unit of time from now, φ.
   | Next (Formula event ty)
     -- | ◯ⁿ φ
     --   ◯⁰ φ ≡ φ
     --   ◯¹⁺ᵏ φ ≡ ◯ (◯ᵏ φ)
     --   For the n-th unit of time from now, φ
   | NextN Word (Formula event ty)
     -- | φ |ⁿ ψ
     --   φ |⁰ ψ ≡ ⊤
     --   φ |¹⁺ᵏ ψ ≡ ψ ∨ ¬ ψ ∧ φ ∧ (φ |ᵏ ψ)
     --   φ until ψ in the n units of time from now
   | UntilN Word (Formula event ty) (Formula event ty)
     -- | ty c̄
   | Atom ty (Set PropConstraint)
   -------------------------------------


   ------------ Connective -------------
     -- | φ ∨ ψ
   | Or (Formula event ty) (Formula event ty)
     -- | φ ∧ ψ
   | And (Formula event ty) (Formula event ty)
     -- | ¬ φ
   | Not (Formula event ty)
     -- | φ ⇒ ψ
   | Implies (Formula event ty) (Formula event ty)
     -- | T
   | Top
     -- | ⊥
   | Bottom
   -------------------------------------


   ----------- Event property ----------
     -- | ∀x ∈ ℤ. φ  —  x ranges over all integers
   | PropIntForall  VariableIdentifier (Formula event ty)
     -- | ∀x ∈ Text. φ  —  x ranges over all strings
   | PropTextForall VariableIdentifier (Formula event ty)
     -- | ∀x ∈ v̄. φ  —  x ranges over the given integers
   | PropIntForallN  VariableIdentifier (Set IntValue)  (Formula event ty)
     -- | ∀x ∈ v̄. φ  —  x ranges over the given strings
   | PropTextForallN VariableIdentifier (Set TextValue) (Formula event ty)
     -- | ∃x ∈ ℤ. φ  —  x ranges over all integers
   | PropIntExists  VariableIdentifier (Formula event ty)
     -- | ∃x ∈ Text. φ  —  x ranges over all strings
   | PropTextExists VariableIdentifier (Formula event ty)
     -- | ∃x ∈ v̄. φ  —  x ranges over the given integers
   | PropIntExistsN  VariableIdentifier (Set IntValue)  (Formula event ty)
     -- | ∃x ∈ v̄. φ  —  x ranges over the given strings
   | PropTextExistsN VariableIdentifier (Set TextValue) (Formula event ty)
     -- | t rel t  (integer)
   | PropIntBinRel BinRel (Relevance event ty) IntTerm  IntTerm
     -- | t = v  (text)
   | PropTextEq (Relevance event ty) TextTerm TextValue
   -------------------------------------
   deriving (Show, Eq, Ord)


-- | Compute the total `Relevance` of the formula.
relevance :: (Ord event, Ord ty) => Formula event ty -> Relevance event ty
relevance = go mempty where
  go :: (Ord event, Ord ty) => Relevance event ty -> Formula event ty -> Relevance event ty
  go acc (Forall _ phi)          = go acc phi
  go acc (ForallN _ phi)         = go acc phi
  go acc (ExistsN _ phi)         = go acc phi
  go acc (Next phi)              = go acc phi
  go acc (NextN _ phi)           = go acc phi
  go acc (UntilN _ phi psi)      = go (go acc phi) psi
  go acc (Or phi psi)            = go (go acc phi) psi
  go acc (And phi psi)           = go (go acc phi) psi
  go acc (Not phi)               = go acc phi
  go acc (Implies phi psi)       = go (go acc phi) psi
  go acc Top                     = acc
  go acc Bottom                  = acc
  go acc (Atom {})               = acc
  go acc (PropIntForall _ phi)   = go acc phi
  go acc (PropTextForall _ phi)  = go acc phi
  go acc (PropIntForallN _ _ phi)  = go acc phi
  go acc (PropTextForallN _ _ phi) = go acc phi
  go acc (PropIntExists _ phi)   = go acc phi
  go acc (PropTextExists _ phi)  = go acc phi
  go acc (PropIntExistsN _ _ phi)  = go acc phi
  go acc (PropTextExistsN _ _ phi) = go acc phi
  go acc (PropIntBinRel _ rel _ _) = rel `union` acc
  go acc (PropTextEq rel _ _)      = rel `union` acc

unfoldForall :: Word -> Formula event ty -> Formula event ty
unfoldForall k phi = And phi (Next (NextN k (Forall k phi)))

unfoldForallN :: Word -> Formula event ty -> Formula event ty
unfoldForallN 0 _   = Top
unfoldForallN k phi = And phi (Next (ForallN (k - 1) phi))

unfoldExistsN :: Word -> Formula event ty -> Formula event ty
unfoldExistsN 0 _   = Bottom
unfoldExistsN k phi = Or phi (Next (ExistsN (k - 1) phi))

unfoldNextN :: Word -> Formula event ty -> Formula event ty
unfoldNextN 0 phi = phi
unfoldNextN k phi = Next (NextN (k - 1) phi)

unfoldUntilN :: Word -> Formula event ty -> Formula event ty -> Formula event ty
unfoldUntilN 0 _ _ = Top
unfoldUntilN k phi psi =
  Or psi
     (And
       phi
       (And
         (Not psi)
         (Next (UntilN (k - 1) phi psi))
       )
     )

and :: [Formula event ty] -> Formula event ty
and []           = Top
and [phi]        = phi
and (phi : phis) = And phi (and phis)

-- | It's useful to express temporal aspect of the formulas in a familiar time unit (e.g milliseconds).
--   Yet, the LTL machinery works with nameless abstract time units.
--   This function can be used to convert one into the other.
interpTimeunit :: (Word -> Word) -> Formula event ty -> Formula event ty
interpTimeunit f (Forall k phi)   = Forall (f k) (interpTimeunit f phi)
interpTimeunit f (ForallN k phi)  = ForallN (f k) (interpTimeunit f phi)
interpTimeunit f (ExistsN k phi)  = ExistsN (f k) (interpTimeunit f phi)
interpTimeunit f (Next phi)       = Next (interpTimeunit f phi)
interpTimeunit f (NextN k phi)    = NextN (f k) (interpTimeunit f phi)
interpTimeunit f (UntilN k phi psi) = UntilN (f k) (interpTimeunit f phi) (interpTimeunit f psi)
interpTimeunit f (Not phi)        = Not (interpTimeunit f phi)
interpTimeunit f (Or phi psi)     = Or (interpTimeunit f phi) (interpTimeunit f psi)
interpTimeunit f (And phi psi)    = And (interpTimeunit f phi) (interpTimeunit f psi)
interpTimeunit f (Implies phi psi) = Implies (interpTimeunit f phi) (interpTimeunit f psi)
interpTimeunit _ Top              = Top
interpTimeunit _ Bottom           = Bottom
interpTimeunit _ phi@Atom{}       = phi
interpTimeunit _ phi@PropIntBinRel{} = phi
interpTimeunit _ phi@PropTextEq{}    = phi
interpTimeunit f (PropIntForall x phi)    = PropIntForall x (interpTimeunit f phi)
interpTimeunit f (PropTextForall x phi)   = PropTextForall x (interpTimeunit f phi)
interpTimeunit f (PropIntForallN x dom phi)  = PropIntForallN x dom (interpTimeunit f phi)
interpTimeunit f (PropTextForallN x dom phi) = PropTextForallN x dom (interpTimeunit f phi)
interpTimeunit f (PropIntExists x phi)    = PropIntExists x (interpTimeunit f phi)
interpTimeunit f (PropTextExists x phi)   = PropTextExists x (interpTimeunit f phi)
interpTimeunit f (PropIntExistsN x dom phi)  = PropIntExistsN x dom (interpTimeunit f phi)
interpTimeunit f (PropTextExistsN x dom phi) = PropTextExistsN x dom (interpTimeunit f phi)

-- | Default name: e.
--
--   Interface between a concrete event representation @a@ and the LTL
--   evaluator.  The functional dependency @a -> ty@ fixes the event-type
--   vocabulary once the event type is known.
--
--   __Semantics.__
--   Each time unit presents a sequence of events to the evaluator.  An
--   @'Atom' ty cs@ is satisfied by unit @t@ if there exists an event @e@ in
--   @t@ such that:
--
--   1. @'ofTy' e ty@ holds, and
--   2. every constraint @c ∈ cs@ matches @e@ against @'intProps' e ty@ or
--      @'textProps' e ty@ (see @satisfiability-semantics.txt@).
--
--   __Precondition.__
--   @'intProps'@ and @'textProps'@ are only called when @'ofTy' e ty@ is
--   @True@; their behaviour is unspecified otherwise.
class Event a ty | a -> ty where
  -- | @'ofTy' e ty@ — is event @e@ of type @ty@?
  ofTy :: a -> ty -> Bool
  -- | Integer-valued properties of @e@ relevant to type @ty@,
  --   keyed by property name.  Precondition: @'ofTy' e ty@.
  intProps  :: a -> ty -> Map VariableIdentifier IntValue
  -- | Text-valued properties of @e@ relevant to type @ty@,
  --   keyed by property name.  Precondition: @'ofTy' e ty@.
  textProps :: a -> ty -> Map VariableIdentifier TextValue
  -- | Timestamp of the event in microseconds.  Used for diagnostics only;
  --   the LTL evaluator does not reason about wall-clock time.
  beg :: a -> Word64
