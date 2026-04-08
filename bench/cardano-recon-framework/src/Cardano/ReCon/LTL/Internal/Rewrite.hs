module Cardano.ReCon.LTL.Internal.Rewrite(
  rewriteHomogeneous,
  rewriteIdentity
  ) where

import           Cardano.ReCon.LTL.Formula
import           Cardano.ReCon.LTL.Internal.IR.HomogeneousFormula (normaliseHomogeneous)

import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set

-- This file concerns applying rewrite rules to a formula.
-- The rewrite rules must be logical identities, hence all rewrites here produce logically equivalent formulas.

-- | Call the given function on all sub-expressions of the formula recursively
--   up to any temporal operator (= heterogeneous fragment), exclusively.
recurseHomogeneous :: (Formula event ty -> Maybe (Formula event ty)) -> Formula event ty -> Formula event ty
recurseHomogeneous _ self@(Atom {})     = self
recurseHomogeneous _ self@(Forall {})   = self
recurseHomogeneous _ self@(ExistsN {})  = self
recurseHomogeneous _ self@(ForallN {})  = self
recurseHomogeneous _ self@(UntilN {})   = self
recurseHomogeneous _ self@(NextN {})    = self
recurseHomogeneous _ self@(Next _)      = self
recurseHomogeneous f self@(And phi psi) =
  fromMaybe (And (recurseHomogeneous f phi) (recurseHomogeneous f psi)) (f self)
recurseHomogeneous f self@(Or phi psi) =
  fromMaybe (Or (recurseHomogeneous f phi) (recurseHomogeneous f psi)) (f self)
recurseHomogeneous f self@(Implies phi psi) =
  fromMaybe (Implies (recurseHomogeneous f phi) (recurseHomogeneous f psi)) (f self)
recurseHomogeneous f self@(Not phi) =
  fromMaybe (Not (recurseHomogeneous f phi)) (f self)
recurseHomogeneous f self@Bottom                       = fromMaybe self (f self)
recurseHomogeneous f self@Top                          = fromMaybe self (f self)
recurseHomogeneous f self@(PropIntBinRel {})           = fromMaybe self (f self)
recurseHomogeneous f self@(PropTextEq {})              = fromMaybe self (f self)
recurseHomogeneous f self@(PropIntForall x phi)        = fromMaybe (PropIntForall x (recurseHomogeneous f phi)) (f self)
recurseHomogeneous f self@(PropTextForall x phi)       = fromMaybe (PropTextForall x (recurseHomogeneous f phi)) (f self)
recurseHomogeneous f self@(PropIntForallN x dom phi)   = fromMaybe (PropIntForallN x dom (recurseHomogeneous f phi)) (f self)
recurseHomogeneous f self@(PropTextForallN x dom phi)  = fromMaybe (PropTextForallN x dom (recurseHomogeneous f phi)) (f self)
recurseHomogeneous f self@(PropIntExists x phi)        = fromMaybe (PropIntExists x (recurseHomogeneous f phi)) (f self)
recurseHomogeneous f self@(PropTextExists x phi)       = fromMaybe (PropTextExists x (recurseHomogeneous f phi)) (f self)
recurseHomogeneous f self@(PropIntExistsN x dom phi)   = fromMaybe (PropIntExistsN x dom (recurseHomogeneous f phi)) (f self)
recurseHomogeneous f self@(PropTextExistsN x dom phi)  = fromMaybe (PropTextExistsN x dom (recurseHomogeneous f phi)) (f self)

-- | Rewrite the formula by applying the homogeneous fragment retraction & normalisation recursively.
rewriteHomogeneous :: Formula event ty -> Formula event ty
rewriteHomogeneous = recurseHomogeneous normaliseHomogeneous

-- | Rewrites the formula by the following logical identities recursively:
--   ☐ ᪲ₖ ⊤ = ⊤
--   ☐ᵏ ⊤ = ⊤
--   ♢ᵏ ⊥ = ⊥
--   φ |ᵏ ⊤ = ⊤
--   φ ∧ ⊤ = φ
--   ⊤ ∧ φ = φ
--   φ ∧ ⊥ = ⊥
--   ⊥ ∧ φ = ⊥
--   ⊤ ∨ φ = ⊤
--   φ ∨ ⊤ = ⊤
--   ⊥ ∨ φ = φ
--   φ ∨ ⊥ = φ
--   ⊤ ⇒ φ = φ
--   ⊥ ⇒ φ = ⊤
--   φ ⇒ ⊤ = ⊤
--   φ ⇒ ⊥ = ¬ φ
--   ¬ (¬ φ) = φ
--   ¬ ⊥ = ⊤
--   ¬ ⊤ = ⊥
--   (t = t) = ⊤
--   (t ≤ t) = ⊤
--   (t ≥ t) = ⊤
--   (t < t) = ⊥
--   (t > t) = ⊥
--   (n = m) = ⊤ where n = m   (n, m integer constants)
--   (n = m) = ⊥ where n ≠ m
--   (n < m) = ⊤ where n < m
--   (n < m) = ⊥ where n ≥ m
--   (n ≤ m) = ⊤ where n ≤ m
--   (n ≤ m) = ⊥ where n > m
--   (n > m) = ⊤ where n > m
--   (n > m) = ⊥ where n ≤ m
--   (n ≥ m) = ⊤ where n ≥ m
--   (n ≥ m) = ⊥ where n < m
--   ∀x. ⊤ = ⊤
--   ∀x. ⊥ = ⊥
--   ∃x. ⊤ = ⊤
--   ∃x. ⊥ = ⊥
--   ∀(x ∈ ∅). φ       = ⊤
--   ∀(x ∈ {v} ⊔ v̄). ⊥ = ⊥
--   ∀(x ∈ {v} ⊔ v̄). ⊤ = ⊤
--   ∃(x ∈ ∅). φ       = ⊥
--   ∃(x ∈ {v} ⊔ v̄). ⊥ = ⊥
--   ∃(x ∈ {v} ⊔ v̄). ⊤ = ⊤
--   Additionally, unfolds base-cases of finite temporal operators.
rewriteIdentity :: Eq ty => Formula event ty -> Formula event ty
rewriteIdentity (Forall k phi) =
  case rewriteIdentity phi of
    Top  -> Top
    phi' -> Forall k phi'
rewriteIdentity (ForallN 0 phi) = rewriteIdentity (unfoldForallN 0 phi)
rewriteIdentity (ForallN k phi) =
  case rewriteIdentity phi of
    Top  -> Top
    phi' -> ForallN k phi'
rewriteIdentity (ExistsN 0 phi) = rewriteIdentity (unfoldExistsN 0 phi)
rewriteIdentity (ExistsN k phi) =
  case rewriteIdentity phi of
    Bottom -> Bottom
    phi'   -> ExistsN k phi'
rewriteIdentity (Next phi) = Next (rewriteIdentity phi)
rewriteIdentity (NextN 0 phi) = rewriteIdentity (unfoldNextN 0 phi)
rewriteIdentity (NextN k phi) = NextN k (rewriteIdentity phi)
rewriteIdentity (UntilN 0 phi psi) = rewriteIdentity (unfoldUntilN 0 phi psi)
rewriteIdentity (UntilN k phi psi) =
  case rewriteIdentity psi of
    Top -> Top
    psi' -> UntilN k (rewriteIdentity phi) psi'
rewriteIdentity (And phi psi) =
  case (rewriteIdentity phi, rewriteIdentity psi) of
    (Bottom, _)   -> Bottom
    (Top, psi')   -> psi'
    (_, Bottom)   -> Bottom
    (phi', Top)   -> phi'
    (phi', psi')  -> And phi' psi'
rewriteIdentity (Or phi psi) =
  case (rewriteIdentity phi, rewriteIdentity psi) of
    (Bottom, psi') -> psi'
    (Top, _)       -> Top
    (phi', Bottom) -> phi'
    (_, Top)       -> Top
    (phi', psi')   -> Or phi' psi'
rewriteIdentity (Implies phi psi) =
  case (rewriteIdentity phi, rewriteIdentity psi) of
    (Top, psi')     -> psi'
    (Bottom, _)     -> Top
    (_, Top)        -> Top
    (phi', Bottom)  -> rewriteIdentity (Not phi')
    (phi', psi')    -> Implies phi' psi'
rewriteIdentity (Not phi) =
  case rewriteIdentity phi of
    Not phi' -> phi'
    Top      -> Bottom
    Bottom   -> Top
    phi'      -> Not phi'
rewriteIdentity Bottom = Bottom
rewriteIdentity Top = Top
rewriteIdentity (PropIntBinRel rel _ lhs rhs) | lhs == rhs =
  case rel of
    Eq  -> Top
    Lte -> Top
    Gte -> Top
    Lt  -> Bottom
    Gt  -> Bottom
rewriteIdentity (PropIntBinRel rel _ (IntConst a) (IntConst b)) =
  case rel of
    Eq  -> if a == b  then Top else Bottom
    Lt  -> if a <  b  then Top else Bottom
    Lte -> if a <= b  then Top else Bottom
    Gt  -> if a >  b  then Top else Bottom
    Gte -> if a >= b  then Top else Bottom
rewriteIdentity p@(PropIntBinRel {}) = p
rewriteIdentity (PropTextEq _ (TextConst v) v') | v == v' = Top
rewriteIdentity (PropTextEq _ (TextConst _) _)            = Bottom
rewriteIdentity p@(PropTextEq {}) = p
rewriteIdentity p@(Atom {}) = p
rewriteIdentity (PropIntForall x phi) =
  case rewriteIdentity phi of
    Top    -> Top
    Bottom -> Bottom
    phi'   -> PropIntForall x phi'
rewriteIdentity (PropTextForall x phi) =
  case rewriteIdentity phi of
    Top    -> Top
    Bottom -> Bottom
    phi'   -> PropTextForall x phi'
rewriteIdentity (PropIntExists x phi) =
  case rewriteIdentity phi of
    Top    -> Top
    Bottom -> Bottom
    phi'   -> PropIntExists x phi'
rewriteIdentity (PropTextExists x phi) =
  case rewriteIdentity phi of
    Top    -> Top
    Bottom -> Bottom
    phi'   -> PropTextExists x phi'
rewriteIdentity (PropIntForallN  _ dom _) | Set.null dom = Top
rewriteIdentity (PropIntForallN  x dom phi) =
  case rewriteIdentity phi of
    Top    -> Top
    Bottom -> Bottom
    phi'   -> PropIntForallN x dom phi'
rewriteIdentity (PropTextForallN _ dom _) | Set.null dom = Top
rewriteIdentity (PropTextForallN x dom phi) =
  case rewriteIdentity phi of
    Top    -> Top
    Bottom -> Bottom
    phi'   -> PropTextForallN x dom phi'
rewriteIdentity (PropIntExistsN  _ dom _) | Set.null dom = Bottom
rewriteIdentity (PropIntExistsN  x dom phi) =
  case rewriteIdentity phi of
    Top    -> Top
    Bottom -> Bottom
    phi'   -> PropIntExistsN x dom phi'
rewriteIdentity (PropTextExistsN _ dom _) | Set.null dom = Bottom
rewriteIdentity (PropTextExistsN x dom phi) =
  case rewriteIdentity phi of
    Top    -> Top
    Bottom -> Bottom
    phi'   -> PropTextExistsN x dom phi'
