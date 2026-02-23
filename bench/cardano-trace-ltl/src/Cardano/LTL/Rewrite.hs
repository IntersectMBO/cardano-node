{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE CPP #-}
module Cardano.LTL.Rewrite(
  rewriteHomogeneous,
  rewriteFragment,
  rewriteIdentity
  ) where

import           Cardano.LTL.Lang.Formula
import           Cardano.LTL.Lang.Fragment           (findAtoms,
                                                      normaliseFragment)
import           Cardano.LTL.Lang.HomogeneousFormula (normaliseHomogeneous)
import Data.Maybe (fromMaybe)
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
recurseHomogeneous f self@Bottom                  = fromMaybe self (f self)
recurseHomogeneous f self@Top                     = fromMaybe self (f self)
recurseHomogeneous f self@(PropEq {})             = fromMaybe self (f self)
recurseHomogeneous f self@(PropForall x phi)      = fromMaybe (PropForall x (recurseHomogeneous f phi)) (f self)
recurseHomogeneous f self@(PropForallN x dom phi) = fromMaybe (PropForallN x dom (recurseHomogeneous f phi)) (f self)

-- | Rewrite the formula by applying the fragment retraction & normalisation recursively.
rewriteFragment :: (Ord event, Ord ty) => Formula event ty -> Formula event ty
rewriteFragment phi = recurseHomogeneous (normaliseFragment (findAtoms phi mempty)) phi

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
--   (v = v') = ⊤ where v = v'
--   (v = v') = ⊥ where v ≠ v'
--   ∀x. ⊤ = ⊤
--   ∀x. ⊥ = ⊥
--   ∀(x ∈ ∅). φ       = ⊤
--   ∀(x ∈ {v} ⊔ v̄). ⊥ = ⊥
--   ∀(x ∈ {v} ⊔ v̄). ⊤ = ⊤
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
rewriteIdentity (PropEq _ (Const v) v') | v == v' = Top
rewriteIdentity (PropEq _ (Const _) _) = Bottom
rewriteIdentity p@(PropEq {}) = p
rewriteIdentity p@(Atom {}) = p
rewriteIdentity (PropForall x phi) =
  case rewriteIdentity phi of
    Top    -> Top
    Bottom -> Bottom
    phi'    -> PropForall x phi'
rewriteIdentity (PropForallN _ dom _) | Set.null dom = Top
rewriteIdentity (PropForallN x dom phi) =
  case rewriteIdentity phi of
    Top -> Top
    Bottom -> Bottom
    phi' -> PropForallN x dom phi'
