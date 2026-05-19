module Cardano.ReCon.LTL.Formula.Pretty (
    Prec(..)
  , prettyIntTerm
  , prettyTextTerm
  , prettyBinRel
  , prettyPropConstraint
  , prettyPropConstraints
  , prettyFormula) where

import           Cardano.Logging (showT)
import           Cardano.ReCon.LTL.Formula
import           Cardano.ReCon.LTL.Formula.Prec (Prec)
import qualified Cardano.ReCon.LTL.Formula.Prec as Prec

import qualified Data.Set as Set
import           Data.Text (Text, intercalate)

-- | Add parentheses when an inner precedence exceeds the outer one.
surround :: Prec -> Prec -> Text -> Text
surround outer inner str | outer <= inner = str
surround _ _ str = "(" <> str <> ")"

-- | Render an integer term.
prettyIntTerm :: IntTerm -> Text
prettyIntTerm (IntVar 1 x) = x
prettyIntTerm (IntVar k x) = showT k <> "·" <> x
prettyIntTerm (IntConst n) = showT n
prettyIntTerm (IntSum a b) = prettyIntTerm a <> " + " <> prettyIntTerm b

-- | Render a text term.
prettyTextTerm :: TextTerm -> Text
prettyTextTerm (TextVar x)   = x
prettyTextTerm (TextConst s) = showT s

-- | Render a binary relation symbol.
prettyBinRel :: BinRel -> Text
prettyBinRel Eq  = "="
prettyBinRel Lt  = "<"
prettyBinRel Lte = "≤"
prettyBinRel Gt  = ">"
prettyBinRel Gte = "≥"

-- | Render a single property constraint.
prettyPropConstraint :: PropConstraint -> Text
prettyPropConstraint (IntPropConstraint  k t) = showT k <> " = " <> prettyIntTerm t
prettyPropConstraint (TextPropConstraint k t) = showT k <> " = " <> prettyTextTerm t

-- | Render a list of property constraints.
prettyPropConstraints :: [PropConstraint] -> Text
prettyPropConstraints = intercalate ", " . fmap prettyPropConstraint

wordToXscript :: (Word -> Text) -> Word -> Text
wordToXscript f x =
  let (d, m) = x `divMod` 10
      ch = f m in
  if d == 0
  then
    ch
  else
    wordToXscript f d <> ch

wordToSuperscript :: Word -> Text
wordToSuperscript = wordToXscript superscript0to9 where
  superscript0to9 :: Word -> Text
  superscript0to9 0 = "⁰"
  superscript0to9 1 = "¹"
  superscript0to9 2 = "²"
  superscript0to9 3 = "³"
  superscript0to9 4 = "⁴"
  superscript0to9 5 = "⁵"
  superscript0to9 6 = "⁶"
  superscript0to9 7 = "⁷"
  superscript0to9 8 = "⁸"
  superscript0to9 9 = "⁹"
  superscript0to9 _ = "" -- Unexpected input

wordToSubscript :: Word -> Text
wordToSubscript = wordToXscript subscript0to9 where
  subscript0to9 :: Word -> Text
  subscript0to9 0 = "₀"
  subscript0to9 1 = "₁"
  subscript0to9 2 = "₂"
  subscript0to9 3 = "₃"
  subscript0to9 4 = "₄"
  subscript0to9 5 = "₅"
  subscript0to9 6 = "₆"
  subscript0to9 7 = "₇"
  subscript0to9 8 = "₈"
  subscript0to9 9 = "₉"
  subscript0to9 _ = "" -- Unexpected input


-- | Pretty-print a `Formula` using unicode operators.
prettyFormula :: Show a => Formula event a -> Prec -> Text
prettyFormula (Forall k phi) lvl = surround lvl Prec.Prefix $
  "☐ ᪲" <> (if k == 0 then "" else wordToSubscript k) <> " " <> prettyFormula phi Prec.Atom
prettyFormula (ForallN k phi) lvl = surround lvl Prec.Prefix $
  "☐" <> wordToSuperscript k <> " " <> prettyFormula phi Prec.Atom
prettyFormula (ExistsN k phi) lvl = surround lvl Prec.Prefix $
  "♢" <> wordToSuperscript k <> " " <> prettyFormula phi Prec.Atom
prettyFormula (Next phi) lvl = surround lvl Prec.Prefix $
  "◯" <> " " <> prettyFormula phi Prec.Atom
prettyFormula (NextN k phi) lvl = surround lvl Prec.Prefix $
  "◯" <> wordToSuperscript k <> " " <> prettyFormula phi Prec.Atom
prettyFormula (UntilN k phi psi) lvl = surround lvl Prec.Universe $
  prettyFormula phi Prec.Implies <> " " <> "|" <> wordToSuperscript k <> " " <> prettyFormula psi Prec.Implies
prettyFormula (Implies phi psi) lvl = surround lvl Prec.Implies $
  prettyFormula phi Prec.Or <> " " <> "⇒" <> " " <> prettyFormula psi Prec.Implies
prettyFormula (Or phi psi) lvl = surround lvl Prec.Or $
  prettyFormula phi Prec.Or <> " " <> "∨" <> " " <> prettyFormula psi Prec.Or
prettyFormula (And phi psi) lvl = surround lvl Prec.And $
  prettyFormula phi Prec.And <> " " <> "∧" <> " " <> prettyFormula psi Prec.And
prettyFormula (Not phi) lvl = surround lvl Prec.Prefix $
  "¬ " <> prettyFormula phi Prec.Atom
prettyFormula Top lvl = surround lvl Prec.Atom "⊤"
prettyFormula Bottom lvl = surround lvl Prec.Atom "⊥"
prettyFormula (PropIntForall x phi) lvl = surround lvl Prec.Universe $
  "∀" <> x <> " ∈ ℤ. " <> prettyFormula phi Prec.Universe
prettyFormula (PropTextForall x phi) lvl = surround lvl Prec.Universe $
  "∀" <> x <> " ∈ Text. " <> prettyFormula phi Prec.Universe
prettyFormula (PropIntForallN x dom phi) lvl = surround lvl Prec.Universe $
  "∀" <> x <> " ∈ "
      <> "{" <> intercalate ", " (fmap showT (Set.toList dom)) <> "}"
      <> ". " <> prettyFormula phi Prec.Universe
prettyFormula (PropTextForallN x dom phi) lvl = surround lvl Prec.Universe $
  "∀" <> x <> " ∈ "
      <> "{" <> intercalate ", " (fmap showT (Set.toList dom)) <> "}"
      <> ". " <> prettyFormula phi Prec.Universe
prettyFormula (PropIntExists x phi) lvl = surround lvl Prec.Universe $
  "∃" <> x <> " ∈ ℤ. " <> prettyFormula phi Prec.Universe
prettyFormula (PropTextExists x phi) lvl = surround lvl Prec.Universe $
  "∃" <> x <> " ∈ Text. " <> prettyFormula phi Prec.Universe
prettyFormula (PropIntExistsN x dom phi) lvl = surround lvl Prec.Universe $
  "∃" <> x <> " ∈ "
      <> "{" <> intercalate ", " (fmap showT (Set.toList dom)) <> "}"
      <> ". " <> prettyFormula phi Prec.Universe
prettyFormula (PropTextExistsN x dom phi) lvl = surround lvl Prec.Universe $
  "∃" <> x <> " ∈ "
      <> "{" <> intercalate ", " (fmap showT (Set.toList dom)) <> "}"
      <> ". " <> prettyFormula phi Prec.Universe
prettyFormula (Atom c is) lvl = surround lvl Prec.Atom $
  showT c <> "(" <> prettyPropConstraints (Set.toList is) <> ")"
prettyFormula (PropIntBinRel rel _ lhs rhs) lvl = surround lvl Prec.Eq $
  prettyIntTerm lhs <> " " <> prettyBinRel rel <> " " <> prettyIntTerm rhs
prettyFormula (PropTextEq _ t v) lvl = surround lvl Prec.Eq $
  prettyTextTerm t <> " = " <> showT v
