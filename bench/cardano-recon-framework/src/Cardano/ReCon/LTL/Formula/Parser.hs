{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Use newtype instead of data" -}
module Cardano.ReCon.LTL.Formula.Parser (Parser, Context(..), Domain(..), PropKind(..), text, name, formula) where


import           Cardano.ReCon.Common.Parser
import           Cardano.ReCon.Common.Types
import           Cardano.ReCon.Integer.Polynomial.Parser (intTerm)
import           Cardano.ReCon.LTL.Formula

import           Prelude hiding (head)

import           Control.Monad (guard)
import           Data.Char (isAlpha, isAlphaNum)
import           Data.Foldable (asum)
import           Data.Functor (void, (<&>))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           GHC.Unicode (isControl, isSpace)
import           Text.Megaparsec
import           Text.Megaparsec.Char (char, space, string)

-- | The kind of a property variable: integer or text.
data PropKind = IntKind | TextKind deriving (Show, Eq, Ord)

-- | A typed domain for a property variable.
data Domain = IntDomain (Set IntValue) | TextDomain (Set Text) deriving (Show, Eq, Ord)

data Context = Context {
  interpDomain :: [(Text, Domain)],
  -- | Kinds of in-scope property variables, populated by quantifier parsers.
  varKinds     :: Map VariableIdentifier PropKind
}

isSubscriptDigit :: Char -> Bool
isSubscriptDigit c = c >= '₀' && c <= '₉'

unescapedVariableIdentifierNextChar :: Parser Char
unescapedVariableIdentifierNextChar = satisfy (\x -> isAlphaNum x || x == '_' || isSubscriptDigit x)

unescapedVariableIdentifier :: Parser Text
unescapedVariableIdentifier =
  Text.pack <$> ((:) <$> firstChar <*> many unescapedVariableIdentifierNextChar) <?> "identifier" where
  firstChar :: Parser Char
  firstChar = satisfy (\x -> isAlpha x || x == '_')

superscriptDigit :: Parser Word
superscriptDigit = 0 <$ char '⁰'
               <|> 1 <$ char '¹'
               <|> 2 <$ char '²'
               <|> 3 <$ char '³'
               <|> 4 <$ char '⁴'
               <|> 5 <$ char '⁵'
               <|> 6 <$ char '⁶'
               <|> 7 <$ char '⁷'
               <|> 8 <$ char '⁸'
               <|> 9 <$ char '⁹'

subscriptDigit :: Parser Word
subscriptDigit = 0 <$ char '₀'
             <|> 1 <$ char '₁'
             <|> 2 <$ char '₂'
             <|> 3 <$ char '₃'
             <|> 4 <$ char '₄'
             <|> 5 <$ char '₅'
             <|> 6 <$ char '₆'
             <|> 7 <$ char '₇'
             <|> 8 <$ char '₈'
             <|> 9 <$ char '₉'

littleEndian :: Word -> [Word] -> Word
littleEndian radix = go 0 1 where
  go :: Word -> Word -> [Word] -> Word
  go acc _ []            = acc
  go acc factor (d : ds) = go (acc + d * factor) (radix * factor) ds

bigEndian :: Word -> [Word] -> Word
bigEndian radix = littleEndian radix . reverse

superscriptWord :: Parser Word
superscriptWord = bigEndian 10 <$> some superscriptDigit

subscriptWord :: Parser Word
subscriptWord = bigEndian 10 <$> some subscriptDigit

variableIdentifier :: Parser Text
variableIdentifier = unescapedVariableIdentifier

text :: Parser Text
text = Text.pack <$> (char '\"' *> many one) <* char '\"' where
  one :: Parser Char
  one = satisfy (\x -> not (isControl x) && (not (isSpace x) || x == ' ') && x /= '"')

-- | Unquoted name: [_a-zA-Z][_a-zA-Z0-9._]*  — for namespaces and property keys.
unquotedName :: Parser Text
unquotedName =
  Text.pack <$> ((:) <$> firstChar <*> many nextChar) <?> "name"
  where
    firstChar = satisfy (\c -> isAlpha c || c == '_')
    nextChar  = satisfy (\c -> isAlphaNum c || c == '_' || c == '.')

-- | A name is either an unquoted identifier or a quoted string.
name :: Parser Text
name = try unquotedName <|> text

formulaBottom :: Parser (Formula event ty)
formulaBottom = Bottom <$ string "⊥"

formulaTop :: Parser (Formula event ty)
formulaTop = Top <$ string "⊤"

-- | Parse a single property constraint inside an Atom: "propname" = term.
--   The term kind is determined by the RHS: int literal → IntPropConstraint,
--   text literal → TextPropConstraint, variable → look up kind in ctx.varKinds.
constraint :: Context -> Parser PropConstraint
constraint ctx = do
  propName <- name <* space <* char '='
  space
  (IntPropConstraint  propName . IntConst <$> try parseIntValue)
    <|> (TextPropConstraint propName . TextConst <$> text)
    <|> do
          var <- variableIdentifier
          case Map.lookup var ctx.varKinds of
            Just IntKind  -> pure $ IntPropConstraint  propName (IntVar 1 var)
            Just TextKind -> pure $ TextPropConstraint propName (TextVar var)
            Nothing       -> fail $ "Unknown variable kind for: " <> Text.unpack var

constraints :: Context -> Parser (Set PropConstraint)
constraints ctx = Set.fromList <$> (char '{' *> space *> sepBy (constraint ctx) (space *> char ',' <* space) <* space <* char '}')

formulaInParens :: Context -> Parser ty -> Parser (Formula event ty)
formulaInParens ctx ty = char '(' *> space *> formulaUniverse ctx ty <* space <* char ')'

formulaPropAtom :: Context -> Parser ty -> Parser (Formula event ty)
formulaPropAtom ctx ty = Atom <$> ty <*> (space *> constraints ctx)

formulaAtom :: Context -> Parser ty -> Parser (Formula event ty)
formulaAtom ctx ty = formulaBottom <|> formulaTop <|> formulaInParens ctx ty <|> formulaPropAtom ctx ty

formulaNext :: Context -> Parser ty -> Parser (Formula event ty)
formulaNext ctx ty = Next <$> (string "◯" *> space *> formulaAtom ctx ty)

formulaNextN :: Context -> Parser ty -> Parser (Formula event ty)
formulaNextN ctx ty = NextN <$> (try (string "◯" *> superscriptWord) <* space) <*> formulaAtom ctx ty

formulaExistsN :: Context -> Parser ty -> Parser (Formula event ty)
formulaExistsN ctx ty = ExistsN <$> (string "♢" *> superscriptWord <* space) <*> formulaAtom ctx ty

formulaForallN :: Context -> Parser ty -> Parser (Formula event ty)
formulaForallN ctx ty = ForallN <$> (string "☐" *> superscriptWord <* space) <*> formulaAtom ctx ty

formulaForall :: Context -> Parser ty -> Parser (Formula event ty)
formulaForall ctx ty = Forall <$> (string "☐ ᪲" *> option 0 subscriptWord <* space) <*> formulaAtom ctx ty

formulaNot :: Context -> Parser ty -> Parser (Formula event ty)
formulaNot ctx ty = Not <$> (string "¬" *> space *> formulaAtom ctx ty)

-- | Set of int values from a literal {1, 2, 3}.
setIntConst :: Parser (Set IntValue)
setIntConst = Set.fromList <$> (char '{' *> space *> sepBy parseIntValue (space *> char ',' <* space) <* space <* char '}')

-- | Set of text values from a literal {"a", "b"}.
setTextConst :: Parser (Set Text)
setTextConst = Set.fromList <$> (char '{' *> space *> sepBy text (space *> char ',' <* space) <* space <* char '}')

-- | A domain literal: try int first (so {1,2,3} is Int, not Text), then text.
setDomainConst :: Parser Domain
setDomainConst = (IntDomain <$> try setIntConst) <|> (TextDomain <$> setTextConst)

-- | A domain variable resolved from the context (prefixed by $).
setDomainVar :: Context -> Parser Domain
setDomainVar ctx = do
  void $ string "$"
  asum (ctx.interpDomain <&> \(k, v) -> v <$ string k)

setDomain :: Context -> Parser Domain
setDomain ctx = setDomainVar ctx <|> setDomainConst

-- | Parse an integer or text binary-relation atom.
--   LHS may be an int literal or a variable identifier; RHS likewise.
--   Text equality (x = "s") is the only form that produces PropTextEq.
formulaEq :: Context -> Parser ty -> Parser (Formula event ty)
formulaEq ctx ty =
  try (do
    lhs <- Left <$> try parseIntValue <|> Right <$> variableIdentifier
    space
    rel <- parseBinRel
    space
    case lhs of
      Right x ->
              PropIntBinRel rel Set.empty (IntVar 1 x) <$> try intTerm
          <|> guard (rel == Eq) *> (PropTextEq Set.empty (TextVar x) <$> text)
      Left v ->
        PropIntBinRel rel Set.empty (IntConst v) <$> intTerm
  )
  <|> formulaAtom ctx ty

formulaPrefixOrEq :: Context -> Parser ty -> Parser (Formula event ty)
formulaPrefixOrEq ctx ty =
      formulaNextN ctx ty
  <|> formulaNext ctx ty
  <|> formulaExistsN ctx ty
  <|> formulaForall ctx ty
  <|> formulaForallN ctx ty
  <|> formulaNot ctx ty
  <|> formulaEq ctx ty

formulaAnd :: Context -> Parser ty -> Parser (Formula event ty)
formulaAnd ctx ty = apply <$> (formulaPrefixOrEq ctx ty <* space) <*> optional (do
    void $ string "∧"
    space
    formulaAnd ctx ty) where
  apply :: Formula event ty -> Maybe (Formula event ty) -> Formula event ty
  apply phi Nothing     = phi
  apply phi (Just !psi) = And phi psi

formulaOr :: Context -> Parser ty -> Parser (Formula event ty)
formulaOr ctx ty = apply <$> (formulaAnd ctx ty <* space) <*> optional (do
    void $ string "∨"
    space
    formulaOr ctx ty) where
  apply :: Formula event ty -> Maybe (Formula event ty) -> Formula event ty
  apply phi Nothing     = phi
  apply phi (Just !psi) = Or phi psi

formulaImplies :: Context -> Parser ty -> Parser (Formula event ty)
formulaImplies ctx ty = apply <$> (formulaOr ctx ty <* space) <*> optional (do
    void $ string "⇒"
    space
    formulaImplies ctx ty) where
  apply :: Formula event ty -> Maybe (Formula event ty) -> Formula event ty
  apply phi Nothing     = phi
  apply phi (Just !psi) = Implies phi psi

data Quantifier = Universal | Existential

-- | Handles Qx ∈ ℤ. φ,  Qx ∈ Text. φ,  Qx ∈ dom. φ  where Q is ∀ or ∃
formulaPropQuantifier :: Context -> Parser ty -> Parser (Formula event ty)
formulaPropQuantifier ctx ty = do
  q <- try $
        (Universal   <$ string "∀") <|>
        (Existential <$ string "∃")
  space
  x <- variableIdentifier
  space
  void $ string "∈"
  space
  result <-
    (Left  <$> ((IntKind <$ string "ℤ") <|> (TextKind <$ string "Text")))
    <|>
    (Right <$> setDomain ctx)
  space
  void $ string "."
  space
  let (kind, ctor) = case (q, result) of
        (Universal,   Left IntKind)          -> (IntKind,  PropIntForall  x)
        (Universal,   Left TextKind)         -> (TextKind, PropTextForall x)
        (Universal,   Right (IntDomain  is)) -> (IntKind,  PropIntForallN  x is)
        (Universal,   Right (TextDomain ts)) -> (TextKind, PropTextForallN x ts)
        (Existential, Left IntKind)          -> (IntKind,  PropIntExists  x)
        (Existential, Left TextKind)         -> (TextKind, PropTextExists x)
        (Existential, Right (IntDomain  is)) -> (IntKind,  PropIntExistsN  x is)
        (Existential, Right (TextDomain ts)) -> (TextKind, PropTextExistsN x ts)
  ctor <$> formulaUniverse (ctx { varKinds = Map.insert x kind ctx.varKinds }) ty

formulaUntilN :: Context -> Parser ty -> Parser (Formula event ty)
formulaUntilN ctx ty = apply <$> (formulaImplies ctx ty <* space) <*> optional (do
     void $ string "|"
     k <- superscriptWord
     space
     phi <- formulaImplies ctx ty
     pure (k, phi)) where
  apply :: Formula event ty -> Maybe (Word, Formula event ty) -> Formula event ty
  apply phi Nothing           = phi
  apply phi (Just (!k, !psi)) = UntilN k phi psi

formulaUniverse :: Context -> Parser ty -> Parser (Formula event ty)
formulaUniverse ctx ty = formulaPropQuantifier ctx ty
                     <|> formulaUntilN ctx ty

formula :: Context -> Parser ty -> Parser (Formula event ty)
formula = formulaUniverse
