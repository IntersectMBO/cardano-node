{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Use newtype instead of data" -}
{-# LANGUAGE OverloadedRecordDot #-}
module Cardano.LTL.Lang.Formula.Parser (Parser, Context(..), int, text, propValue, formula) where


import           Cardano.LTL.Lang.Formula
import           Control.Monad              (guard)
import           Data.Char                  (isAlpha, isAlphaNum)
import           Data.Foldable              (asum)
import           Data.Functor               (void, (<&>))
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Void                  (Void)
import           GHC.Unicode                (isControl, isSpace)
import           Prelude                    hiding (head)
import           Text.Megaparsec
import           Text.Megaparsec.Char       (char, space, string)
import           Text.Megaparsec.Char.Lexer (decimal, signed)

type Parser = Parsec Void Text

data Context = Context {
  interpDomain :: [(Text, Set PropValue)]
}

keywords :: [Text]
keywords = []

unescapedVariableIdentifierNextChar :: Parser Char
unescapedVariableIdentifierNextChar = satisfy (\x -> isAlphaNum x || x == '_')

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

int :: Parser Int
int = signed (pure ()) decimal

variableIdentifier :: Parser Text
variableIdentifier = do
  x <- unescapedVariableIdentifier
  guard (x `notElem` keywords)
  pure x

text :: Parser Text
text = Text.pack <$> (char '\"' *> many one) <* char '\"' where
  one :: Parser Char
  one = satisfy (\x -> not (isControl x) && (not (isSpace x) || x == ' ') && x /= '"')

formulaBottom :: Parser (Formula event ty)
formulaBottom = Bottom <$ string "⊥"

formulaTop :: Parser (Formula event ty)
formulaTop = Top <$ string "⊤"

constraint :: Parser PropConstraint
constraint = PropConstraint <$> (text <* space <* char '=') <*> (space *> propTerm)

constraints :: Parser (Set PropConstraint)
constraints = Set.fromList <$> (char '{' *> space *> sepBy constraint (space *> char ',' <* space) <* space <* char '}')

formulaInParens :: Context -> Parser ty -> Parser (Formula event ty)
formulaInParens ctx ty = char '(' *> space *> formulaUniverse ctx ty <* space <* char ')'

formulaPropAtom :: Parser ty -> Parser (Formula event ty)
formulaPropAtom ty = Atom <$> ty <*> (space *> constraints)

formulaAtom :: Context -> Parser ty -> Parser (Formula event ty)
formulaAtom ctx ty = formulaBottom <|> formulaTop <|> formulaInParens ctx ty <|> formulaPropAtom ty

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

propValue :: Parser PropValue
propValue = try (IntValue <$> int) <|> TextValue <$> text

setPropValueConst :: Parser (Set PropValue)
setPropValueConst = Set.fromList <$> (string "{" *> space *> sepBy propValue (space *> string "," <* space) <* space <* string "}")

setPropValueVar :: Context -> Parser (Set PropValue)
setPropValueVar ctx = do
  void $ string "$"
  asum (ctx.interpDomain <&> \(k, v) -> v <$ string k)

setPropValue :: Context -> Parser (Set PropValue)
setPropValue ctx = setPropValueVar ctx <|> setPropValueConst

propTerm :: Parser PropTerm
propTerm = try (Const <$> propValue) <|> Var <$> variableIdentifier

formulaEq :: Context -> Parser ty -> Parser (Formula event ty)
formulaEq ctx ty = try (PropEq Set.empty <$> (propTerm <* space <* char '=') <*> (space *> propValue))
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

formulaPropForall :: Context -> Parser ty -> Parser (Formula event ty)
formulaPropForall ctx ty = do
  void $ string "∀"
  space
  x <- variableIdentifier
  space
  optDom <- optional $ do
    void $ string "∈"
    space *> setPropValue ctx <* space
  void $ string "."
  space
  phi <- formulaUniverse ctx ty
  pure (maybe (PropForall x phi) (\dom -> PropForallN x dom phi) optDom)

formulaPropForallN :: Context -> Parser ty -> Parser (Formula event ty)
formulaPropForallN ctx ty = do
  void $ try (string "∀" *> space *> string "(" *> space)
  x <- variableIdentifier
  space
  void $ string "∈"
  space
  vs <- setPropValue ctx
  space
  void $ string ")"
  space
  void $ string "."
  space
  phi <- formulaUniverse ctx ty
  pure (PropForallN x vs phi)

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
formulaUniverse ctx ty = formulaPropForallN ctx ty <|> formulaPropForall ctx ty <|> formulaUntilN ctx ty

formula :: Context -> Parser ty -> Parser (Formula event ty)
formula = formulaUniverse
