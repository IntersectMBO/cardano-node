module Cardano.ReCon.Integer.Polynomial.Parser (Parser, intTerm) where

import           Cardano.ReCon.Common.Parser
import           Cardano.ReCon.Common.Types
import           Cardano.ReCon.Integer.Polynomial.Term (IntTerm (..), mul)

import           Text.Megaparsec
import           Text.Megaparsec.Char (char, space)

-- ---------------------------------------------------------------------------
-- IntTerm parser
-- ---------------------------------------------------------------------------

-- | Atom-level IntTerm.
--
-- @
--   ( intTerm )
--   k · x          (signed coefficient times identifier)
--   k              (signed integer constant)
--   x              (identifier, implicit coefficient 1)
-- @
intTermAtom :: Parser IntTerm
intTermAtom =
      char '(' *> space *> intTerm <* space <* char ')'
  <|> try (IntVar <$> parseIntValue <* char '·' <*> parseIdentifier)
  <|> IntConst <$> parseIntValue
  <|> IntVar 1 <$> parseIdentifier

-- | Full IntTerm: atoms joined by @+@ and @-@.
intTerm :: Parser IntTerm
intTerm = do
  hd   <- intTermAtom
  rest <- many addOp
  pure (foldl applyOp hd rest)
  where
    -- Each alternative wraps the leading space in `try` so that whitespace
    -- consumed before a non-additive token (e.g. '<') is backtracked.
    addOp =
          try (space *> (Left  <$> (char '+' *> space *> intTermAtom)))
      <|> try (space *> (Right <$> (char '-' *> space *> intTermAtom)))
    applyOp acc (Left  t) = IntSum acc t
    applyOp acc (Right t) = IntSum acc (mul (-1) t)
