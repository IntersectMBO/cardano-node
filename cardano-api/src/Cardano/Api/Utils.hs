{-# LANGUAGE LambdaCase #-}

-- | Internal utils for the other Api modules
--
module Cardano.Api.Utils
  ( (?!)
  , (?!.)
  , formatParsecError
  , noInlineMaybeToStrictMaybe
  , runParsecParser
  , note
  ) where

import           Prelude

import qualified Data.Aeson.Types as Aeson
import           Data.Maybe.Strict
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.String as Parsec
import qualified Text.ParserCombinators.Parsec.Error as Parsec

(?!) :: Maybe a -> e -> Either e a
Nothing ?! e = Left e
Just x  ?! _ = Right x

(?!.) :: Either e a -> (e -> e') -> Either e' a
Left  e ?!. f = Left (f e)
Right x ?!. _ = Right x

{-# NOINLINE noInlineMaybeToStrictMaybe #-}
noInlineMaybeToStrictMaybe :: Maybe a -> StrictMaybe a
noInlineMaybeToStrictMaybe Nothing = SNothing
noInlineMaybeToStrictMaybe (Just x) = SJust x

formatParsecError :: Parsec.ParseError -> String
formatParsecError err =
  Parsec.showErrorMessages "or" "unknown parse error"
    "expecting" "unexpected" "end of input"
    $ Parsec.errorMessages err

runParsecParser :: Parsec.Parser a -> Text -> Aeson.Parser a
runParsecParser parser input =
  case Parsec.parse (parser <* Parsec.eof) "" (Text.unpack input) of
    Right txin -> pure txin
    Left parseError -> fail $ formatParsecError parseError

note :: MonadFail m => String -> Maybe a -> m a
note msg = \case
  Nothing -> fail msg
  Just a -> pure a
