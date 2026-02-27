module Cardano.ReCon.LTL.Lang.Formula.Yaml(YamlReadError, readPropValues, readFormulas) where

import           Cardano.ReCon.LTL.Lang.Formula (Formula, PropValue (..))
import           Cardano.ReCon.LTL.Lang.Formula.Parser (Context, Parser)
import qualified Cardano.ReCon.LTL.Lang.Formula.Parser as Parser

import           Data.Map.Strict (Map)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Yaml (prettyPrintParseException)
import           Data.Yaml.Include (decodeFileEither)
import           Text.Megaparsec

type FormulasEncodedType = [Text]

type PropValuesEncodedType = Map Text [Text]

type YamlReadError = Text

readPropValues :: FilePath -> IO (Either YamlReadError (Map Text (Set PropValue)))
readPropValues path = decodeFileEither @PropValuesEncodedType path >>= \case
  Left err -> pure (Left (Text.pack $ prettyPrintParseException err))
  Right propValues -> pure $ Right $ fmap (Set.fromList . fmap parsePropValue) propValues
  where
    -- | If the text is an integer, interpret as such, otherwise interpret as text.
    parsePropValue :: Text -> PropValue
    parsePropValue txt = either (const (TextValue txt)) IntValue (parse Parser.int "input" txt)

readFormulas :: FilePath -> Context -> Parser ty -> IO (Either YamlReadError [Formula event ty])
readFormulas path ctx ty = decodeFileEither @FormulasEncodedType path >>= \case
  Left err -> pure (Left (Text.pack $ prettyPrintParseException err))
  Right formulas ->
    case traverse (parse (Parser.formula ctx ty) "input") formulas of
      Left err   -> pure (Left (Text.pack $ errorBundlePretty err))
      Right done -> pure (Right done)
