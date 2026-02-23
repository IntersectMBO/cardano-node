{-# LANGUAGE TypeApplications #-}

module Cardano.LTL.Lang.Formula.Yaml(Exception, readPropValues, readFormulas) where

import           Cardano.LTL.Lang.Formula        (Formula, PropValue (..))
import           Cardano.LTL.Lang.Formula.Parser (Parser, Context)
import qualified Cardano.LTL.Lang.Formula.Parser as Parser
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Data.Yaml                       (prettyPrintParseException)
import           Data.Yaml.Include               (decodeFileEither)
import           Text.Megaparsec
import Data.Set (Set)
import Data.Map.Strict (Map)
import qualified Data.Set as Set

type FormulasEncodedType = [Text]

type PropValuesEncodedType = Map Text [Text]

type Exception = Text

readPropValues :: FilePath -> IO (Either Exception (Map Text (Set PropValue)))
readPropValues path = decodeFileEither @PropValuesEncodedType path >>= \case
  Left err -> pure (Left (Text.pack $ prettyPrintParseException err))
  Right propValues -> pure $ Right $ fmap (Set.fromList . fmap parsePropValue) propValues
  where
    -- | If the text is an integer, interpret as such, otherwise interpret as text.
    parsePropValue :: Text -> PropValue
    parsePropValue txt = either (const (TextValue txt)) IntValue (parse Parser.int "input" txt)

readFormulas :: FilePath -> Context -> Parser ty -> IO (Either Exception [Formula event ty])
readFormulas path ctx ty = decodeFileEither @FormulasEncodedType path >>= \case
  Left err -> pure (Left (Text.pack $ prettyPrintParseException err))
  Right formulas ->
    case traverse (parse (Parser.formula ctx ty) "input") formulas of
      Left err   -> pure (Left (Text.pack $ errorBundlePretty err))
      Right done -> pure (Right done)
