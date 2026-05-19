module Cardano.ReCon.LTL.Formula.Yaml(YamlReadError, readPropValues, readFormulas) where

import           Cardano.ReCon.Common.Parser
import           Cardano.ReCon.Common.Types
import           Cardano.ReCon.LTL.Formula (Formula)
import           Cardano.ReCon.LTL.Formula.Parser (Context, Domain (..))
import qualified Cardano.ReCon.LTL.Formula.Parser as Parser

import           Data.Map.Strict (Map)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Yaml (prettyPrintParseException)
import           Data.Yaml.Include (decodeFileEither)
import           Text.Megaparsec

type FormulasEncodedType = [Text]

type PropValuesEncodedType = Map Text [Text]

type YamlReadError = Text

readPropValues :: FilePath -> IO (Either YamlReadError (Map Text Domain))
readPropValues path = decodeFileEither @PropValuesEncodedType path >>= \case
  Left err -> pure (Left (Text.pack $ prettyPrintParseException err))
  Right propValues -> pure $ Right $ fmap parseValues propValues
  where
    -- | If all values parse as integers, produce an IntDomain; otherwise TextDomain.
    parseValues :: [Text] -> Domain
    parseValues vs =
      case traverse (either (const Nothing) Just . parse parseIntValue "input") vs of
        Just ints -> IntDomain (Set.fromList ints)
        Nothing   -> TextDomain (Set.fromList vs)

readFormulas :: FilePath -> Context -> Parser ty -> IO (Either YamlReadError [Formula event ty])
readFormulas path ctx ty = decodeFileEither @FormulasEncodedType path >>= \case
  Left err -> pure (Left (Text.pack $ prettyPrintParseException err))
  Right formulas ->
    case traverse (parse (Parser.formula ctx ty) "input") formulas of
      Left err   -> pure (Left (Text.pack $ errorBundlePretty err))
      Right done -> pure (Right done)
