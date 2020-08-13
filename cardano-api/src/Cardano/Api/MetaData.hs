module Cardano.Api.MetaData
  ( MetaDataJsonConversionError (..)
  , jsonFromMetadata
  , jsonFromMetadataValue
  , jsonToMetadata
  , jsonToMetadataValue
  , renderMetaDataJsonConversionError
  ) where

import           Cardano.Prelude hiding (MetaData)
import           Prelude (String)

import           Cardano.Api.Typed as Api

import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as Atto
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified Data.Vector as Vector

import           Shelley.Spec.Ledger.MetaData (MetaData (..), MetaDatum (..))



data MetaDataJsonConversionError
  = ConversionErrDecodeJSON !String
  | ConversionErrToplevelNotMap
  | ConversionErrToplevelBadKey
  | ConversionErrBoolNotAllowed
  | ConversionErrNullNotAllowed
  | ConversionErrNumberNotInteger !Double
  | ConversionErrLongerThan64Bytes
  | ConversionErrBadBytes !ByteString
  | ConversionErrExpected !Text !Text
  deriving (Eq, Ord, Show)

jsonFromMetadata :: Api.TxMetadata -> Aeson.Value
jsonFromMetadata (Api.TxMetadata (MetaData meta)) =
    Aeson.Object $ HashMap.fromList (map convertPair $ Map.toList meta)
  where
    convertPair :: (Word64, MetaDatum) -> (Text, Aeson.Value)
    convertPair (w, d) = (Text.pack (show w), jsonFromMetadataValue d)

jsonFromMetadataValue :: MetaDatum -> Aeson.Value
jsonFromMetadataValue md =
    case md of
      S txt -> Aeson.String txt
      B bs -> jsonFromByteString bs
      I i -> Aeson.Number (fromInteger i)
      List xs -> Aeson.toJSON $ map jsonFromMetadataValue xs
      Map xs -> jsonFromPairList xs
  where
    jsonFromByteString :: ByteString -> Aeson.Value
    jsonFromByteString bs =
      Aeson.object [ "hex" .= Aeson.String (Text.decodeUtf8 $ Base16.encode bs) ]

-- Try to convert it to an Aeson Object and if that fails, use an Array.
-- Object is basically a 'HashMap Text Value' but if the first element of
-- the tuple is not Text, convert it to an Array instead.
jsonFromPairList :: [(MetaDatum, MetaDatum)] -> Aeson.Value
jsonFromPairList xs =
    -- If all the left hand elements of 'xs' are 'S' ('collapseLeft' returns 'Just' for every
    -- element), then convert this into a JSON object.
    -- If one of more of the elements return 'Nothing' then represent it as a JSON list.
    case traverse collapseLeft xs of
      Nothing -> Aeson.toJSON $ map (\ (a, b) -> (jsonFromMetadataValue a, jsonFromMetadataValue b)) xs
      Just zs -> Aeson.Object $ HashMap.fromList zs
  where
    collapseLeft :: (MetaDatum, MetaDatum) -> Maybe (Text, Aeson.Value)
    collapseLeft (a, b) =
      case a of
        S txt -> Just (txt, jsonFromMetadataValue b)
        _otherwise -> Nothing


jsonToMetadata :: Aeson.Value
               -> Either MetaDataJsonConversionError Api.TxMetadata
jsonToMetadata av =
    case av of
      Aeson.Object kvs ->
        fmap (Api.makeTransactionMetadata . Map.fromList)
          . mapM (\(k,v) -> (,) <$> expectWord64 k <*> jsonToMetadataValue v)
          $ HashMap.toList kvs
      _otherwise -> Left ConversionErrToplevelNotMap
  where
    expectWord64 :: Text -> Either MetaDataJsonConversionError Word64
    expectWord64 =
      first (const ConversionErrToplevelBadKey)
        . Atto.parseOnly ((Atto.decimal <|> Atto.hexadecimal) <* Atto.endOfInput)


renderMetaDataJsonConversionError :: MetaDataJsonConversionError -> Text
renderMetaDataJsonConversionError err =
  case err of
    ConversionErrDecodeJSON decErr -> "Error decoding JSON: " <> show decErr
    ConversionErrToplevelNotMap -> "The JSON metadata top level must be a map (object) from word to value"
    ConversionErrToplevelBadKey -> "The JSON metadata top level must be a map with unsigned integer keys"
    ConversionErrBoolNotAllowed -> "JSON Bool value is not allowed in MetaData"
    ConversionErrNullNotAllowed -> "JSON Null value is not allowed in MetaData"
    ConversionErrNumberNotInteger _ -> "Only integers are allowed in MetaData"
    ConversionErrLongerThan64Bytes -> "JSON string is longer than 64 bytes"
    ConversionErrBadBytes bs -> "JSON failure to decode as hex bytestring: " <> show bs
    ConversionErrExpected expected actual ->
      mconcat [ "JSON decode error. Expected ", expected, " but got '", actual, "'"]

-- -------------------------------------------------------------------------------------------------

jsonToMetadataValue :: Aeson.Value
                    -> Either MetaDataJsonConversionError Api.TxMetadataValue
jsonToMetadataValue  av =
    case av of
      Aeson.Null -> Left ConversionErrNullNotAllowed
      Aeson.Bool _ -> Left ConversionErrBoolNotAllowed
      Aeson.Number sci ->
        case Scientific.floatingOrInteger sci :: Either Double Integer of
          Left  n -> Left (ConversionErrNumberNotInteger n)
          Right n -> Right (Api.TxMetaNumber n)
      Aeson.String txt -> jsonToMetadataString txt
      Aeson.Array vs ->
        let xs = Vector.toList vs in
        case convertAsMap [] xs of
          Nothing -> Api.TxMetaList <$> traverse jsonToMetadataValue xs
          Just ys -> Right (Api.TxMetaMap ys)
      Aeson.Object kvs ->
        if HashMap.keys kvs /= ["hex"]
          then Api.TxMetaMap . List.sortOn fst <$> traverse convertPair (HashMap.toList kvs)
          else
            case HashMap.elems kvs of
              [Aeson.String bs] ->
                case Base16.decode (Text.encodeUtf8 bs) of
                  (bs2, "") -> Right $ TxMetaBytes bs2
                  (_, bsf) -> Left $ ConversionErrBadBytes bsf
              _otherwise -> Left $ ConversionErrExpected "[Aeson.String x]" (show kvs)
  where
    convertPair :: (Text, Aeson.Value)
                -> Either MetaDataJsonConversionError (TxMetadataValue, TxMetadataValue)
    convertPair (k, v) =
      (,) <$> jsonToMetadataValue (Aeson.String k) <*> jsonToMetadataValue v

    -- If the list of values is a list of pairs, then return it as such, otherwise
    -- return Nothing.
    convertAsMap :: [(TxMetadataValue, TxMetadataValue)] -> [Aeson.Value]
                    -> Maybe [(TxMetadataValue, TxMetadataValue)]
    convertAsMap accum xs =
      case xs of
        [] -> Just $ List.sortOn fst accum
        (Aeson.Array ys:zs) ->
          case Vector.toList ys of
            [a, b] -> case (jsonToMetadataValue a, jsonToMetadataValue b) of
                        (Right am, Right bm) -> convertAsMap ((am, bm):accum) zs
                        _otherwise -> Nothing
            _otherwise -> Nothing
        _otherwise -> Nothing


jsonToMetadataString :: Text -> Either MetaDataJsonConversionError TxMetadataValue
jsonToMetadataString txt =
    if BS.length utf8 > 64
      then Left ConversionErrLongerThan64Bytes
      else Right (Api.TxMetaText txt)
  where
    utf8 :: ByteString
    utf8 = Text.encodeUtf8 txt
