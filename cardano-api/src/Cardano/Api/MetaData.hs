module Cardano.Api.MetaData
  ( MetaDataJsonConversionError (..)
  , jsonToMetadata
  , renderMetaDataJsonConversionError
  ) where

import           Cardano.Prelude
import           Prelude (String)

import           Cardano.Api.Typed as Api

import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as Atto
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Scientific as Scientific
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector



data MetaDataJsonConversionError
  = ConversionErrDecodeJSON !String
  | ConversionErrToplevelNotMap
  | ConversionErrToplevelBadKey
  | ConversionErrBoolNotAllowed
  | ConversionErrNullNotAllowed
  | ConversionErrNumberNotInteger Double
  | ConversionErrLongerThan64Bytes
  deriving (Eq, Ord, Show)



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
        Api.TxMetaList <$> mapM jsonToMetadataValue (Vector.toList vs)
      Aeson.Object kvs ->
        Api.TxMetaMap <$> traverse convertPair (HashMap.toList kvs)
  where
    convertPair :: (Text, Aeson.Value)
                -> Either MetaDataJsonConversionError (TxMetadataValue, TxMetadataValue)
    convertPair (k, v) =
      (,) <$> jsonToMetadataValue (Aeson.String k) <*> jsonToMetadataValue v

jsonToMetadataString :: Text -> Either MetaDataJsonConversionError TxMetadataValue
jsonToMetadataString txt
    -- If the text is encoded in hex, we convert it to a byte string.
  | BS.take 2 utf8 == "0x"
  , let (raw, trailing) = Base16.decode (BS.drop 2 utf8)
  , BS.null trailing
      = if BS.length raw > 64
          then Left ConversionErrLongerThan64Bytes
          else Right (Api.TxMetaBytes raw)

  | otherwise
      = if BS.length utf8 > 64
                then Left ConversionErrLongerThan64Bytes
                else Right (Api.TxMetaText txt)
  where
    utf8 = encodeUtf8 txt
