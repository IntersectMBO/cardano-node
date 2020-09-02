module Cardano.Api.MetaData
  ( MetaDataJsonConversionError (..)
  , TxMetadataValidationError (..)
  , jsonFromMetadata
  , jsonFromMetadataValue
  , jsonToMetadata
  , jsonToMetadataValue
  , renderMetaDataJsonConversionError
  , renderTxMetadataValidationError
  , validateTxMetadata
  , bytesPrefix
  , txMetadataTextStringMaxByteLength
  , txMetadataByteStringMaxLength
  ) where

import           Cardano.Prelude hiding (MetaData)
import           Prelude (String)


import           Cardano.Api.Typed as Api

import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as Atto
import qualified Data.ByteString.Char8 as BS
import           Data.Bifunctor (bimap)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector

import           Shelley.Spec.Ledger.MetaData (MetaData (..), MetaDatum (..))



-- | A transaction metadata validation error.
data TxMetadataValidationError
  = TxMetadataTextStringInvalidLengthError
    -- ^ The length of a text string metadatum value exceeds the maximum.
      !Int
      -- ^ Maximum byte length.
      !Int
      -- ^ Actual byte length.
  | TxMetadataByteStringInvalidLengthError
    -- ^ The length of a byte string metadatum value exceeds the maximum.
      !Int
      -- ^ Maximum byte length.
      !Int
      -- ^ Actual byte length.
  deriving (Eq, Show)

-- | The maximum byte length of a transaction metadata text string value.
txMetadataTextStringMaxByteLength :: Int
txMetadataTextStringMaxByteLength = 64

-- | The maximum length of a transaction metadata byte string value.
txMetadataByteStringMaxLength :: Int
txMetadataByteStringMaxLength = 64

-- | Render a transaction metadata validation error as text.
renderTxMetadataValidationError :: TxMetadataValidationError -> Text
renderTxMetadataValidationError err =
  case err of
    TxMetadataTextStringInvalidLengthError maxLen actualLen ->
      "Text string metadatum value must consist of at most "
        <> show maxLen
        <> " bytes, but it consists of "
        <> show actualLen
        <> " bytes."
    TxMetadataByteStringInvalidLengthError maxLen actualLen ->
      "Byte string metadatum value must consist of at most "
        <> show maxLen
        <> " bytes, but it consists of "
        <> show actualLen
        <> " bytes."

-- | Validate the provided transaction metadata.
validateTxMetadata
  :: TxMetadata
  -> Either (NonEmpty TxMetadataValidationError) TxMetadata
validateTxMetadata txMd@(TxMetadata (MetaData mdMap)) =
    maybe (Right txMd) Left . nonEmpty $ foldMap validate mdMap
  where
    validate :: MetaDatum -> [TxMetadataValidationError]
    validate metaDatum =
      case metaDatum of
        Map mdPairs -> foldMap (\(k, v) -> validate k <> validate v) mdPairs
        List mds -> foldMap validate mds

        I _ -> mempty

        B bs
          | BS.length bs <= txMetadataByteStringMaxLength -> mempty
          | otherwise ->
              [ TxMetadataByteStringInvalidLengthError
                  txMetadataByteStringMaxLength
                  (BS.length bs)
              ]

        S txt
          | BS.length (Text.encodeUtf8 txt) <= txMetadataTextStringMaxByteLength -> mempty
          | otherwise ->
              [ TxMetadataTextStringInvalidLengthError
                  txMetadataTextStringMaxByteLength
                  (BS.length (Text.encodeUtf8 txt))
              ]

-- -------------------------------------------------------------------------------------------------

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
      Aeson.String $ bytesPrefix <> Text.decodeUtf8 (Base16.encode bs)

-- JSON only allows keys to be strings. So, there really are two cases:
--
-- - Either the keys of the CBOR map are indeed strings and we render them as such
-- - Or they are something else, and we render them as a serialized JSON string
--
-- So, for metadata coming from JSON in the first place, this will be pretty much invisible.
-- And for more elaborated metadata crafted by other mean (via a CBOR library for instance),
-- the key in the JSON-rendered metadata will look at bit funky.
jsonFromPairList :: [(MetaDatum, MetaDatum)] -> Aeson.Value
jsonFromPairList =
    Aeson.toJSON . Map.fromList . fmap (bimap metadataValueToString jsonFromMetadataValue)
  where
    metadataValueToString :: MetaDatum -> Text
    metadataValueToString (S txt) = txt
    metadataValueToString someValue
      = Text.decodeUtf8
      $ stripQuotes
      $ BL.toStrict
      $ Aeson.encode
      $ jsonFromMetadataValue someValue

    stripQuotes :: ByteString -> ByteString
    stripQuotes = BS.tail . BS.init

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
    ConversionErrLongerThan64Bytes -> "JSON string or bytestring is longer than 64 bytes"
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
      Aeson.String txt ->
        case Text.stripPrefix bytesPrefix txt of
          Nothing  -> jsonToMetadataString txt
          Just hex -> case Base16.decode (Text.encodeUtf8 hex) of
            (bytes, "") -> jsonToMetadataBytes bytes
            (_, err)    -> Left $ ConversionErrBadBytes err
      Aeson.Array vs ->
        Api.TxMetaList <$> traverse jsonToMetadataValue (Vector.toList vs)
      Aeson.Object kvs ->
        Api.TxMetaMap . List.sortOn fst <$> traverse convertPair (HashMap.toList kvs)
  where
    convertPair :: (Text, Aeson.Value)
                -> Either MetaDataJsonConversionError (TxMetadataValue, TxMetadataValue)
    convertPair (k, v) =
      (,) <$> jsonToMetadataValue (Aeson.String k) <*> jsonToMetadataValue v

jsonToMetadataString :: Text -> Either MetaDataJsonConversionError TxMetadataValue
jsonToMetadataString txt =
    if BS.length utf8 > txMetadataTextStringMaxByteLength
      then Left ConversionErrLongerThan64Bytes
      else Right (Api.TxMetaText txt)
  where
    utf8 :: ByteString
    utf8 = Text.encodeUtf8 txt

jsonToMetadataBytes :: ByteString -> Either MetaDataJsonConversionError TxMetadataValue
jsonToMetadataBytes bytes =
    if BS.length bytes > txMetadataByteStringMaxLength
      then Left ConversionErrLongerThan64Bytes
      else Right (Api.TxMetaBytes bytes)

-- | JSON strings that are base16 encoded and prefixed with 'bytesPrefix' will
-- be encoded as CBOR bytestrings.
bytesPrefix :: Text
bytesPrefix = "0x"
