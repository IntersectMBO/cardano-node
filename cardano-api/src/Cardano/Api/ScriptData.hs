{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.ScriptData (
    -- * Script data
    HashableScriptData,
    hashScriptDataBytes,
    getOriginalScriptDataBytes,
    getScriptData,
    unsafeHashableScriptData,
    ScriptData(..),

    -- * Script data hashes
    hashScriptData,

    -- * Validating metadata
    validateScriptData,
    ScriptDataRangeError (..),

    -- * Conversion to\/from JSON
    ScriptDataJsonSchema (..),
    scriptDataFromJson,
    scriptDataToJson,
    ScriptDataJsonError (..),
    ScriptDataJsonSchemaError (..),
    scriptDataFromJsonDetailedSchema,
    scriptDataToJsonDetailedSchema,
    ScriptBytesError(..),
    ScriptDataJsonBytesError(..),
    scriptDataJsonToHashable,

    -- * Internal conversion functions
    toPlutusData,
    fromPlutusData,
    toAlonzoData,
    fromAlonzoData,

    -- * Data family instances
    AsType(..),
    Hash(..),
  ) where

import qualified Cardano.Binary as CBOR
import           Codec.Serialise.Class (Serialise (..))
import           Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Short as SB
import qualified Data.Char as Char
import           Data.Either.Combinators
import qualified Data.List as List
import           Data.Maybe (fromMaybe)
import qualified Data.Scientific as Scientific
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Vector as Vector
import           Data.Word

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Text as Aeson.Text
import qualified Data.Attoparsec.ByteString.Char8 as Atto

import           Control.Applicative (Alternative (..))

import qualified Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Ledger.Core (Era)
import qualified Cardano.Ledger.Alonzo.Scripts.Data as Alonzo
import qualified Cardano.Ledger.SafeHash as Ledger
import           Ouroboros.Consensus.Shelley.Eras (StandardAlonzo, StandardCrypto)
import qualified PlutusLedgerApi.V1 as Plutus

import           Cardano.Api.Eras
import           Cardano.Api.Error
import           Cardano.Api.Hash

import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Keys.Shelley

import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseJSON
import           Cardano.Api.SerialiseRaw
import           Cardano.Api.SerialiseUsing
import           Cardano.Api.TxMetadata (pBytes, pSigned, parseAll)

-- Original script data bytes
data HashableScriptData
  = HashableScriptData
      !BS.ByteString -- ^ Original 'ScriptData' bytes
      !ScriptData
      deriving (Eq, Show)

instance HasTypeProxy HashableScriptData where
    data AsType HashableScriptData = AsHashableScriptData
    proxyToAsType _ = AsHashableScriptData

instance SerialiseAsCBOR HashableScriptData where
    serialiseToCBOR (HashableScriptData origBytes _) = origBytes
    deserialiseFromCBOR AsHashableScriptData bs =
      HashableScriptData bs
        <$> CBOR.decodeFullDecoder "ScriptData" fromCBOR (LBS.fromStrict bs)


getOriginalScriptDataBytes :: HashableScriptData -> BS.ByteString
getOriginalScriptDataBytes (HashableScriptData bs _) = bs

getScriptData :: HashableScriptData -> ScriptData
getScriptData (HashableScriptData _ sd) = sd

-- | Warning: Creating 'HashableScriptData' from a 'ScriptData' value pretty
-- much guarantees the original bytes used to create the 'ScriptData'
-- value will be different if we serialize `HashableScriptData` again.
-- Do not use this.
unsafeHashableScriptData :: ScriptData -> HashableScriptData
unsafeHashableScriptData sd = HashableScriptData (serialiseToCBOR sd) sd

-- ----------------------------------------------------------------------------
-- Script data - Allows us to represent script data as JSON
--

data ScriptData = ScriptDataConstructor
                                        Integer                     -- ^ Tag for the constructor
                                        [ScriptData]                -- ^ Constructor arguments
                | ScriptDataMap         [(ScriptData, ScriptData)]  -- ^ Key value pairs
                | ScriptDataList        [ScriptData]                -- ^ Elements
                | ScriptDataNumber      Integer
                | ScriptDataBytes       BS.ByteString
  deriving (Eq, Ord, Show)
  -- Note the order of constructors is the same as the Plutus definitions
  -- so that the Ord instance is consistent with the Plutus one.
  -- This is checked by prop_ord_distributive_ScriptData

instance HasTypeProxy ScriptData where
    data AsType ScriptData = AsScriptData
    proxyToAsType _ = AsScriptData

-- ----------------------------------------------------------------------------
-- Script data hash
--

newtype instance Hash ScriptData =
    ScriptDataHash (Alonzo.DataHash StandardCrypto)
  deriving stock (Eq, Ord)
  deriving (Show, IsString)         via UsingRawBytesHex (Hash ScriptData)
  deriving (ToJSON, FromJSON)       via UsingRawBytesHex (Hash ScriptData)
  deriving (ToJSONKey, FromJSONKey) via UsingRawBytesHex (Hash ScriptData)

instance SerialiseAsRawBytes (Hash ScriptData) where
    serialiseToRawBytes (ScriptDataHash dh) =
      Crypto.hashToBytes (Ledger.extractHash dh)

    deserialiseFromRawBytes (AsHash AsScriptData) bs =
      maybeToRight (SerialiseAsRawBytesError "Unable to deserialise Hash ScriptData") $
        ScriptDataHash . Ledger.unsafeMakeSafeHash <$> Crypto.hashFromBytes bs

instance SerialiseAsCBOR ScriptData where
    serialiseToCBOR = CBOR.serialize'
    deserialiseFromCBOR AsScriptData bs = CBOR.decodeFullDecoder "ScriptData" fromCBOR (LBS.fromStrict bs) :: Either CBOR.DecoderError ScriptData


instance ToCBOR ScriptData where
  toCBOR = encode @Plutus.Data . toPlutusData

instance FromCBOR ScriptData where
  fromCBOR :: CBOR.Decoder s ScriptData
  fromCBOR = fromPlutusData <$> decode @Plutus.Data

hashScriptDataBytes :: HashableScriptData -> Hash ScriptData
hashScriptDataBytes  =
  ScriptDataHash . Alonzo.hashData . (toAlonzoData :: HashableScriptData  -> Alonzo.Data StandardAlonzo)

{-# DEPRECATED hashScriptData "Use hashScriptDataBytes" #-}
hashScriptData :: HashableScriptData -> Hash ScriptData
hashScriptData = hashScriptDataBytes

-- ----------------------------------------------------------------------------
-- Conversion functions
--

newtype ScriptBytesError = ScriptBytesError String deriving Show

-- There is a subtlety here. We must use the original bytes
-- when converting to and from `HashableScriptData`/`Data`. This
-- avoids problems that arise due to reserialization of the script
-- data i.e differing script data hashes due to the re-encoding being slightly
-- different to the original encoding. See: https://github.com/input-output-hk/cardano-ledger/issues/2943

toAlonzoData :: Era ledgerera => HashableScriptData -> Alonzo.Data ledgerera
toAlonzoData =
  either
  (\ e -> error $ "toAlonzoData: " <> show e)
  Alonzo.binaryDataToData
  . first ScriptBytesError . Alonzo.makeBinaryData . SB.toShort . getOriginalScriptDataBytes

fromAlonzoData :: Alonzo.Data ledgerera -> HashableScriptData
fromAlonzoData d =
  HashableScriptData
    (Ledger.originalBytes d)
    (fromPlutusData $ Alonzo.getPlutusData d)

toPlutusData :: ScriptData -> Plutus.Data
toPlutusData (ScriptDataConstructor int xs)
                                  = Plutus.Constr int
                                      [ toPlutusData x | x <- xs ]
toPlutusData (ScriptDataMap  kvs) = Plutus.Map
                                      [ (toPlutusData k, toPlutusData v)
                                      | (k,v) <- kvs ]
toPlutusData (ScriptDataList  xs) = Plutus.List
                                      [ toPlutusData x | x <- xs ]
toPlutusData (ScriptDataNumber n) = Plutus.I n
toPlutusData (ScriptDataBytes bs) = Plutus.B bs


fromPlutusData :: Plutus.Data -> ScriptData
fromPlutusData (Plutus.Constr int xs)
                                = ScriptDataConstructor int
                                    [ fromPlutusData x | x <- xs ]
fromPlutusData (Plutus.Map kvs) = ScriptDataMap
                                    [ (fromPlutusData k, fromPlutusData v)
                                    | (k,v) <- kvs ]
fromPlutusData (Plutus.List xs) = ScriptDataList
                                    [ fromPlutusData x | x <- xs ]
fromPlutusData (Plutus.I     n) = ScriptDataNumber n
fromPlutusData (Plutus.B    bs) = ScriptDataBytes bs


-- ----------------------------------------------------------------------------
-- Validate script data
--

-- | Validate script data. This is for use with existing constructed script
-- data values, e.g. constructed manually or decoded from CBOR directly.
--
validateScriptData :: ScriptData -> Either ScriptDataRangeError ()
validateScriptData d =
    case collect d of
      []    -> Right ()
      err:_ -> Left err
  where
    -- Arbitrary size numbers are fine
    collect (ScriptDataNumber _) = []

    -- Arbitrary sized bytes are fine
    collect (ScriptDataBytes _) = []

    collect (ScriptDataList xs) =
        foldMap collect xs

    collect (ScriptDataMap kvs) =
        foldMap (\(k, v) -> collect k
                         <> collect v)
                kvs

    -- Constr tags do need to be less than a Word64
    collect (ScriptDataConstructor n xs) =
        [ ScriptDataConstructorOutOfRange n
        | n > fromIntegral (maxBound :: Word64) || n < 0 ]
     <> foldMap collect xs

-- | An error in script data due to an out-of-range value.
--
newtype ScriptDataRangeError =

    -- | The constructor number is outside the maximum range of @-2^64-1 .. 2^64-1@.
    --
  ScriptDataConstructorOutOfRange Integer
  deriving (Eq, Show)

instance Error ScriptDataRangeError where
  displayError (ScriptDataConstructorOutOfRange n) =
      "Constructor numbers in script data value "
        <> show n
        <> " is outside the range 0 .. 2^64-1."


-- ----------------------------------------------------------------------------
-- JSON conversion
--

-- | Script data is similar to JSON but not exactly the same. It has some
-- deliberate limitations such as no support for floating point numbers or
-- special forms for null or boolean values. It also has limitations on the
-- length of strings. On the other hand, unlike JSON, it distinguishes between
-- byte strings and text strings. It also supports any value as map keys rather
-- than just string. It also supports alternatives \/ tagged unions, used for
-- representing constructors for Plutus data values.
--
-- We provide two different mappings between script data and JSON, useful
-- for different purposes:
--
-- 1. A mapping that allows almost any JSON value to be converted into script
--    data. This does not require a specific JSON schema for the input. It does
--    not expose the full representation capability of script data.
--
-- 2. A mapping that exposes the full representation capability of script data,
--    but relies on a specific JSON schema for the input JSON.
--
-- In the \"no schema"\ mapping, the idea is that (almost) any JSON can be
-- turned into script data and then converted back, without loss. That is, we
-- can round-trip the JSON.
--
-- The subset of JSON supported is all JSON except:
-- * No null or bool values
-- * No floating point, only integers in the range of a 64bit signed integer
-- * A limitation on string lengths
--
-- The approach for this mapping is to use whichever representation as script
-- data is most compact. In particular:
--
-- * JSON lists and maps represented as CBOR lists and maps
-- * JSON strings represented as CBOR strings
-- * JSON hex strings with \"0x\" prefix represented as CBOR byte strings
-- * JSON integer numbers represented as CBOR signed or unsigned numbers
-- * JSON maps with string keys that parse as numbers or hex byte strings,
--   represented as CBOR map keys that are actually numbers or byte strings.
--
-- The string length limit depends on whether the hex string representation
-- is used or not. For text strings the limit is 64 bytes for the UTF8
-- representation of the text string. For byte strings the limit is 64 bytes
-- for the raw byte form (ie not the input hex, but after hex decoding).
--
-- In the \"detailed schema\" mapping, the idea is that we expose the full
-- representation capability of the script data in the form of a JSON schema.
-- This means the full representation is available and can be controlled
-- precisely. It also means any script data can be converted into the JSON and
-- back without loss. That is we can round-trip the script data via the JSON and
-- also round-trip schema-compliant JSON via script data.
--
data ScriptDataJsonSchema =

       -- | Use the \"no schema\" mapping between JSON and script data as
       -- described above.
       ScriptDataJsonNoSchema

       -- | Use the \"detailed schema\" mapping between JSON and script data as
       -- described above.
     | ScriptDataJsonDetailedSchema
  deriving (Eq, Show)


-- | Convert a value from JSON into script data, using the given choice of
-- mapping between JSON and script data.
--
-- This may fail with a conversion error if the JSON is outside the supported
-- subset for the chosen mapping. See 'ScriptDataJsonSchema' for the details.
--
scriptDataFromJson :: ScriptDataJsonSchema
                   -> Aeson.Value
                   -> Either ScriptDataJsonError HashableScriptData
scriptDataFromJson schema v = do
    d <- first (ScriptDataJsonSchemaError v) (scriptDataFromJson' v)
    first (ScriptDataRangeError v) (validateScriptData $ getScriptData d)
    return d
  where
    scriptDataFromJson' =
      case schema of
        ScriptDataJsonNoSchema       -> scriptDataFromJsonNoSchema
        ScriptDataJsonDetailedSchema -> scriptDataFromJsonDetailedSchema



-- | Convert a script data value into JSON , using the given choice of mapping
-- between JSON and script data.
--
-- This conversion is total but is not necessarily invertible.
-- See 'ScriptDataJsonSchema' for the details.
--
scriptDataToJson :: ScriptDataJsonSchema
                 -> HashableScriptData
                 -> Aeson.Value
scriptDataToJson schema =
    case schema of
      ScriptDataJsonNoSchema       -> scriptDataToJsonNoSchema
      ScriptDataJsonDetailedSchema -> scriptDataToJsonDetailedSchema


-- ----------------------------------------------------------------------------
-- JSON conversion using the the "no schema" style
--

scriptDataToJsonNoSchema :: HashableScriptData -> Aeson.Value
scriptDataToJsonNoSchema = conv . getScriptData
  where
    conv :: ScriptData -> Aeson.Value
    conv (ScriptDataNumber n) = Aeson.Number (fromInteger n)
    conv (ScriptDataBytes bs)
      | Right s <- Text.decodeUtf8' bs
      , Text.all Char.isPrint s
      = Aeson.String s

      | otherwise
      = Aeson.String (bytesPrefix <> Text.decodeLatin1 (Base16.encode bs))

    conv (ScriptDataList  vs) = Aeson.Array (Vector.fromList (map conv vs))
    conv (ScriptDataMap  kvs) = Aeson.object
                                  [ (convKey k, conv v)
                                  | (k, v) <- kvs ]

    conv (ScriptDataConstructor n vs) =
        Aeson.Array $
          Vector.fromList
           [ Aeson.Number (fromInteger n)
           , Aeson.Array (Vector.fromList (map conv vs))
           ]


    -- Script data allows any value as a key, not just string as JSON does.
    -- For simple types we just convert them to string directly.
    -- For structured keys we render them as JSON and use that as the string.
    convKey :: ScriptData -> Aeson.Key
    convKey (ScriptDataNumber n) = Aeson.fromText $ Text.pack (show n)
    convKey (ScriptDataBytes bs) = Aeson.fromText $ bytesPrefix
                                <> Text.decodeLatin1 (Base16.encode bs)
    convKey v                    = Aeson.fromText
                                 . Text.Lazy.toStrict
                                 . Aeson.Text.encodeToLazyText
                                 . conv
                                 $ v

scriptDataFromJsonNoSchema :: Aeson.Value
                           -> Either ScriptDataJsonSchemaError
                                     HashableScriptData
scriptDataFromJsonNoSchema = fmap (\sd -> HashableScriptData (serialiseToCBOR sd) sd) . conv
  where
    conv :: Aeson.Value
         -> Either ScriptDataJsonSchemaError ScriptData
    conv Aeson.Null   = Left ScriptDataJsonNullNotAllowed
    conv Aeson.Bool{} = Left ScriptDataJsonBoolNotAllowed

    conv (Aeson.Number d) =
      case Scientific.floatingOrInteger d :: Either Double Integer of
        Left  n -> Left (ScriptDataJsonNumberNotInteger n)
        Right n -> Right (ScriptDataNumber n)

    conv (Aeson.String s)
      | Just s' <- Text.stripPrefix bytesPrefix s
      , let bs' = Text.encodeUtf8 s'
      , Right bs <- Base16.decode bs'
      , not (BSC.any (\c -> c >= 'A' && c <= 'F') bs')
      = Right (ScriptDataBytes bs)

      | otherwise
      = Right (ScriptDataBytes (Text.encodeUtf8 s))

    conv (Aeson.Array vs) =
        fmap ScriptDataList
      . traverse conv
      $ Vector.toList vs

    conv (Aeson.Object kvs) =
        fmap ScriptDataMap
      . traverse (\(k,v) -> (,) (convKey k) <$> conv v)
      . List.sortOn fst
      . fmap (first Aeson.toText)
      $ KeyMap.toList kvs

    convKey :: Text -> ScriptData
    convKey s =
      fromMaybe (ScriptDataBytes (Text.encodeUtf8 s)) $
      parseAll ((fmap ScriptDataNumber pSigned <* Atto.endOfInput)
            <|> (fmap ScriptDataBytes  pBytes  <* Atto.endOfInput)) s

-- | JSON strings that are base16 encoded and prefixed with 'bytesPrefix' will
-- be encoded as CBOR bytestrings.
bytesPrefix :: Text
bytesPrefix = "0x"

data ScriptDataJsonBytesError
    = ScriptDataJsonBytesErrorValue ScriptDataJsonError
    | ScriptDataJsonBytesErrorInvalid ScriptDataRangeError
    deriving Show

instance Error ScriptDataJsonBytesError where
  displayError (ScriptDataJsonBytesErrorValue e) =
    "Error decoding ScriptData JSON value: " <> displayError e
  displayError (ScriptDataJsonBytesErrorInvalid e) =
    "ScriptData is invalid: " <> displayError e


-- | This allows us to take JSON formatted ScriptData and encode it in the CDDL format
-- whilst preserving the original bytes.
scriptDataJsonToHashable
  :: ScriptDataJsonSchema
  -> Aeson.Value -- ^ ScriptData Value
  -> Either ScriptDataJsonBytesError HashableScriptData
scriptDataJsonToHashable schema scriptDataVal = do
  sData <- first ScriptDataJsonBytesErrorValue $ scriptDataFromJson schema scriptDataVal
  first ScriptDataJsonBytesErrorInvalid $ validateScriptData $ getScriptData sData
  return sData

-- ----------------------------------------------------------------------------
-- JSON conversion using the "detailed schema" style
--

scriptDataToJsonDetailedSchema :: HashableScriptData -> Aeson.Value
scriptDataToJsonDetailedSchema = conv . getScriptData
  where
    conv :: ScriptData -> Aeson.Value
    conv (ScriptDataNumber n) = singleFieldObject "int"
                              . Aeson.Number
                              $ fromInteger n
    conv (ScriptDataBytes bs) = singleFieldObject "bytes"
                              . Aeson.String
                              $ Text.decodeLatin1 (Base16.encode bs)
    conv (ScriptDataList  vs) = singleFieldObject "list"
                              . Aeson.Array
                              $ Vector.fromList (map conv vs)
    conv (ScriptDataMap  kvs) = singleFieldObject "map"
                              . Aeson.Array
                              $ Vector.fromList
                                  [ Aeson.object [ ("k", conv k), ("v", conv v) ]
                                  | (k, v) <- kvs ]

    conv (ScriptDataConstructor n vs) =
      Aeson.object
        [ ("constructor", Aeson.Number (fromInteger n))
        , ("fields",      Aeson.Array (Vector.fromList (map conv vs)))
        ]

    singleFieldObject name v = Aeson.object [(name, v)]


scriptDataFromJsonDetailedSchema :: Aeson.Value
                                 -> Either ScriptDataJsonSchemaError
                                           HashableScriptData
scriptDataFromJsonDetailedSchema = fmap (\sd -> HashableScriptData (serialiseToCBOR sd) sd) . conv
  where
    conv :: Aeson.Value
         -> Either ScriptDataJsonSchemaError ScriptData
    conv (Aeson.Object m) =
      case List.sort $ KeyMap.toList m of
        [("int", Aeson.Number d)] ->
          case Scientific.floatingOrInteger d :: Either Double Integer of
            Left  n -> Left (ScriptDataJsonNumberNotInteger n)
            Right n -> Right (ScriptDataNumber n)

        [("bytes", Aeson.String s)]
          | Right bs <- Base16.decode (Text.encodeUtf8 s)
          -> Right (ScriptDataBytes bs)

        [("list", Aeson.Array vs)] ->
            fmap ScriptDataList
          . traverse conv
          $ Vector.toList vs

        [("map", Aeson.Array kvs)] ->
            fmap ScriptDataMap
          . traverse convKeyValuePair
          $ Vector.toList kvs

        [("constructor", Aeson.Number d),
         ("fields",      Aeson.Array vs)] ->
          case Scientific.floatingOrInteger d :: Either Double Integer of
            Left  n -> Left (ScriptDataJsonNumberNotInteger n)
            Right n -> fmap (ScriptDataConstructor n)
                     . traverse conv
                     $ Vector.toList vs

        (key, v):_ | key `elem` ["int", "bytes", "list", "map", "constructor"] ->
            Left (ScriptDataJsonTypeMismatch (Aeson.toText key) v)

        kvs -> Left (ScriptDataJsonBadObject $ first Aeson.toText <$> kvs)

    conv v = Left (ScriptDataJsonNotObject v)

    convKeyValuePair :: Aeson.Value
                     -> Either ScriptDataJsonSchemaError
                               (ScriptData, ScriptData)
    convKeyValuePair (Aeson.Object m)
      | KeyMap.size m == 2
      , Just k <- KeyMap.lookup "k" m
      , Just v <- KeyMap.lookup "v" m
      = (,) <$> conv k <*> conv v

    convKeyValuePair v = Left (ScriptDataJsonBadMapPair v)


-- ----------------------------------------------------------------------------
-- Shared JSON conversion error types
--

data ScriptDataJsonError =
       ScriptDataJsonSchemaError !Aeson.Value !ScriptDataJsonSchemaError
     | ScriptDataRangeError      !Aeson.Value !ScriptDataRangeError
  deriving (Eq, Show)

data ScriptDataJsonSchemaError =
       -- Only used for 'ScriptDataJsonNoSchema'
       ScriptDataJsonNullNotAllowed
     | ScriptDataJsonBoolNotAllowed

       -- Used by both mappings
     | ScriptDataJsonNumberNotInteger !Double

       -- Only used for 'ScriptDataJsonDetailedSchema'
     | ScriptDataJsonNotObject !Aeson.Value
     | ScriptDataJsonBadObject ![(Text, Aeson.Value)]
     | ScriptDataJsonBadMapPair !Aeson.Value
     | ScriptDataJsonTypeMismatch !Text !Aeson.Value
  deriving (Eq, Show)

instance Error ScriptDataJsonError where
    displayError (ScriptDataJsonSchemaError v detail) =
        "JSON schema error within the script data: "
     ++ LBS.unpack (Aeson.encode v) ++ "\n" ++ displayError detail
    displayError (ScriptDataRangeError v detail) =
        "Value out of range within the script data: "
     ++ LBS.unpack (Aeson.encode v) ++ "\n" ++ displayError detail

instance Error ScriptDataJsonSchemaError where
    displayError ScriptDataJsonNullNotAllowed =
        "JSON null values are not supported."
    displayError ScriptDataJsonBoolNotAllowed =
        "JSON bool values are not supported."
    displayError (ScriptDataJsonNumberNotInteger d) =
        "JSON numbers must be integers. Unexpected value: " ++ show d
    displayError (ScriptDataJsonNotObject v) =
        "JSON object expected. Unexpected value: "
     ++ LBS.unpack (Aeson.encode v)
    displayError (ScriptDataJsonBadObject v) =
        "JSON object does not match the schema.\nExpected a single field named "
     ++ "\"int\", \"bytes\", \"list\" or \"map\".\n"
     ++ "Unexpected object field(s): "
     ++ LBS.unpack (Aeson.encode (KeyMap.fromList $ first Aeson.fromText <$> v))
    displayError (ScriptDataJsonBadMapPair v) =
        "Expected a list of key/value pair { \"k\": ..., \"v\": ... } objects."
     ++ "\nUnexpected value: " ++ LBS.unpack (Aeson.encode v)
    displayError (ScriptDataJsonTypeMismatch k v) =
        "The value in the field " ++ show k ++ " does not have the type "
     ++ "required by the schema.\nUnexpected value: "
     ++ LBS.unpack (Aeson.encode v)

