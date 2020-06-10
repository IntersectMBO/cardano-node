{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Api.MetaData
  ( MetaDataError
  , readJSONMetaData
  , renderMetaDataError
  ) where

import           Cardano.Prelude hiding (MetaData)
import           Prelude (String)

import           Control.Exception (IOException)
import qualified Control.Exception as Exception
import           Control.Monad.Fail (fail)
import           Data.Aeson
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as SC8
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.HashMap.Strict as HMS
import           Data.Scientific (floatingOrInteger)
import qualified Data.Vector as Vector

import           Cardano.Api.Error (textShow)
import           Shelley.Spec.Ledger.MetaData (MetaData(..), MetaDatum(..))


data MetaDataError
  = ConversionErrBoolNotAllowed
  | ConversionErrNullNotAllowed
  | ConversionErrNumberNotInteger
  | ConversionErrLongerThan64Bytes
  | DecodeErrJSON !FilePath !String
  | ReadFileErr !FilePath !String
  deriving (Eq, Ord, Show)


renderMetaDataError :: MetaDataError -> Text
renderMetaDataError err =
  case err of
    ConversionErrBoolNotAllowed -> "JSON Bool value is not allowed in MetaData"
    ConversionErrNullNotAllowed -> "JSON Null value is not allowed in MetaData"
    ConversionErrNumberNotInteger -> "Only integers are allowed in MetaData"
    ConversionErrLongerThan64Bytes -> "JSON string is longer than 64 bytes"
    DecodeErrJSON fp decErr ->
      "Error decoding JSON at: " <> textShow fp <> " Error: " <> textShow decErr
    ReadFileErr fp rErr ->
      "Error reading file at: " <> textShow fp <> " Error: " <> textShow rErr

-- No 'ToJSON' instance is written for 'MetaData'.
-- It does not make sense to JSON roundtrip test 'MetaData'
-- as 'String's and 'ByteString's are indistinguishable.
instance FromJSON MetaData where
  parseJSON v = MetaData <$> parseJSON v

instance FromJSON MetaDatum where
  parseJSON v =
    case valueToMetaDatum v of
      Right mDatum -> return mDatum
      Left decErr -> fail . show $ renderMetaDataError decErr

readJSONMetaData :: FilePath -> IO (Either MetaDataError MetaData)
readJSONMetaData fp = do
  eBs <- Exception.try $ LC8.readFile fp
  case eBs of
    Left ioEx -> return . Left . ReadFileErr fp $ handler ioEx
    Right bs -> return . first (DecodeErrJSON fp) $ eitherDecode' bs
 where
  handler :: IOException -> String
  handler e = "Cardano.Api.MetaData.readJSONMetaData: " <> displayException e


valueToMetaDatum :: Value -> Either MetaDataError MetaDatum
valueToMetaDatum Null = Left ConversionErrNullNotAllowed
valueToMetaDatum (Bool _) = Left ConversionErrBoolNotAllowed
valueToMetaDatum (Number sci) =
  case (floatingOrInteger sci :: Either Double Integer) of
    Left _ -> Left ConversionErrNumberNotInteger
    Right int -> Right $ I int
valueToMetaDatum (String txt) = byteStringOrText txt
valueToMetaDatum (Array vector) =
  case Vector.mapM valueToMetaDatum vector of
    Left err -> Left err
    Right vecDatums -> Right . List $ Vector.toList vecDatums
valueToMetaDatum (Object hm) =
  case mapM tupleToMetaDatum $ HMS.toList hm of
    Left err -> Left err
    Right tuples -> Right $ Map tuples

-- Helpers

tupleToMetaDatum :: (Text, Value) -> Either MetaDataError (MetaDatum, MetaDatum)
tupleToMetaDatum (k, v) = do
  metaKey <- valueToMetaDatum $ String k
  metaValue <- valueToMetaDatum v
  Right (metaKey, metaValue)

-- | If text is encoded in base64, we convert it to a 'ByteString'.
byteStringOrText :: Text -> Either MetaDataError MetaDatum
byteStringOrText txt =
  case B64.decodeBase64 utf8 of
    Left _ -> if SC8.length utf8 > 64
              then Left ConversionErrLongerThan64Bytes
              else Right $ S txt
    Right bs -> if SC8.length bs > 64
                then Left ConversionErrLongerThan64Bytes
                else Right $ B bs
 where
  utf8 = encodeUtf8 txt
