{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Raw binary serialisation
--
module Cardano.Api.SerialiseRaw
  ( RawBytesHexError(..)
  , SerialiseAsRawBytes(..)
  , SerialiseAsRawBytesError(..)
  , serialiseToRawBytesHex
  , deserialiseFromRawBytesHex
  , eitherDeserialiseFromRawBytes
  , serialiseToRawBytesHexText
  ) where

import           Data.Bifunctor (Bifunctor (..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import           Data.Data (typeRep)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Typeable (TypeRep, Typeable)

import           Cardano.Api.Error (Error, displayError)
import           Cardano.Api.HasTypeProxy
import           Prettyprinter (Pretty (..))

newtype SerialiseAsRawBytesError = SerialiseAsRawBytesError
  -- TODO We can do better than use String to carry the error message
  { unSerialiseAsRawBytesError :: String
  }
  deriving (Eq, Show)

class (HasTypeProxy a, Typeable a) => SerialiseAsRawBytes a where

  serialiseToRawBytes :: a -> ByteString

  deserialiseFromRawBytes :: AsType a -> ByteString -> Either SerialiseAsRawBytesError a

eitherDeserialiseFromRawBytes :: SerialiseAsRawBytes a => AsType a -> ByteString -> Either SerialiseAsRawBytesError a
eitherDeserialiseFromRawBytes = deserialiseFromRawBytes
{-# DEPRECATED eitherDeserialiseFromRawBytes "Use deserialiseFromRawBytes instead" #-}

serialiseToRawBytesHex :: SerialiseAsRawBytes a => a -> ByteString
serialiseToRawBytesHex = Base16.encode . serialiseToRawBytes

serialiseToRawBytesHexText :: SerialiseAsRawBytes a => a -> Text
serialiseToRawBytesHexText = Text.decodeUtf8 . serialiseToRawBytesHex

-- | The errors that the pure 'SerialiseAsRawBytes' parsing\/decoding functions can return.
data RawBytesHexError
  = RawBytesHexErrorBase16DecodeFail
      ByteString -- ^ original input
      String -- ^ error message
  | RawBytesHexErrorRawBytesDecodeFail
      ByteString                -- ^ original input
      TypeRep                   -- ^ expected type
      SerialiseAsRawBytesError  -- ^ error message
  deriving (Show)

instance Error RawBytesHexError where
  displayError = \case
    RawBytesHexErrorBase16DecodeFail input message ->
      "Expected Base16-encoded bytestring, but got " <> pretty (textOf input) <> "; "
      <> pretty message
    RawBytesHexErrorRawBytesDecodeFail input asType (SerialiseAsRawBytesError e) ->
      "Failed to deserialise " <> pretty (textOf input) <> " as " <> pretty (show asType)
      <> ". " <> pretty e
    where
      textOf bs = case Text.decodeUtf8' bs of
        Right t -> Text.unpack t
        Left _ -> show bs

deserialiseFromRawBytesHex
  :: SerialiseAsRawBytes a
  => AsType a -> ByteString -> Either RawBytesHexError a
deserialiseFromRawBytesHex proxy hex = do
  raw <- first (RawBytesHexErrorBase16DecodeFail hex) $ Base16.decode hex
  case deserialiseFromRawBytes proxy raw of
    Left e -> Left $ RawBytesHexErrorRawBytesDecodeFail hex (typeRep proxy) e
    Right a -> Right a
