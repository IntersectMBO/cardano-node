{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Shelley CLI option data types and functions for cryptographic keys.
module Cardano.CLI.Shelley.Key
  ( SigningKeyDecodeError (..)
  , readSigningKeyFile
  , readSigningKeyFileAnyOf
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra (handleIOExceptT)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text

import           Cardano.Api.TextView (TextViewError (..))
import           Cardano.Api.Typed

import           Cardano.CLI.Types

-- | Signing key decoding error.
data SigningKeyDecodeError
  = SigningKeyTextEnvelopeError !TextEnvelopeError
  -- ^ The provided data seems to be a valid text envelope, but some error
  -- occurred in extracting a valid signing key from it.
  | SigningKeyBech32DecodeError !Bech32DecodeError
  -- ^ The provided data is valid Bech32, but some error occurred in
  -- deserializing it to a signing key.
  | SigningKeyInvalidError
  -- ^ The provided data does not represent a valid signing key.
  deriving (Eq, Show)

-- | Render an error message for a 'SigningKeyDecodeError'.
renderSigningKeyDecodeError :: SigningKeyDecodeError -> Text
renderSigningKeyDecodeError err =
  case err of
    SigningKeyTextEnvelopeError textEnvErr ->
      Text.pack (displayError textEnvErr)
    SigningKeyBech32DecodeError decodeErr ->
      Text.pack (displayError decodeErr)
    SigningKeyInvalidError -> "Invalid signing key."

instance Error SigningKeyDecodeError where
  displayError = Text.unpack . renderSigningKeyDecodeError

-- | Read a signing key from a file.
--
-- The contents of the file can either be Bech32-encoded, hex-encoded, or in
-- the text envelope format.
readSigningKeyFile
  :: forall keyrole.
     ( HasTextEnvelope (SigningKey keyrole)
     , SerialiseAsBech32 (SigningKey keyrole)
     )
  => AsType keyrole
  -> SigningKeyFile
  -> IO (Either (FileError SigningKeyDecodeError) (SigningKey keyrole))
readSigningKeyFile asType (SigningKeyFile fp) =
    tryReadTextEnvelope (tryDeserialiseBech32 tryDeserialiseHex)
  where
    tryReadTextEnvelope
      :: (Text -> Either SigningKeyDecodeError (SigningKey keyrole))
      -> IO (Either (FileError SigningKeyDecodeError) (SigningKey keyrole))
    tryReadTextEnvelope tryBech32 = do
      readTextEnvRes <- readFileTextEnvelope (AsSigningKey asType) fp
      case readTextEnvRes of
        Right res -> pure (Right res)

        -- If there was an IO exception, return the error.
        Left (FileIOError _ ioEx) -> pure $ Left $ FileIOError fp ioEx

        Left (FileError _ textEnvErr) ->
          case textEnvErr of
            -- The input was valid text envelope, but there was a type mismatch
            -- error.
            TextViewTypeError _ _ ->
              pure $ Left $ FileError fp (SigningKeyTextEnvelopeError textEnvErr)

            _ -> do
              content <- runExceptT . handleIOExceptT (FileIOError fp)
                $ Text.readFile fp
              case content of
                Left err -> pure (Left err)
                Right c' -> pure $ first (FileError fp) (tryBech32 c')

    tryDeserialiseBech32
      :: (ByteString -> Either SigningKeyDecodeError (SigningKey keyrole))
      -> Text
      -> Either SigningKeyDecodeError (SigningKey keyrole)
    tryDeserialiseBech32 tryHex content =
      case deserialiseFromBech32 (AsSigningKey asType) content of
        Right res -> Right res

        -- The input was not valid Bech32. Attempt to deserialize it as hex.
        Left (Bech32DecodingError _) -> tryHex (Text.encodeUtf8 content)

        -- The input was valid Bech32, but some other error occurred.
        Left err -> Left $ SigningKeyBech32DecodeError err

    tryDeserialiseHex
      :: ByteString
      -> Either SigningKeyDecodeError (SigningKey keyrole)
    tryDeserialiseHex content =
      case deserialiseFromRawBytesHex (AsSigningKey asType) content of
        Just res -> Right res
        Nothing -> Left SigningKeyInvalidError

-- | Read a signing key from a file given that it is one of the provided types
-- of signing key.
--
-- The contents of the file can either be Bech32-encoded or in the text
-- envelope format.
readSigningKeyFileAnyOf
  :: forall b.
     [FromSomeType HasTextEnvelope b]
  -> [FromSomeType SerialiseAsBech32 b]
  -> SigningKeyFile
  -> IO (Either (FileError SigningKeyDecodeError) b)
readSigningKeyFileAnyOf textEnvTypes bech32Types (SigningKeyFile fp) =
    tryReadTextEnvelope tryDeserialiseBech32
  where
    tryReadTextEnvelope
      :: (Text -> Either SigningKeyDecodeError b)
      -> IO (Either (FileError SigningKeyDecodeError) b)
    tryReadTextEnvelope tryBech32 = do
      readTextEnvRes <- readFileTextEnvelopeAnyOf textEnvTypes fp
      case readTextEnvRes of
        Right res -> pure (Right res)

        -- If there was an IO exception, return the error.
        Left (FileIOError _ ioEx) -> pure $ Left $ FileIOError fp ioEx

        Left (FileError _ textEnvErr) ->
          case textEnvErr of
            -- The input was valid text envelope, but there was a type mismatch
            -- error.
            TextViewTypeError _ _ ->
              pure $ Left $ FileError fp (SigningKeyTextEnvelopeError textEnvErr)

            _ -> do
              content <- runExceptT . handleIOExceptT (FileIOError fp)
                $ Text.readFile fp
              case content of
                Left err -> pure (Left err)
                Right c' -> pure $ first (FileError fp) (tryBech32 c')

    tryDeserialiseBech32
      :: Text
      -> Either SigningKeyDecodeError b
    tryDeserialiseBech32 content =
      case deserialiseAnyOfFromBech32 bech32Types content of
        Right res -> Right res

        -- The input was not valid Bech32.
        Left (Bech32DecodingError _) -> Left SigningKeyInvalidError

        -- The input was valid Bech32, but some other error occurred.
        Left err -> Left $ SigningKeyBech32DecodeError err
