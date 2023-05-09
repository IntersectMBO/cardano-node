{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | DRep off-chain metadata
--
module Cardano.Api.DRepMetadata (
    -- * DRep off-chain metadata
    DRepMetadata(..),
    validateAndHashDRepMetadata,
    DRepMetadataValidationError(..),

    -- * Data family instances
    AsType(..),
    Hash(..),
  ) where

import           Data.Bifunctor (first)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Either.Combinators (maybeToRight)
import           Data.Text (Text)
import qualified Data.Text as Text

import           Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

import qualified Cardano.Crypto.Hash.Class as Crypto

import           Cardano.Api.Eras
import           Cardano.Api.Error
import           Cardano.Api.Hash
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Keys.Byron
import           Cardano.Api.Keys.Praos
import           Cardano.Api.Script
import           Cardano.Api.SerialiseJSON
import           Cardano.Api.SerialiseRaw
import           Cardano.Ledger.Crypto (StandardCrypto)

import qualified Cardano.Ledger.Keys as Shelley


-- ----------------------------------------------------------------------------
-- DRep metadata
--

-- | A representation of the required fields for off-chain drep metadata.
--
data DRepMetadata =
     DRepMetadata {

       -- | A name of up to 50 characters.
       drepName :: !Text

       -- | A description of up to 255 characters.
     , drepDescription :: !Text

       -- | A ticker of 3-5 characters, for a compact display of dreps in
       -- a wallet.
     , drepTicker :: !Text

       -- | A URL to a homepage with additional information about the drep.
       -- n.b. the spec does not specify a character limit for this field.
     , drepHomepage :: !Text
     }
  deriving (Eq, Show)

newtype instance Hash DRepMetadata =
                 DRepMetadataHash (Shelley.Hash StandardCrypto ByteString)
    deriving (Eq, Show)

instance HasTypeProxy DRepMetadata where
    data AsType DRepMetadata = AsDRepMetadata
    proxyToAsType _ = AsDRepMetadata

instance SerialiseAsRawBytes (Hash DRepMetadata) where
  serialiseToRawBytes (DRepMetadataHash h) = Crypto.hashToBytes h

  deserialiseFromRawBytes (AsHash AsDRepMetadata) bs =
    maybeToRight (SerialiseAsRawBytesError "Unable to deserialise Hash DRepMetadata") $
      DRepMetadataHash <$> Crypto.hashFromBytes bs

--TODO: instance ToJSON DRepMetadata where

instance FromJSON DRepMetadata where
  parseJSON =
      Aeson.withObject "DRepMetadata" $ \obj ->
        DRepMetadata
          <$> parseName obj
          <*> parseDescription obj
          <*> parseTicker obj
          <*> obj .: "homepage"

    where
      -- Parse and validate the drep metadata name from a JSON object.
      -- The name must be 50 characters or fewer.
      --
      parseName :: Aeson.Object -> Aeson.Parser Text
      parseName obj = do
        name <- obj .: "name"
        if Text.length name <= 50
          then pure name
          else fail $ "\"name\" must have at most 50 characters, but it has "
                    <> show (Text.length name)
                    <> " characters."

      -- Parse and validate the drep metadata description
      -- The description must be 255 characters or fewer.
      --
      parseDescription :: Aeson.Object -> Aeson.Parser Text
      parseDescription obj = do
        description <- obj .: "description"
        if Text.length description <= 255
          then pure description
          else fail $
                "\"description\" must have at most 255 characters, but it has "
            <> show (Text.length description)
            <> " characters."

      -- | Parse and validate the drep ticker description
      -- The ticker must be 3 to 5 characters long.
      --
      parseTicker :: Aeson.Object -> Aeson.Parser Text
      parseTicker obj = do
        ticker <- obj .: "ticker"
        let tickerLen = Text.length ticker
        if tickerLen >= 3 && tickerLen <= 5
          then pure ticker
          else fail $
                "\"ticker\" must have at least 3 and at most 5 "
            <> "characters, but it has "
            <> show (Text.length ticker)
            <> " characters."

-- | A drep metadata validation error.
data DRepMetadataValidationError
  = DRepMetadataJsonDecodeError !String
  | DRepMetadataInvalidLengthError
    -- ^ The length of the JSON-encoded drep metadata exceeds the
    -- maximum.
      !Int
      -- ^ Maximum byte length.
      !Int
      -- ^ Actual byte length.
  deriving Show

instance Error DRepMetadataValidationError where
  displayError (DRepMetadataJsonDecodeError errStr) = errStr
  displayError (DRepMetadataInvalidLengthError maxLen actualLen) =
        "DRep metadata must consist of at most "
    <> show maxLen
    <> " bytes, but it consists of "
    <> show actualLen
    <> " bytes."

-- | Decode and validate the provided JSON-encoded bytes as 'DRepMetadata'.
-- Return the decoded metadata and the hash of the original bytes.
--
validateAndHashDRepMetadata
  :: ByteString
  -> Either DRepMetadataValidationError (DRepMetadata, Hash DRepMetadata)
validateAndHashDRepMetadata bs
  | BS.length bs <= 512 = do
      md <- first DRepMetadataJsonDecodeError
                  (Aeson.eitherDecodeStrict' bs)
      let mdh = DRepMetadataHash (Crypto.hashWith id bs)
      return (md, mdh)
  | otherwise = Left $ DRepMetadataInvalidLengthError 512 (BS.length bs)
