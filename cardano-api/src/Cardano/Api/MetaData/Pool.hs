module Cardano.Api.MetaData.Pool
  ( StakePoolMetadata
  , StakePoolMetadataValidationError (..)
  , decodeAndValidateStakePoolMetadata
  , renderStakePoolMetadataValidationError
  ) where

import           Cardano.Prelude
import           Prelude (String)

import           Control.Monad.Fail (fail)

import           Data.Aeson (FromJSON (..), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.Text as Text

-- | A representation of the required fields for stake pool metadata.
data StakePoolMetadata = StakePoolMetadata
  { _spmName :: !Text
    -- ^ A name of up to 50 characters.
  , _spmDescription :: !Text
    -- ^ A description of up to 255 characters.
  , _spmTicker :: !Text
    -- ^ A ticker of 3-5 characters, for a compact display of stake pools in
    -- a wallet.
  , _spmHomepage :: !Text
    -- ^ A URL to a homepage with additional information about the pool.
    -- n.b. the spec does not specify a character limit for this field.
  }

instance FromJSON StakePoolMetadata where
  parseJSON =
    Aeson.withObject "StakePoolMetadata" $ \obj ->
      StakePoolMetadata
        <$> parseName obj
        <*> parseDescription obj
        <*> parseTicker obj
        <*> obj .: "homepage"

-- | Parse and validate the stake pool metadata name from a JSON object.
--
-- If the name consists of more than 50 characters, the parser will fail.
parseName :: Aeson.Object -> Aeson.Parser Text
parseName obj = do
  name <- obj .: "name"
  if Text.length name <= 50
    then pure name
    else fail $
         "\"name\" must have at most 50 characters, but it has "
      <> show (Text.length name)
      <> " characters."

-- | Parse and validate the stake pool metadata description from a JSON
-- object.
--
-- If the description consists of more than 255 characters, the parser will
-- fail.
parseDescription :: Aeson.Object -> Aeson.Parser Text
parseDescription obj = do
  description <- obj .: "description"
  if Text.length description <= 255
    then pure description
    else fail $
         "\"description\" must have at most 255 characters, but it has "
      <> show (Text.length description)
      <> " characters."

-- | Parse and validate the stake pool ticker description from a JSON object.
--
-- If the ticker consists of less than 3 or more than 5 characters, the parser
-- will fail.
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

-- | A stake pool metadata validation error.
data StakePoolMetadataValidationError
  = StakePoolMetadataJsonDecodeError !String
  | StakePoolMetadataInvalidLengthError
    -- ^ The length of the JSON-encoded stake pool metadata exceeds the
    -- maximum.
      !Int
      -- ^ Maximum byte length.
      !Int
      -- ^ Actual byte length.
  deriving Show

renderStakePoolMetadataValidationError
  :: StakePoolMetadataValidationError
  -> Text
renderStakePoolMetadataValidationError err =
  case err of
    StakePoolMetadataJsonDecodeError errStr -> Text.pack errStr
    StakePoolMetadataInvalidLengthError maxLen actualLen ->
         "Stake pool metadata must consist of at most "
      <> show maxLen
      <> " bytes, but it consists of "
      <> show actualLen
      <> " bytes."

-- | Decode and validate the provided JSON-encoded 'ByteString' as
-- 'StakePoolMetadata'.
decodeAndValidateStakePoolMetadata
  :: ByteString
  -> Either StakePoolMetadataValidationError StakePoolMetadata
decodeAndValidateStakePoolMetadata bs
  | BS.length bs <= 512 =
      either
        (Left . StakePoolMetadataJsonDecodeError)
        Right
        (Aeson.eitherDecodeStrict' bs)
  | otherwise = Left $ StakePoolMetadataInvalidLengthError 512 (BS.length bs)
