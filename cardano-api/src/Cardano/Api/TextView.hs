{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Api.TextView
  ( -- * \"TextView\" format
    TextView (..)
  , TextViewError (..)
  , TextViewType (..)
  , TextViewDescription (..)
  , renderTextViewError
  , expectTextViewOfType
  , textViewJSONConfig
  , textViewJSONKeyOrder
  , textShow

    -- * File IO support
  , TextViewFileError (..)
  , renderTextViewFileError
  ) where

import           Cardano.Prelude
import           Prelude (String)

import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import           Data.Aeson.Encode.Pretty (Config (..), defConfig, keyOrder)
import qualified Data.ByteString.Base16 as Base16
import           Data.ByteString.Char8 (ByteString)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Cardano.Binary

newtype TextViewType
  = TextViewType { unTextViewType :: ByteString }
  deriving (Eq, IsString, Show, Semigroup)

newtype TextViewDescription
  = TextViewDescription { unTextViewDescription :: ByteString }
  deriving (Eq, IsString, Show, Semigroup)

-- | A 'TextView' is a structured envalope for serialised binary values
-- with an external format with a semi-readable textual format.
--
-- It contains a \"type\" field, e.g. \"PublicKeyByron\" or \"TxSignedShelley\"
-- to indicate the type of the encoded data. This is used as a sanity check
-- and to help readers.
--
-- It also contains a \"title\" field which is free-form, and could be used
-- to indicate the role or purpose to a reader.
--
data TextView = TextView
  { tvType :: !TextViewType
  , tvDescription :: !TextViewDescription
  , tvRawCBOR :: !ByteString
  } deriving (Eq, Show)

instance ToJSON TextView where
  toJSON (TextView (TextViewType tvType') (TextViewDescription desc) rawCBOR) =
    object [ "type" .= Text.decodeUtf8 tvType'
           , "description" .= Text.decodeUtf8 desc
           , "cborHex" .= (Text.decodeUtf8 $ Base16.encode rawCBOR)
           ]

instance FromJSON TextView where
  parseJSON = withObject "TextView" $ \v -> TextView
                <$> (TextViewType . Text.encodeUtf8 <$> v .: "type")
                <*> (TextViewDescription . Text.encodeUtf8 <$> v .: "description")
                <*> (fst . Base16.decode . Text.encodeUtf8 <$> v .: "cborHex")

textViewJSONConfig :: Config
textViewJSONConfig = defConfig { confCompare = textViewJSONKeyOrder }

textViewJSONKeyOrder :: Text -> Text -> Ordering
textViewJSONKeyOrder = keyOrder ["type", "description", "cborHex"]


-- | The errors that the pure 'TextView' parsing\/decoding functions can return.
--
data TextViewError
  = TextViewFormatError !Text
  | TextViewTypeError   ![TextViewType] !TextViewType -- ^ expected, actual
  | TextViewDecodeError !DecoderError
  | TextViewAesonDecodeError !String
  deriving (Eq, Show)

renderTextViewError :: TextViewError -> Text
renderTextViewError tve =
  case tve of
    TextViewFormatError err -> "TextView format error: " <> toS err

    TextViewTypeError [expType] actType ->
        "TextView type error: "
     <> " Expected: " <> Text.decodeLatin1 (unTextViewType expType)
     <> " Actual: " <> Text.decodeLatin1 (unTextViewType actType)

    TextViewTypeError expTypes actType ->
        "TextView type error: "
     <> " Expected one of: "
     <> Text.intercalate ", "
          [ Text.decodeLatin1 (unTextViewType expType) | expType <- expTypes ]
     <> " Actual: " <> (Text.decodeLatin1 (unTextViewType actType))
    TextViewAesonDecodeError decErr -> "TextView aeson decode error: " <> textShow decErr
    TextViewDecodeError decErr -> "TextView decode error: " <> textShow decErr

-- ----------------------------------------------------------------------------

-- | Check that the \"type\" of the 'TextView' is as expected.
--
-- For example, one might check that the type is \"TxSignedShelley\".
--
expectTextViewOfType :: TextViewType -> TextView -> Either TextViewError ()
expectTextViewOfType expectedType tv = do
    let actualType = tvType tv
    unless (expectedType == actualType) $
      throwError (TextViewTypeError [expectedType] actualType)


-- ----------------------------------------------------------------------------

-- | The errors that the IO 'TextView' reading\/decoding actions can return.
--
data TextViewFileError
  = TextViewFileError !FilePath !TextViewError
  | TextViewFileIOError !FilePath !IOException
  deriving (Eq, Show)

renderTextViewFileError :: TextViewFileError -> Text
renderTextViewFileError tvfe =
  case tvfe of
    TextViewFileError fp err -> toS fp <> ": " <> renderTextViewError err
    TextViewFileIOError fp ioExcpt ->
      "TextView IO exception at: " <> toS fp <> " Error: " <> textShow ioExcpt


-- ----------------------------------------------------------------------------

textShow :: Show a => a -> Text
textShow = Text.pack . show
