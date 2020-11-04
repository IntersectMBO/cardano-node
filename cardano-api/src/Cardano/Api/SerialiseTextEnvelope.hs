{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | TextEnvelope Serialisation
--
module Cardano.Api.SerialiseTextEnvelope
  ( HasTextEnvelope(..)
  , TextEnvelope
  , TextEnvelopeType
  , TextEnvelopeDescr
  , TextEnvelopeError
  , serialiseToTextEnvelope
  , deserialiseFromTextEnvelope
  , readFileTextEnvelope
  , writeFileTextEnvelope
  , writeFileTextEnvelopeWithOwnerPermissions
  , readTextEnvelopeFromFile
  , readTextEnvelopeOfTypeFromFile
    -- * Reading one of several key types
  , FromSomeType(..)
  , deserialiseFromTextEnvelopeAnyOf
  , readFileTextEnvelopeAnyOf
  ) where

import           Prelude

import           Data.Bifunctor (first)
import           Data.Maybe (fromMaybe)
import           Data.String (IsString)
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.List as List
import qualified Data.Text as Text
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text

import qualified Data.Aeson as Aeson
import           Data.Aeson
                   (FromJSON (..), ToJSON (..), Value, object, withObject,
                    (.:), (.=))
import           Data.Aeson.Types (Parser)
import           Data.Aeson.Encode.Pretty
                   (Config (..), encodePretty', defConfig, keyOrder)

import           Control.Monad (unless)
import           Control.Exception (bracketOnError)
import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import           Control.Monad.Trans.Except.Extra
                   (hoistEither, firstExceptT, handleIOExceptT)

import           System.Directory (removeFile, renameFile)
import           System.FilePath (splitFileName, (<.>))
import           System.IO (hClose, openTempFile)

import           Cardano.Binary (DecoderError)

import           Cardano.Api.Error
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.SerialiseCBOR


-- ----------------------------------------------------------------------------
-- Text envelopes
--

type TextEnvelope = TextView
type TextEnvelopeType = TextViewType
type TextEnvelopeDescr = TextViewDescription
type TextEnvelopeError = TextViewError

newtype TextViewType
  = TextViewType { unTextViewType :: ByteString }
  deriving (Eq, IsString, Show, Semigroup)

newtype TextViewDescription
  = TextViewDescription { unTextViewDescription :: ByteString }
  deriving (Eq, IsString, Show, Semigroup)

-- | A 'TextView' is a structured envelope for serialised binary values
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
    object [ "type"        .= Text.decodeUtf8 tvType'
           , "description" .= Text.decodeUtf8 desc
           , "cborHex"     .= Text.decodeUtf8 (Base16.encode rawCBOR)
           ]

instance FromJSON TextView where
  parseJSON = withObject "TextView" $ \v -> TextView
                <$> (TextViewType . Text.encodeUtf8 <$> v .: "type")
                <*> (TextViewDescription . Text.encodeUtf8 <$> v .: "description")
                <*> (parseJSONBase16 =<< v .: "cborHex")

parseJSONBase16 :: Value -> Parser ByteString
parseJSONBase16 v = do
  t <- parseJSON v
  either fail return (Base16.decode (Text.encodeUtf8 t))

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
    TextViewFormatError err -> "TextView format error: " <> err

    TextViewTypeError [expType] actType ->
        "TextView type error: "
     <> " Expected: " <> Text.decodeLatin1 (unTextViewType expType)
     <> " Actual: " <> Text.decodeLatin1 (unTextViewType actType)

    TextViewTypeError expTypes actType ->
        "TextView type error: "
     <> " Expected one of: "
     <> Text.intercalate ", "
          [ Text.decodeLatin1 (unTextViewType expType) | expType <- expTypes ]
     <> " Actual: " <> Text.decodeLatin1 (unTextViewType actType)
    TextViewAesonDecodeError decErr -> "TextView aeson decode error: " <> Text.pack decErr
    TextViewDecodeError decErr -> "TextView decode error: " <> Text.pack (show decErr)


-- | Check that the \"type\" of the 'TextView' is as expected.
--
-- For example, one might check that the type is \"TxSignedShelley\".
--
expectTextViewOfType :: TextViewType -> TextView -> Either TextViewError ()
expectTextViewOfType expectedType tv =
    let actualType = tvType tv in
    unless (expectedType == actualType) $
      Left (TextViewTypeError [expectedType] actualType)


-- ----------------------------------------------------------------------------
-- Serialisation in text envelope format
--

class SerialiseAsCBOR a => HasTextEnvelope a where
    textEnvelopeType :: AsType a -> TextEnvelopeType

    textEnvelopeDefaultDescr :: a -> TextEnvelopeDescr
    textEnvelopeDefaultDescr _ = ""


instance Error TextViewError where
  displayError = Text.unpack . renderTextViewError


serialiseToTextEnvelope :: forall a. HasTextEnvelope a
                        => Maybe TextEnvelopeDescr -> a -> TextEnvelope
serialiseToTextEnvelope mbDescr a =
    TextView {
      tvType    = textEnvelopeType ttoken
    , tvDescription   = fromMaybe (textEnvelopeDefaultDescr a) mbDescr
    , tvRawCBOR = serialiseToCBOR a
    }
  where
    ttoken :: AsType a
    ttoken = proxyToAsType Proxy


deserialiseFromTextEnvelope :: HasTextEnvelope a
                            => AsType a
                            -> TextEnvelope
                            -> Either TextEnvelopeError a
deserialiseFromTextEnvelope ttoken te = do
    expectTextViewOfType (textEnvelopeType ttoken) te
    first TextViewDecodeError $
      deserialiseFromCBOR ttoken (tvRawCBOR te) --TODO: You have switched from CBOR to JSON


deserialiseFromTextEnvelopeAnyOf :: [FromSomeType HasTextEnvelope b]
                                 -> TextEnvelope
                                 -> Either TextEnvelopeError b
deserialiseFromTextEnvelopeAnyOf types te =
    case List.find matching types of
      Nothing ->
        Left (TextViewTypeError expectedTypes actualType)

      Just (FromSomeType ttoken f) ->
        first TextViewDecodeError $
          f <$> deserialiseFromCBOR ttoken (tvRawCBOR te)
  where
    actualType    = tvType te
    expectedTypes = [ textEnvelopeType ttoken
                    | FromSomeType ttoken _f <- types ]

    matching (FromSomeType ttoken _f) = actualType == textEnvelopeType ttoken


writeFileWithOwnerPermissions
  :: FilePath
  -> ByteString
  -> IO (Either (FileError ()) ())
writeFileWithOwnerPermissions targetPath a =
    bracketOnError
      (openTempFile targetDir $ targetFile <.> "tmp")
      (\(tmpPath, fHandle) -> do
        hClose fHandle >> removeFile tmpPath
        return . Left $ FileErrorTempFile targetPath tmpPath fHandle)
      (\(tmpPath, fHandle) -> do
          BS.hPut fHandle a
          hClose fHandle
          renameFile tmpPath targetPath
          return $ Right ())
  where
    (targetDir, targetFile) = splitFileName targetPath

writeFileTextEnvelope :: HasTextEnvelope a
                      => FilePath
                      -> Maybe TextEnvelopeDescr
                      -> a
                      -> IO (Either (FileError ()) ())
writeFileTextEnvelope path mbDescr a =
    runExceptT $ do
      handleIOExceptT (FileIOError path) $ BS.writeFile path content
  where
    content = textEnvelopeToJSON mbDescr a


writeFileTextEnvelopeWithOwnerPermissions
  :: HasTextEnvelope a
  => FilePath
  -> Maybe TextEnvelopeDescr
  -> a
  -> IO (Either (FileError ()) ())
writeFileTextEnvelopeWithOwnerPermissions targetPath mbDescr a =
  writeFileWithOwnerPermissions targetPath content
 where
  content = textEnvelopeToJSON mbDescr a


textEnvelopeToJSON :: HasTextEnvelope a =>  Maybe TextEnvelopeDescr -> a -> ByteString
textEnvelopeToJSON mbDescr a  =
  LBS.toStrict $ encodePretty' textViewJSONConfig
                               (serialiseToTextEnvelope mbDescr a)
              <> "\n"


readFileTextEnvelope :: HasTextEnvelope a
                     => AsType a
                     -> FilePath
                     -> IO (Either (FileError TextEnvelopeError) a)
readFileTextEnvelope ttoken path =
    runExceptT $ do
      content <- handleIOExceptT (FileIOError path) $ BS.readFile path
      firstExceptT (FileError path) $ hoistEither $ do
        te <- first TextViewAesonDecodeError $ Aeson.eitherDecodeStrict' content
        deserialiseFromTextEnvelope ttoken te


readFileTextEnvelopeAnyOf :: [FromSomeType HasTextEnvelope b]
                          -> FilePath
                          -> IO (Either (FileError TextEnvelopeError) b)
readFileTextEnvelopeAnyOf types path =
    runExceptT $ do
      content <- handleIOExceptT (FileIOError path) $ BS.readFile path
      firstExceptT (FileError path) $ hoistEither $ do
        te <- first TextViewAesonDecodeError $ Aeson.eitherDecodeStrict' content
        deserialiseFromTextEnvelopeAnyOf types te


readTextEnvelopeFromFile :: FilePath
                         -> IO (Either (FileError TextEnvelopeError) TextEnvelope)
readTextEnvelopeFromFile path =
  runExceptT $ do
    bs <- handleIOExceptT (FileIOError path) $
            BS.readFile path
    firstExceptT (FileError path . TextViewAesonDecodeError)
      . hoistEither $ Aeson.eitherDecodeStrict' bs


readTextEnvelopeOfTypeFromFile
  :: TextEnvelopeType
  -> FilePath
  -> IO (Either (FileError TextEnvelopeError) TextEnvelope)
readTextEnvelopeOfTypeFromFile expectedType path =
  runExceptT $ do
    te <- ExceptT (readTextEnvelopeFromFile path)
    firstExceptT (FileError path) $ hoistEither $
      expectTextViewOfType expectedType te
    return te

