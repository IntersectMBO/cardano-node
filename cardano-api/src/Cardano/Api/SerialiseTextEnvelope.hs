{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

-- | TextEnvelope Serialisation
--
module Cardano.Api.SerialiseTextEnvelope
  ( HasTextEnvelope(..)
  , TextEnvelope(..)
  , TextEnvelopeType(..)
  , TextEnvelopeDescr(..)
  , textEnvelopeRawCBOR
  , TextEnvelopeError(..)
  , serialiseToTextEnvelope
  , deserialiseFromTextEnvelopeCBOR
  , readFileTextEnvelope
  , writeFileTextEnvelope
  , writeFileTextEnvelopeWithOwnerPermissions
  , readTextEnvelopeFromFile
  , readTextEnvelopeOfTypeFromFile
  , textEnvelopeToJSON

    -- * Reading one of several key types
  , FromSomeType(..)
  , deserialiseFromTextEnvelopeAnyOfCBOR
  , readFileTextEnvelopeAnyOf

    -- * Data family instances
  , AsType(..)
  ) where

import           Prelude

import           Data.Bifunctor (first)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import           Data.Maybe (fromMaybe)
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text

import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty (Config (..), defConfig, encodePretty', keyOrder)

import           Control.Monad (unless)
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither)


import           Cardano.Binary (DecoderError)

import           Cardano.Api.Error
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.Utils (readFileBlocking)

#ifdef UNIX
import           Control.Exception (IOException, bracket, bracketOnError, try)
import           System.Directory ()
import           System.IO (hClose)
import           System.Posix.Files (ownerModes, setFdOwnerAndGroup)
import           System.Posix.IO (OpenMode (..), closeFd, defaultFileFlags, fdToHandle, openFd)
import           System.Posix.User (getRealUserID)
#else
import           Control.Exception (bracketOnError)
import           System.Directory (removeFile, renameFile)
import           System.FilePath (splitFileName, (<.>))
import           System.IO (hClose, openTempFile)
#endif


-- ----------------------------------------------------------------------------
-- Text envelopes
--

newtype TextEnvelopeType = TextEnvelopeType String
  deriving (Eq, Show)
  deriving newtype (IsString, Semigroup, ToJSON, FromJSON)

newtype TextEnvelopeDescr = TextEnvelopeDescr String
  deriving (Eq, Show)
  deriving newtype (IsString, Semigroup, ToJSON, FromJSON)

-- | A 'TextEnvelope' is a structured envelope for serialised binary values
-- with an external format with a semi-readable textual format.
--
-- It contains a \"type\" field, e.g. \"PublicKeyByron\" or \"TxSignedShelley\"
-- to indicate the type of the encoded data. This is used as a sanity check
-- and to help readers.
--
-- It also contains a \"title\" field which is free-form, and could be used
-- to indicate the role or purpose to a reader.
--
data TextEnvelope = TextEnvelope
  { teType        :: !TextEnvelopeType
  , teDescription :: !TextEnvelopeDescr
  , teRaw         :: !ByteString
  } deriving (Eq, Show)

instance HasTypeProxy TextEnvelope where
    data AsType TextEnvelope = AsTextEnvelope
    proxyToAsType _ = AsTextEnvelope

instance ToJSON TextEnvelope where
  toJSON TextEnvelope {teType, teDescription, teRaw} =
    object [ "type"        .= teType
           , "description" .= teDescription
           , "cborHex"     .= Text.decodeUtf8 (Base16.encode teRaw)
           ]

instance FromJSON TextEnvelope where
  parseJSON = withObject "TextEnvelope" $ \v ->
                TextEnvelope <$> (v .: "type")
                             <*> (v .: "description")
                             <*> (parseJSONBase16 =<< v .: "cborHex")
    where
      parseJSONBase16 v =
        either fail return . Base16.decode . Text.encodeUtf8 =<< parseJSON v

textEnvelopeJSONConfig :: Config
textEnvelopeJSONConfig = defConfig { confCompare = textEnvelopeJSONKeyOrder }

textEnvelopeJSONKeyOrder :: Text -> Text -> Ordering
textEnvelopeJSONKeyOrder = keyOrder ["type", "description", "cborHex"]


textEnvelopeRawCBOR :: TextEnvelope -> ByteString
textEnvelopeRawCBOR = teRaw


-- | The errors that the pure 'TextEnvelope' parsing\/decoding functions can return.
--
data TextEnvelopeError
  = TextEnvelopeTypeError   ![TextEnvelopeType] !TextEnvelopeType -- ^ expected, actual
  | TextEnvelopeDecodeError !DecoderError
  | TextEnvelopeAesonDecodeError !String
  deriving (Eq, Show)

instance Error TextEnvelopeError where
  displayError tee =
    case tee of
      TextEnvelopeTypeError [TextEnvelopeType expType]
                            (TextEnvelopeType actType) ->
          "TextEnvelope type error: "
       <> " Expected: " <> expType
       <> " Actual: " <> actType

      TextEnvelopeTypeError expTypes (TextEnvelopeType actType) ->
          "TextEnvelope type error: "
       <> " Expected one of: "
       <> List.intercalate ", "
            [ expType | TextEnvelopeType expType <- expTypes ]
       <> " Actual: " <> actType
      TextEnvelopeAesonDecodeError decErr -> "TextEnvelope aeson decode error: " <> decErr
      TextEnvelopeDecodeError decErr -> "TextEnvelope decode error: " <> show decErr


-- | Check that the \"type\" of the 'TextEnvelope' is as expected.
--
-- For example, one might check that the type is \"TxSignedShelley\".
--
expectTextEnvelopeOfType :: TextEnvelopeType -> TextEnvelope -> Either TextEnvelopeError ()
expectTextEnvelopeOfType expectedType TextEnvelope { teType = actualType } =
    unless (expectedType == actualType) $
      Left (TextEnvelopeTypeError [expectedType] actualType)


-- ----------------------------------------------------------------------------
-- Serialisation in text envelope format
--

class HasTextEnvelope a where
    textEnvelopeType :: AsType a -> TextEnvelopeType

    textEnvelopeDefaultDescr :: a -> TextEnvelopeDescr
    textEnvelopeDefaultDescr _ = ""


serialiseToTextEnvelope :: forall a. HasTextEnvelope a
                        => HasTypeProxy a
                        => (a -> ByteString) -> Maybe TextEnvelopeDescr -> a -> TextEnvelope
serialiseToTextEnvelope ser mbDescr a =
    TextEnvelope {
      teType    = textEnvelopeType ttoken
    , teDescription   = fromMaybe (textEnvelopeDefaultDescr a) mbDescr
    , teRaw = ser a
    }
  where
    ttoken :: AsType a
    ttoken = proxyToAsType Proxy


deserialiseFromTextEnvelopeCBOR
  :: HasTextEnvelope a
  => SerialiseAsCBOR a
  => AsType a
  -> TextEnvelope
  -> Either TextEnvelopeError a
deserialiseFromTextEnvelopeCBOR ttoken te = do
    expectTextEnvelopeOfType (textEnvelopeType ttoken) te
    first TextEnvelopeDecodeError $
      deserialiseFromCBOR ttoken (teRaw te) --TODO: You have switched from CBOR to JSON


deserialiseFromTextEnvelopeAnyOfCBOR
  :: [FromSomeType HasTextEnvelope SerialiseAsCBOR b]
  -> TextEnvelope
  -> Either TextEnvelopeError b
deserialiseFromTextEnvelopeAnyOfCBOR types te =
    case List.find matching types of
      Nothing ->
        Left (TextEnvelopeTypeError expectedTypes actualType)

      Just (FromSomeType ttoken f) ->
        first TextEnvelopeDecodeError $
          f <$> deserialiseFromCBOR ttoken (teRaw te)
  where
    actualType    = teType te
    expectedTypes = [ textEnvelopeType ttoken
                    | FromSomeType ttoken _f <- types ]

    matching (FromSomeType ttoken _f) = actualType == textEnvelopeType ttoken

writeFileWithOwnerPermissions
  :: FilePath
  -> LBS.ByteString
  -> IO (Either (FileError ()) ())
#ifdef UNIX
-- On a unix based system, we grab a file descriptor and set ourselves as owner.
-- Since we're holding the file descriptor at this point, we can be sure that
-- what we're about to write to is owned by us if an error didn't occur.
writeFileWithOwnerPermissions path a = do
    user <- getRealUserID
    ownedFile <- try $
      -- We only close the FD on error here, otherwise we let it leak out, since
      -- it will be immediately turned into a Handle (which will be closed when
      -- the Handle is closed)
      bracketOnError
        (openFd path WriteOnly (Just ownerModes) defaultFileFlags)
        closeFd
        (\fd -> setFdOwnerAndGroup fd user (-1) >> pure fd)
    case ownedFile of
      Left (err :: IOException) -> do
        pure $ Left $ FileIOError path err
      Right fd -> do
        bracket
          (fdToHandle fd)
          hClose
          (\handle -> runExceptT $ handleIOExceptT (FileIOError path) $ LBS.hPut handle a)
#else
-- On something other than unix, we make a _new_ file, and since we created it,
-- we must own it. We then place it at the target location. Unfortunately this
-- won't work correctly with pseudo-files.
writeFileWithOwnerPermissions targetPath a =
    bracketOnError
      (openTempFile targetDir $ targetFile <.> "tmp")
      (\(tmpPath, fHandle) -> do
        hClose fHandle >> removeFile tmpPath
        return . Left $ FileErrorTempFile targetPath tmpPath fHandle)
      (\(tmpPath, fHandle) -> do
          LBS.hPut fHandle a
          hClose fHandle
          renameFile tmpPath targetPath
          return $ Right ())
  where
    (targetDir, targetFile) = splitFileName targetPath
#endif

writeFileTextEnvelope :: HasTextEnvelope a
                      => HasTypeProxy a
                      => FilePath
                      -> (a -> ByteString)
                      -> Maybe TextEnvelopeDescr
                      -> a
                      -> IO (Either (FileError ()) ())
writeFileTextEnvelope path ser mbDescr a =
    runExceptT $ do
      handleIOExceptT (FileIOError path) $ LBS.writeFile path content
  where
    content = textEnvelopeToJSON ser mbDescr a


writeFileTextEnvelopeWithOwnerPermissions
  :: HasTextEnvelope a
  => HasTypeProxy a
  => FilePath
  -> Maybe TextEnvelopeDescr
  -> a
  -> IO (Either (FileError ()) ())
writeFileTextEnvelopeWithOwnerPermissions targetPath mbDescr a =
  writeFileWithOwnerPermissions targetPath content
 where
  content = textEnvelopeToJSON (error "serialiseToCBOR") mbDescr a

textEnvelopeToJSON
  :: HasTypeProxy a
  => HasTextEnvelope a
  => (a -> ByteString) -> Maybe TextEnvelopeDescr -> a -> LBS.ByteString
textEnvelopeToJSON ser mbDescr a  =
  encodePretty' textEnvelopeJSONConfig (serialiseToTextEnvelope ser mbDescr a) <> "\n"

readFileTextEnvelope :: HasTextEnvelope a
                     => SerialiseAsCBOR a
                     => AsType a
                     -> FilePath
                     -> IO (Either (FileError TextEnvelopeError) a)
readFileTextEnvelope ttoken path =
    runExceptT $ do
      content <- handleIOExceptT (FileIOError path) $ readFileBlocking path
      firstExceptT (FileError path) $ hoistEither $ do
        te <- first TextEnvelopeAesonDecodeError $ Aeson.eitherDecodeStrict' content
        deserialiseFromTextEnvelopeCBOR ttoken te


readFileTextEnvelopeAnyOf :: [FromSomeType HasTextEnvelope SerialiseAsCBOR b]
                          -> FilePath
                          -> IO (Either (FileError TextEnvelopeError) b)
readFileTextEnvelopeAnyOf types path =
    runExceptT $ do
      content <- handleIOExceptT (FileIOError path) $ readFileBlocking path
      firstExceptT (FileError path) $ hoistEither $ do
        te <- first TextEnvelopeAesonDecodeError $ Aeson.eitherDecodeStrict' content
        deserialiseFromTextEnvelopeAnyOfCBOR types te


readTextEnvelopeFromFile :: FilePath
                         -> IO (Either (FileError TextEnvelopeError) TextEnvelope)
readTextEnvelopeFromFile path =
  runExceptT $ do
    bs <- handleIOExceptT (FileIOError path) $
            readFileBlocking path
    firstExceptT (FileError path . TextEnvelopeAesonDecodeError)
      . hoistEither $ Aeson.eitherDecodeStrict' bs


readTextEnvelopeOfTypeFromFile
  :: TextEnvelopeType
  -> FilePath
  -> IO (Either (FileError TextEnvelopeError) TextEnvelope)
readTextEnvelopeOfTypeFromFile expectedType path =
  runExceptT $ do
    te <- ExceptT (readTextEnvelopeFromFile path)
    firstExceptT (FileError path) $ hoistEither $
      expectTextEnvelopeOfType expectedType te
    return te

