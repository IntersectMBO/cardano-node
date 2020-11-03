{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
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
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Data.Text as Text

import qualified Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty (encodePretty')

import           Control.Exception (bracketOnError)
import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import           Control.Monad.Trans.Except.Extra
                   (hoistEither, firstExceptT, handleIOExceptT)

import           System.Directory (removeFile, renameFile)
import           System.FilePath (splitFileName, (<.>))
import           System.IO (hClose, openTempFile)

import           Cardano.Api.Error
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.SerialiseCBOR
import qualified Cardano.Api.TextView as TextView


type TextEnvelope = TextView.TextView
type TextEnvelopeType = TextView.TextViewType
type TextEnvelopeDescr = TextView.TextViewDescription
type TextEnvelopeError = TextView.TextViewError

class SerialiseAsCBOR a => HasTextEnvelope a where
    textEnvelopeType :: AsType a -> TextEnvelopeType

    textEnvelopeDefaultDescr :: a -> TextEnvelopeDescr
    textEnvelopeDefaultDescr _ = ""


instance Error TextView.TextViewError where
  displayError = Text.unpack . TextView.renderTextViewError


serialiseToTextEnvelope :: forall a. HasTextEnvelope a
                        => Maybe TextEnvelopeDescr -> a -> TextEnvelope
serialiseToTextEnvelope mbDescr a =
    TextView.TextView {
      TextView.tvType    = textEnvelopeType ttoken
    , TextView.tvDescription   = fromMaybe (textEnvelopeDefaultDescr a) mbDescr
    , TextView.tvRawCBOR = serialiseToCBOR a
    }
  where
    ttoken :: AsType a
    ttoken = proxyToAsType Proxy


deserialiseFromTextEnvelope :: HasTextEnvelope a
                            => AsType a
                            -> TextEnvelope
                            -> Either TextEnvelopeError a
deserialiseFromTextEnvelope ttoken te = do
    TextView.expectTextViewOfType (textEnvelopeType ttoken) te
    first TextView.TextViewDecodeError $
      deserialiseFromCBOR ttoken (TextView.tvRawCBOR te) --TODO: You have switched from CBOR to JSON


deserialiseFromTextEnvelopeAnyOf :: [FromSomeType HasTextEnvelope b]
                                 -> TextEnvelope
                                 -> Either TextEnvelopeError b
deserialiseFromTextEnvelopeAnyOf types te =
    case List.find matching types of
      Nothing ->
        Left (TextView.TextViewTypeError expectedTypes actualType)

      Just (FromSomeType ttoken f) ->
        first TextView.TextViewDecodeError $
          f <$> deserialiseFromCBOR ttoken (TextView.tvRawCBOR te)
  where
    actualType    = TextView.tvType te
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
  LBS.toStrict $ encodePretty' TextView.textViewJSONConfig
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
        te <- first TextView.TextViewAesonDecodeError $ Aeson.eitherDecodeStrict' content
        deserialiseFromTextEnvelope ttoken te


readFileTextEnvelopeAnyOf :: [FromSomeType HasTextEnvelope b]
                          -> FilePath
                          -> IO (Either (FileError TextEnvelopeError) b)
readFileTextEnvelopeAnyOf types path =
    runExceptT $ do
      content <- handleIOExceptT (FileIOError path) $ BS.readFile path
      firstExceptT (FileError path) $ hoistEither $ do
        te <- first TextView.TextViewAesonDecodeError $ Aeson.eitherDecodeStrict' content
        deserialiseFromTextEnvelopeAnyOf types te


readTextEnvelopeFromFile :: FilePath
                         -> IO (Either (FileError TextEnvelopeError) TextEnvelope)
readTextEnvelopeFromFile path =
  runExceptT $ do
    bs <- handleIOExceptT (FileIOError path) $
            BS.readFile path
    firstExceptT (FileError path . TextView.TextViewAesonDecodeError)
      . hoistEither $ Aeson.eitherDecodeStrict' bs


readTextEnvelopeOfTypeFromFile
  :: TextEnvelopeType
  -> FilePath
  -> IO (Either (FileError TextEnvelopeError) TextEnvelope)
readTextEnvelopeOfTypeFromFile expectedType path =
  runExceptT $ do
    te <- ExceptT (readTextEnvelopeFromFile path)
    firstExceptT (FileError path) $ hoistEither $
      TextView.expectTextViewOfType expectedType te
    return te

