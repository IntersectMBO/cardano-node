{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Api.TextView
  ( -- * \"TextView\" format
    TextView (..)
  , TextViewError (..)
  , TextViewType (..)
  , TextViewTitle (..)
  , parseTextView
  , renderTextView
  , renderTextViewError
  , expectTextViewOfType
  , expectTextViewOfTypes
  , decodeFromTextView
  , encodeToTextView
  , textShow

    -- * File IO support
  , TextViewFileError (..)
  , readTextViewFile
  , readTextViewFileOfType
  , readTextViewEncodedFile
  , renderTextViewFileError
  , writeTextViewFile
  , writeTextViewEncodedFile

    -- * Exported for testing.
  , rawToMultilineHex
  , unRawToMultilineHex
  ) where

import           Cardano.Prelude

import           Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString.Base16 as Base16
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Char (isSpace)
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Cardano.Binary
import           Control.Monad.Trans.Except.Extra


newtype TextViewType
  = TextViewType { unTextViewType :: ByteString }
  deriving (Eq, IsString, Show, Semigroup)

newtype TextViewTitle
  = TextViewTitle { unTextViewTitle :: ByteString }
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
  , tvTitle :: !TextViewTitle
  , tvRawCBOR :: !ByteString
  } deriving (Eq, Show)

-- | The errors that the pure 'TextView' parsing\/decoding functions can return.
--
data TextViewError
  = TextViewFormatError !Text
  | TextViewTypeError   ![TextViewType] !TextViewType -- ^ expected, actual
  | TextViewDecodeError !DecoderError
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

    TextViewDecodeError decErr -> "TextView decode error: " <> textShow decErr

-- | Parse a 'TextView' from the external serialised format.
--
parseTextView :: ByteString -> Either TextViewError TextView
parseTextView =
  first (TextViewFormatError . Text.pack) . Atto.parseOnly pTextView


-- | Render a 'TextView' into the external serialised format.
--
renderTextView :: TextView -> ByteString
renderTextView tv =
  BS.unlines $
    [ "type: " <> unTextViewType (tvType tv)
    , "title: " <> unTextViewTitle (tvTitle tv)
    , "cbor-hex:"
    ]
    <> rawToMultilineHex (tvRawCBOR tv)

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


-- | Check that the \"type\" of the 'TextView' is one of a list of expected
-- types, and return which one it was.
--
expectTextViewOfTypes :: [(TextViewType, t)]
                      -> TextView
                      -> Either TextViewError t
expectTextViewOfTypes expectedTypes tv =
    let actualType = tvType tv in
    case List.lookup actualType expectedTypes of
      Nothing  -> throwError $ TextViewTypeError (map fst expectedTypes)
                                                 actualType
      Just tag -> return tag


-- | Decode the body of a 'TextView' with a CBOR 'Decoder'.
--
decodeFromTextView :: (forall s . Decoder s a)
                   -> TextView -> Either TextViewError a
decodeFromTextView decoder tv =
    first TextViewDecodeError $
      decodeFullDecoder errlabel decoder (LBS.fromStrict $ tvRawCBOR tv)
  where
    errlabel :: Text
    errlabel = Text.decodeLatin1 (unTextViewType $ tvType tv)

-- | Encode a value to a 'TextView' using a CBOR encoder. The type and title
-- fields must also be specified.
--
encodeToTextView :: TextViewType -> TextViewTitle -> (a -> Encoding)
                 -> a -> TextView
encodeToTextView tvType tvTitle encode a =
  TextView
    { tvType
    , tvTitle
    , tvRawCBOR = serializeEncoding' (encode a)
    }


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

-- | Read a file in the external serialised format for 'TextView'.
--
readTextViewFile :: FilePath -> IO (Either TextViewFileError TextView)
readTextViewFile path =
    runExceptT $ do
      bs <- handleIOExceptT (TextViewFileIOError path) $
              BS.readFile path
      firstExceptT (TextViewFileError path) $ hoistEither $
        parseTextView bs


-- | Read a file in the external serialised format for 'TextView', but use
-- 'expectTextViewOfType' to check that the file is declared to be of the
-- expected type.
--
readTextViewFileOfType :: TextViewType -> FilePath
                       -> IO (Either TextViewFileError TextView)
readTextViewFileOfType expectedType path =
    runExceptT $ do
      tv <- ExceptT $ readTextViewFile path
      firstExceptT (TextViewFileError path) $ hoistEither $
        expectTextViewOfType expectedType tv
      return tv


-- | Read a file in the external serialised format for 'TextView', check
-- that it's declared to be the expected type, and decode the body.
--
readTextViewEncodedFile :: (TextView -> Either TextViewError a)
                        -> FilePath -> IO (Either TextViewFileError a)
readTextViewEncodedFile decoder path =
    runExceptT $ do
      tv <- ExceptT $ readTextViewFile path
      firstExceptT (TextViewFileError path) $ hoistEither $ decoder tv


-- | Write a file in the external serialised format for 'TextView'.
--
writeTextViewFile :: FilePath -> TextView -> IO (Either TextViewFileError ())
writeTextViewFile path tv =
    runExceptT $
      handleIOExceptT (TextViewFileIOError path) $
        BS.writeFile path (renderTextView tv)


-- | Write a file in the external serialised format for 'TextView'.
-- Use supplied encoder to encode the value in TextView's tvRawCBOR field.
--
writeTextViewEncodedFile :: (a -> TextView) -> FilePath -> a
                         -> IO (Either TextViewFileError ())
writeTextViewEncodedFile encode path a =
    runExceptT $
      handleIOExceptT (TextViewFileIOError path) $
        BS.writeFile path (renderTextView tv)
  where
    tv = encode a


-- ----------------------------------------------------------------------------

chunksOf :: ByteString -> [ByteString]
chunksOf bs =
  case BS.splitAt 80 bs of
    (leading, "") -> [leading]
    (leading, trailing) -> leading : chunksOf trailing

pTextView :: Parser TextView
pTextView = do
  typ <- Atto.string "type: " *> Atto.takeWhile (/= '\n') <* Atto.endOfLine
  title <- Atto.string "title: " *> Atto.takeWhile (/= '\n') <* Atto.endOfLine
  hex <- Atto.string "cbor-hex:\n" *> Atto.takeByteString <* Atto.endOfInput
  case Base16.decode . BS.concat . map (BS.dropWhile isSpace) $ BS.lines hex of
    (raw, "") -> pure $ TextView (TextViewType typ) (TextViewTitle title) raw
    (_, err) -> panic $ "pTextView: Base16.deocde failed on " <> textShow err

-- | Convert a raw ByteString to hexadecimal and then line wrap
rawToMultilineHex :: ByteString -> [ByteString]
rawToMultilineHex = map (BS.cons ' ') . chunksOf . Base16.encode

textShow :: Show a => a -> Text
textShow = Text.pack . show

-- | Convert from multiline hexadecimal to a raw ByteString.
unRawToMultilineHex :: ByteString -> Either TextViewError ByteString
unRawToMultilineHex bs =
  case Base16.decode $ BS.concat (map (BS.dropWhile isSpace) $ BS.lines bs) of
    (raw, "") -> Right raw
    (_, err) -> Left $ TextViewFormatError ("unRawToMultilineHex: Unable to decode " <> textShow err)
