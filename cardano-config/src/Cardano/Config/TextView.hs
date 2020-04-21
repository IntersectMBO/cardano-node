{-# LANGUAGE OverloadedStrings #-}
module Cardano.Config.TextView
  ( TextView (..)
  , TextViewError (..)
  , parseTextView
  , renderTextView

  -- Exported for testing.
  , rawToMultilineHex
  , unRawToMultilineHex
  ) where

import           Cardano.Prelude

import           Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString.Base16 as Base16
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Char (isSpace)

import qualified Data.Text as Text


newtype TextViewError
  = TextViewError { unTextViewError :: Text }
  deriving (Eq, Show)


data TextView = TextView
  { tvType :: !ByteString
  , tvTitle :: !ByteString
  , tvRawCBOR :: !ByteString
  } deriving (Eq, Show)

parseTextView :: ByteString -> Either TextViewError TextView
parseTextView =
  first (TextViewError . Text.pack) . Atto.parseOnly pTextView

renderTextView :: TextView -> ByteString
renderTextView tv =
  BS.unlines $
    [ "type: " <> tvType tv
    , "title: " <> tvTitle tv
    , "cbor-hex:"
    ]
    <> rawToMultilineHex (tvRawCBOR tv)
-- -------------------------------------------------------------------------------------------------

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
    (raw, "") -> pure $ TextView typ title raw
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
    (_, err) -> Left $ TextViewError ("unRawToMultilineHex: Unable to decode " <> textShow err)
