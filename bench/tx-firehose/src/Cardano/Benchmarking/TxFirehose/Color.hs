{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- | Colours that tag a firehose's transactions, so a mempool observer can tell
-- whose load a mempool is holding.
module Cardano.Benchmarking.TxFirehose.Color
  ( Color (..)
  , parseColor
  , colorFromPublicKey
  , colorHex
  , colorBytes
  , colorFromBytes
  , colorFromOctets
  , colorSwatch
  , colorMetadataLabel
  ) where

import Cardano.Api (PaymentKey, VerificationKey, serialiseToRawBytes)
import Cardano.Crypto.Hash qualified as Hash
import Cardano.Crypto.Hash.SHA256 (SHA256)
import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (isHexDigit, toLower)
import Data.Word (Word64, Word8)
import Text.Printf (printf)

-- | A 24-bit RGB colour.
data Color = Color
  { colorRed :: !Word8
  , colorGreen :: !Word8
  , colorBlue :: !Word8
  }
  deriving (Eq, Ord, Show)

-- | Metadata label carrying the colour, named after the issue this was built for.
colorMetadataLabel :: Word64
colorMetadataLabel = 1022

-- | Parse @ff0000@ or @#ff0000@. The default colour is derived from a key
-- (see 'colorFromPublicKey'), so the parser only has to cover the literal.
parseColor :: String -> Either String Color
parseColor s
  | length normalised == 6 && all isHexDigit normalised =
      Right (Color (octet 0) (octet 1) (octet 2))
  | otherwise =
      Left ("not a colour: " ++ s ++ " (expected six hex digits)")
 where
  normalised = map toLower (dropWhile (== '#') s)

  octet i = 16 * hexValue (normalised !! (2 * i)) + hexValue (normalised !! (2 * i + 1))

  hexValue c
    | c >= '0' && c <= '9' = fromIntegral (fromEnum c - fromEnum '0')
    | otherwise = fromIntegral (fromEnum c - fromEnum 'a' + 10)

-- | Derive a vivid colour from a payment verification key. SHA-256 over the
-- serialized key picks two bytes of hue; saturation and lightness stay fixed,
-- so the result is always vivid regardless of the key.
--
-- Hashing the public portion keeps a tool that only ever holds the pub key
-- (e.g. an observer) in sync with one that signs with the matching key.
colorFromPublicKey :: VerificationKey PaymentKey -> Color
colorFromPublicKey vk = hueColor hue
 where
  digest = Hash.hashToBytes (Hash.hashWith @SHA256 id (serialiseToRawBytes vk))
  hue = case BS.unpack digest of
    (hi : lo : _) -> 360 * fromIntegral (word16 hi lo) / 65536
    _ -> 0

  word16 hi lo = (fromIntegral hi `shiftL` 8) .|. fromIntegral lo :: Int

-- | A vivid colour at the given hue.
hueColor :: Double -> Color
hueColor h = Color r g b
 where
  (r, g, b) = hslToRgb h 0.85 0.55

-- | Hue in [0,360), saturation and lightness in [0,1].
hslToRgb :: Double -> Double -> Double -> (Word8, Word8, Word8)
hslToRgb h s l = (toOctet (r + m), toOctet (g + m), toOctet (b + m))
 where
  chroma = (1 - abs (2 * l - 1)) * s
  sector = h / 60
  x = chroma * (1 - abs (sector `fmod` 2 - 1))
  m = l - chroma / 2

  (r, g, b)
    | sector < 1 = (chroma, x, 0)
    | sector < 2 = (x, chroma, 0)
    | sector < 3 = (0, chroma, x)
    | sector < 4 = (0, x, chroma)
    | sector < 5 = (x, 0, chroma)
    | otherwise = (chroma, 0, x)

  fmod a n = a - n * fromIntegral (floor (a / n) :: Int)

  toOctet v = round (255 * max 0 (min 1 v))

-- | Six lowercase hex digits, no leading @#@.
colorHex :: Color -> String
colorHex (Color r g b) = printf "%02x%02x%02x" r g b

-- | The three bytes that go into transaction metadata.
colorBytes :: Color -> ByteString
colorBytes (Color r g b) = BS.pack [r, g, b]

-- | Read a colour back out of the three metadata bytes.
colorFromBytes :: ByteString -> Maybe Color
colorFromBytes = colorFromOctets . BS.unpack

-- | The wire format in one place: exactly three octets, red green blue.
colorFromOctets :: [Word8] -> Maybe Color
colorFromOctets = \case
  [r, g, b] -> Just (Color r g b)
  _ -> Nothing

-- | The colour itself, as a 24-bit background block for a terminal.
colorSwatch :: Color -> String
colorSwatch (Color r g b) = printf "\ESC[48;2;%d;%d;%dm   \ESC[0m" r g b
