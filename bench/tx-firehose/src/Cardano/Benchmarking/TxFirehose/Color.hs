{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

-- | Colours that tag a firehose's transactions, so a mempool observer can tell
-- whose load a mempool is holding.
module Cardano.Benchmarking.TxFirehose.Color
  ( Color (..)
  , ColorSpec (..)
  , parseColorSpec
  , resolveColor
  , colorHex
  , colorBytes
  , colorFromBytes
  , colorFromOctets
  , colorSwatch
  , colorMetadataLabel
  ) where

import Cardano.Api (PaymentKey, VerificationKey, serialiseToRawBytes, verificationKeyHash)
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

-- | What @--color@ asked for: a literal colour, or one derived from the key.
data ColorSpec
  = ColorLiteral !Color
  | ColorFromKey
  deriving (Eq, Show)

-- | Metadata label carrying the colour, named after the issue this was built for.
colorMetadataLabel :: Word64
colorMetadataLabel = 1022

-- | Parse @ff0000@, @#ff0000@ or @auto@.
parseColorSpec :: String -> Either String ColorSpec
parseColorSpec s
  | normalised == "auto" = Right ColorFromKey
  | length normalised == 6 && all isHexDigit normalised =
      Right (ColorLiteral (Color (octet 0) (octet 1) (octet 2)))
  | otherwise =
      Left ("not a colour: " ++ s ++ " (expected six hex digits or 'auto')")
 where
  normalised = map toLower (dropWhile (== '#') s)

  octet i = 16 * hexValue (normalised !! (2 * i)) + hexValue (normalised !! (2 * i + 1))

  hexValue c
    | c >= '0' && c <= '9' = fromIntegral (fromEnum c - fromEnum '0')
    | otherwise = fromIntegral (fromEnum c - fromEnum 'a' + 10)

-- | Resolve a spec against the key whose transactions will carry the colour.
resolveColor :: ColorSpec -> VerificationKey PaymentKey -> Color
resolveColor (ColorLiteral c) _ = c
resolveColor ColorFromKey vk = hueColor hue
 where
  -- Two bytes of the key hash pick a hue, while saturation and lightness stay
  -- fixed. Taking hash bytes as RGB directly would leave a good share of keys
  -- dark or muddy, which is exactly what makes colours hard to tell apart.
  hue = case BS.unpack (serialiseToRawBytes (verificationKeyHash vk)) of
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
