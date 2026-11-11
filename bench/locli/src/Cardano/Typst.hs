{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StrictData #-}

-- | Typst rendering helpers shared by all Typst outputs: number formatting and
--   escaping. Document structure comes from the templates in report-templates/typst.
module Cardano.Typst
  ( Coloring (..)
  , colorCode
  , NumField
  , numFieldValue
  , renderNumField
  , nan
  , threshold
  , typstEscape
  )
where

import Cardano.Prelude
import Prelude                          (isNegativeZero)

import Data.Text                        qualified as T
import Data.Text.Lazy                   qualified as LT
import Data.Text.Lazy.Builder           qualified as LT (toLazyText)
import Data.Text.Lazy.Builder.RealFloat qualified as LT (FPFormat (..), formatRealFloat)


--
-- Number formatting
--

data Coloring = NoCol | Green | Red | Yellow deriving (Eq, Show)

-- | The function names have to be in sync with report-templates/typst/preamble.ede.
colorCode :: Coloring -> Text
colorCode NoCol = T.empty
colorCode Green = "gr"
colorCode Red   = "rd"
colorCode Yellow = "yw"

-- | A value with its rendering precision (derived from the 'Field' definition),
--   tagged for custom coloring to be used in the future. 'renderNumField' reproduces
--   the number formatting the reports have always used.
type NumField = ((Double, Maybe Int), Coloring)

numFieldValue :: NumField -> Double
numFieldValue = fst . fst

nan, threshold :: Double
nan       = 0 / 0
threshold = 0.0001

renderNumField :: NumField -> Text
renderNumField ((val, prec), _)
  | isNaN val               = "—" -- not available, or not defined
  | absval < threshold / 10 = zeroPaddedText
  | otherwise =
    let res = LT.toStrict $ LT.toLazyText $ LT.formatRealFloat format prec' val
    in if decimals > 3 && '.' `T.elem` res
      then keepDecimal $ T.dropWhileEnd (== '0') res
      else res
  where
    absval   = abs val
    decimals = fromMaybe 1 prec
    -- "34." -> "34.0"
    keepDecimal t = if "." `T.isSuffixOf` t then t <> "0" else t
    zeroPaddedText =
      let start = if val < 0.0 || isNegativeZero val then "-0" else "0"
      in if
        | decimals == 0 -> start
        | decimals > 3  -> start <> ".0"
        | otherwise     -> start <> "." <> T.replicate decimals "0"
    (format, prec') =
      let
        cutoff
          | decimals <= 5    = prec
          | absval >= 10000  = Just 0
          | absval >= 1000   = Just 1
          | absval >= 100    = Just 2
          | absval >= 10     = Just 3
          | otherwise        = Just 4
      in if absval < threshold then (LT.Exponent, Just 0) else (LT.Fixed, cutoff)


--
-- Escaping
--

-- | Escape text for verbatim display inside a Typst content block @[...]@:
--   every character Typst markup could interpret is backslash-escaped, so data
--   values (run names, filter descriptions, \"---\", ...) display exactly as given.
--   Apply to data only, before adding any intended markup.
typstEscape :: Text -> Text
typstEscape t = T.pack (go True (T.unpack t))
 where
   -- '-' and '/' are only markup in combination (dashes "--", soft hyphen "-?",
   -- a list item "- " at the start; comments "//", "/*"), so leave them bare
   -- otherwise: negative numbers, exponents and dates stay readable.
   go :: Bool -> [Char] -> [Char]
   go _ [] = []
   go start (c:cs)
     | c `elem` ("\\#[]*_`$<>@=+~'\"" :: [Char]) = '\\' : c : go False cs
     | c == '-', next `elem` ("-?" :: [Char]) || (start && next == ' ')
                                               = '\\' : c : go False cs
     | c == '/', next `elem` ("/*" :: [Char])  = '\\' : c : go False cs
     | otherwise                               =        c : go False cs
    where next = fromMaybe ' ' (head cs)
