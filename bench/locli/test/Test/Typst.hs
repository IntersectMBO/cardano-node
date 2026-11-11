{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Number formatting and escaping of Typst output: the formatting keeps reports
--   comparable with past ones, the escaping keeps data values displayed verbatim.
module Test.Typst where

import           Cardano.Prelude

import           Cardano.Typst

import           Hedgehog


num :: Double -> Maybe Int -> Text
num v prec = renderNumField ((v, prec), NoCol)

prop_renderNumField_examples = withTests 1 $ property $ do
  -- not available
  num nan (Just 2)          === "—"
  -- zero, padded to precision
  num 0 (Just 0)            === "0"
  num 0 (Just 2)            === "0.00"
  num (-0.0) (Just 2)       === "-0.00"
  num 0 (Just 5)            === "0.0"
  num 0.000001 (Just 2)     === "0.00"
  -- below the threshold: exponent format
  num 0.00005 (Just 2)      === "5.0e-5"
  -- fixed precision
  num 1.5 (Just 1)          === "1.5"
  num (-2.5) (Just 1)       === "-2.5"
  num 1.23456 (Just 3)      === "1.235"
  -- precision > 3: trailing zeros dropped, but a decimal kept
  num 34 (Just 5)           === "34.0"
  num 1.25 (Just 5)         === "1.25"
  -- precision > 5: cut off by magnitude
  num 12345.678 (Just 6)    === "12346"
  num 123.456789 (Just 6)   === "123.46"
  num 1.23456789 (Just 6)   === "1.2346"

prop_typstEscape_examples = withTests 1 $ property $ do
  -- dates, negative numbers, plain slashes and dashes stay bare
  typstEscape "2026-09-28"  === "2026-09-28"
  typstEscape "-1.5"        === "-1.5"
  typstEscape "a/b"         === "a/b"
  typstEscape "a - b"       === "a - b"
  -- dashes, soft hyphen, list item at the start
  typstEscape "---"         === "\\-\\--"
  typstEscape "x -? y"      === "x \\-? y"
  typstEscape "- item"      === "\\- item"
  typstEscape "a - item"    === "a - item"
  -- comments
  typstEscape "a // b"      === "a \\// b"
  typstEscape "/*x"         === "\\/\\*x"
  -- always escaped
  typstEscape "a_b #c $d"   === "a\\_b \\#c \\$d"
  typstEscape "[x] <y> @z"  === "\\[x\\] \\<y\\> \\@z"
  typstEscape "it's \"q\""  === "it\\'s \\\"q\\\""
  typstEscape "a\\b"        === "a\\\\b"
  typstEscape "= h"         === "\\= h"
  typstEscape "a+b~c`d`*e*" === "a\\+b\\~c\\`d\\`\\*e\\*"

tests :: IO Bool
tests =
  checkSequential $$discover
