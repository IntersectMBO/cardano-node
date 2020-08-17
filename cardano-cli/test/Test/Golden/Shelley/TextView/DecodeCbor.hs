{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.TextView.DecodeCbor
  ( golden_shelleyTextViewDecodeCbor
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified System.IO as IO

{- HLINT ignore "Use camelCase" -}

golden_shelleyTextViewDecodeCbor :: Property
golden_shelleyTextViewDecodeCbor = propertyOnce $ moduleWorkspace "tmp" $ \tempDir -> do
  unsignedTxFile <- noteInputFile "test/data/golden/shelley/tx/unsigned.tx"
  decodedTxtFile <- noteTempFile tempDir "decoded.txt"

  -- Defaults to signing a Mainnet transaction.

  decodedTxt <- execCardanoCLI
    [ "shelley","text-view","decode-cbor"
    , "--file", unsignedTxFile
    ]

  liftIO $ IO.writeFile decodedTxtFile decodedTxt

  assertFileOccurences 1 "# int(4999998000)" decodedTxtFile
  assertFileOccurences 1 "# int(2000)" decodedTxtFile
  assertFileOccurences 1 "# int(1000)" decodedTxtFile

  assertEndsWithSingleNewline decodedTxtFile
  assertFileLines (>= 10) decodedTxtFile
