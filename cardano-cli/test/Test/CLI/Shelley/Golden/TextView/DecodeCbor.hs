{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.TextView.DecodeCbor
  ( golden_shelleyTextViewDecodeCbor
  )
where

import           Cardano.Prelude

import           Hedgehog                       ( Property )

import qualified Test.OptParse                 as OP
import qualified System.IO                     as IO

{- HLINT ignore "Use camelCase" -}

golden_shelleyTextViewDecodeCbor :: Property
golden_shelleyTextViewDecodeCbor =
  OP.propertyOnce $ OP.workspace "tmp/decode-cbor" $ \tempDir -> do
    unsignedTxFile <- OP.noteInputFile "test/Test/golden/shelley/tx/unsigned.tx"
    decodedTxtFile <- OP.noteTempFile tempDir "decoded.txt"

    -- Defaults to signing a Mainnet transaction.

    decodedTxt <- OP.noteEvalM $ OP.execCardanoCLI
      ["shelley", "text-view", "decode-cbor", "--file", unsignedTxFile]

    liftIO $ IO.writeFile decodedTxtFile decodedTxt

    OP.assertFileOccurences 1 "# int(4999998000)" decodedTxtFile
    OP.assertFileOccurences 1 "# int(2000)" decodedTxtFile
    OP.assertFileOccurences 1 "# int(1000)" decodedTxtFile

    OP.assertEndsWithSingleNewline decodedTxtFile
    OP.assertFileLines (>= 10) decodedTxtFile
