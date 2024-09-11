-- editorconfig-checker-disable-file
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Cardano.Api (File (..), PlutusScriptV3, PlutusScriptVersion (PlutusScriptV3),
                    Script (PlutusScript), writeFileTextEnvelope)
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised))
import Cardano.Constitution.Validator.Sorted (defaultConstitutionCode)
import PlutusLedgerApi.Common (serialiseCompiledCode)
import System.Environment (getArgs)
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  case args of
      [file] -> either (error . show) pure
          =<< writeFileTextEnvelope (File file) (Just "*BE CAREFUL* that this is compiled from a release commit of plutus and not from master") compiledScript
      _ -> die "USAGE: create-json-envelope OUT_FILE"

compiledScript :: Script PlutusScriptV3
compiledScript =
  PlutusScript PlutusScriptV3
    . PlutusScriptSerialised
    . serialiseCompiledCode
    $ defaultConstitutionCode
