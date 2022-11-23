{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module  Cardano.TxGenerator.PlutusContext
        ( module Cardano.TxGenerator.PlutusContext
        )
        where

import           Data.Aeson as Aeson
import           System.Exit
import           System.FilePath

import           Cardano.Api

import           Paths_tx_generator


-- load a redeemer for the script
-- TODO the redeemer serialization JSON file should be provided, not resolved inside tx-generator
readRedeemer :: FilePath -> IO ScriptData
readRedeemer scriptPath
  = do
    redeemer <- getDataFileName $ "data" </> redeemerFile
    sData :: Aeson.Value <-
      either die pure =<< eitherDecodeFileStrict' redeemer
    case scriptDataFromJson ScriptDataJsonDetailedSchema sData of
      Left e -> die (show e)
      Right sData' -> putStrLn ("--> read redeemer: " ++ redeemerFile) >> return sData'
  where
    redeemerFile = (<.> ".redeemer.json") $ dropExtension $ takeFileName scriptPath

-- adds a value to the first ScriptDataNumber encountered during traversal to the value provided
scriptDataAddToNumber :: Integer -> ScriptData -> ScriptData
scriptDataAddToNumber add
  = go
  where
    go = \case
      ScriptDataNumber i -> ScriptDataNumber (i + add)
      ScriptDataConstructor int list -> ScriptDataConstructor int (goList list)
      ScriptDataList list -> ScriptDataList (goList list)
      ScriptDataMap m ->
        let {(ks, vs) = unzip m; vs' = goList vs}
        in ScriptDataMap (zip ks vs')
      other -> other
    goList [] = []
    goList (x:xs) =
      let x' = go x in if x' == x then x : goList xs else x' : xs
