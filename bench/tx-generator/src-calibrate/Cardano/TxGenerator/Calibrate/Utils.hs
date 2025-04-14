{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.TxGenerator.Calibrate.Utils
       (module Cardano.TxGenerator.Calibrate.Utils)
       where

import           Cardano.Api

import           Cardano.TxGenerator.PlutusContext (PlutusBudgetSummary, readScriptData)
import           Cardano.TxGenerator.ProtocolParameters (ProtocolParameters)
import           Cardano.TxGenerator.Types

import           Control.Exception (SomeException (..), try)
import           Control.Monad
import           Data.Aeson (eitherDecodeFileStrict')
import           Data.Aeson.Encode.Pretty
import           Data.Bool (bool)
import qualified Data.ByteString.Lazy.Char8 as BSL (ByteString, putStrLn)
import           System.Directory (doesFileExist)
import           System.FilePath

import           Paths_tx_generator


-- Resolve protocol parameters, given an optionally specified file path.
-- 1. resolve from 'data/protocol-parameters-v10.json', if not specified (file assumed to exist)
-- 2. try to resolve from file path
-- 3. try to resolve file path from within 'data/' directory
readProtocolParametersOrDie :: Maybe FilePath -> IO ProtocolParameters
readProtocolParametersOrDie mFile =
  resolver mFile >>= eitherDecodeFileStrict' >>= either error pure
  where
    resolver Nothing  = getDataFileName ("data" </> "protocol-parameters-v10.json") >>= resolver . Just
    resolver (Just f) = doesFileExist f >>= bool (getDataFileName $ "data" </> f) (pure f)

summaryAndRedeermerOrDie :: String -> Either TxGenError (PlutusBudgetSummary, PlutusAutoBudget, b) -> (PlutusBudgetSummary, ScriptRedeemer)
summaryAndRedeermerOrDie scope =
  either
    (error . withScope . show)
    (\(summary, budget, _) -> (summary, autoBudgetRedeemer budget))
  where withScope s = concat [scope, ": ", s]

resolveRedeemer :: Either ScriptData TxGenPlutusParams -> IO (Either TxGenError HashableScriptData)
resolveRedeemer = resolveRedeemerQuiet False

resolveRedeemerQuiet :: Bool -> Either ScriptData TxGenPlutusParams -> IO (Either TxGenError HashableScriptData)
resolveRedeemerQuiet quiet = \case
  Left hsd -> do
    unless quiet $ putStrLn "--> a hard-coded redeemer has been provided"
    pure $ Right $ unsafeHashableScriptData hsd

  Right PlutusOn{..} -> do
    case plutusScript of
      -- it's a file path: we rely on a redeemer file that's been passed explicitly
      Right{} -> loader plutusRedeemer

      -- it's a built-in, either from the library or from scripts-fallback/
      -- 1. an explicitly passed in redeemer file takes precedence
      -- 2. a fallback redeemer is resolved from data/ by adding .redeemer.json
      -- NB: while scripts-fallback/ content might be used in production, data/ should *NEVER* be - it's for tx-generator development and testing only
      Left n -> do
         let fallbackName = "data" </> n <.> "redeemer" <.> "json"
         fileExists     <- or <$> forM plutusRedeemer doesFileExist
         fallbackFile   <- either (\SomeException{} -> Nothing) Just <$> try (getDataFileName fallbackName)
         loader $ if fileExists then plutusRedeemer else fallbackFile

  Right{} -> pure $ Left $ TxGenError "resolveRedeemer: no Plutus script defined"

  where
    loader = \case
      Just f -> do
        unless quiet $ putStrLn $ "--> will read redeemer from: " ++ f
        readScriptData f
      Nothing -> pure $ Left $ TxGenError "resolveRedeemer: no redeemer file resolved"

printScriptData :: ScriptData -> IO ()
printScriptData =
    BSL.putStrLn
  . encodePretty
  . scriptDataToJson ScriptDataJsonDetailedSchema
  . unsafeHashableScriptData

encodePrettySorted :: ToJSON a => a -> BSL.ByteString
encodePrettySorted = encodePretty' defConfig { confCompare = compare, confTrailingNewline = True, confIndent = Spaces 2 }

replaceAt :: Int -> a -> [a] -> [a]
replaceAt at new xs =
  let (pref, suff) = splitAt at xs
  in pref ++ new : drop 1 suff
