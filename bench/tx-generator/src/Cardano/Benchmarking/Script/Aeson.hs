{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
module Cardano.Benchmarking.Script.Aeson
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS (lines)
import qualified Data.ByteString.Lazy as BSL
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Prelude
import           System.Exit

import           Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Aeson.Parser as Aeson (json)
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.Yaml as Yaml (encode)

import           Cardano.Api
import           Cardano.Api.Shelley (ProtocolParameters)

import           Cardano.Benchmarking.Script.Types
import           Cardano.TxGenerator.Internal.Orphans ()
import           Cardano.TxGenerator.Types

testJSONRoundTrip :: [Action] -> Maybe String
testJSONRoundTrip l = case fromJSON $ toJSON l of
  Success r -> if l == r then Nothing else Just "compare: not equal"
  Error err -> Just err

prettyPrintOrdered :: ToJSON a => a -> BSL.ByteString
prettyPrintOrdered = encodePretty' conf
  where
    conf = defConfig {confCompare = compare}

prettyPrint :: [Action] -> BSL.ByteString
prettyPrint = encodePretty' conf
 where
  conf = defConfig {confCompare = keyOrder actionNames }
  actionNames :: [Text]
  actionNames =
    [ "startProtocol", "readSigningKey", "secureGenesisFund", "splitFund"
    , "splitFundToList", "delay", "prepareTxList"
    , "runBenchmark", "asyncBenchmark", "waitBenchmark", "cancelBenchmark"
    , "reserved" ]

prettyPrintYaml :: [Action] -> BSL.ByteString
prettyPrintYaml = BSL.fromStrict . Yaml.encode

jsonOptionsUnTaggedSum :: Options
jsonOptionsUnTaggedSum = defaultOptions { sumEncoding = ObjectWithSingleField }

deriving instance Generic TxGenTxParams
instance ToJSON TxGenTxParams where
  toJSON     = genericToJSON jsonOptionsUnTaggedSum
  toEncoding = genericToEncoding jsonOptionsUnTaggedSum
instance FromJSON TxGenTxParams where
  parseJSON = genericParseJSON jsonOptionsUnTaggedSum

-- FIXME: workaround instances
instance ToJSON (SigningKey PaymentKey) where
  toJSON = toJSON . serialiseToTextEnvelope Nothing
instance FromJSON (SigningKey PaymentKey) where
  parseJSON o = do
    te <- parseJSON o
    case deserialiseFromTextEnvelope (AsSigningKey AsPaymentKey) te of
      Right k   -> pure k
      Left err  -> fail $ show err

instance ToJSON ProtocolParametersSource where
  toJSON     = genericToJSON jsonOptionsUnTaggedSum
  toEncoding = genericToEncoding jsonOptionsUnTaggedSum
instance FromJSON ProtocolParametersSource where
  parseJSON = genericParseJSON jsonOptionsUnTaggedSum

-- Orphan instance used in the tx-generator
instance ToJSON ScriptData where
  toJSON = scriptDataToJson ScriptDataJsonNoSchema . unsafeHashableScriptData
instance FromJSON ScriptData where
  parseJSON v = case scriptDataFromJson ScriptDataJsonNoSchema v of
    Right r -> return $ getScriptData r
    Left err -> fail $ show err

instance ToJSON Generator where
  toJSON     = genericToJSON jsonOptionsUnTaggedSum
  toEncoding = genericToEncoding jsonOptionsUnTaggedSum
instance FromJSON Generator where
  parseJSON = genericParseJSON jsonOptionsUnTaggedSum

instance ToJSON SubmitMode where
  toJSON     = genericToJSON jsonOptionsUnTaggedSum
  toEncoding = genericToEncoding jsonOptionsUnTaggedSum
instance FromJSON SubmitMode where
  parseJSON = genericParseJSON jsonOptionsUnTaggedSum

instance ToJSON PayMode where
  toJSON     = genericToJSON jsonOptionsUnTaggedSum
  toEncoding = genericToEncoding jsonOptionsUnTaggedSum
instance FromJSON PayMode where
  parseJSON = genericParseJSON jsonOptionsUnTaggedSum

instance ToJSON ScriptBudget where
  toJSON     = genericToJSON jsonOptionsUnTaggedSum
  toEncoding = genericToEncoding jsonOptionsUnTaggedSum
instance FromJSON ScriptBudget where
  parseJSON = genericParseJSON jsonOptionsUnTaggedSum

instance ToJSON ScriptSpec where
  toJSON     = genericToJSON jsonOptionsUnTaggedSum
  toEncoding = genericToEncoding jsonOptionsUnTaggedSum
instance FromJSON ScriptSpec where
  parseJSON = genericParseJSON jsonOptionsUnTaggedSum

instance ToJSON Action where
  toJSON     = genericToJSON jsonOptionsUnTaggedSum
  toEncoding = genericToEncoding jsonOptionsUnTaggedSum
instance FromJSON Action where
  parseJSON = genericParseJSON jsonOptionsUnTaggedSum

scanScriptFile :: FilePath -> IO Aeson.Value
scanScriptFile filePath = do
  input <- BS.readFile filePath
  case Atto.parse Aeson.json input of
    Atto.Fail rest _context msg -> die errorMsg
      where
        consumed = BS.take (BS.length input - BS.length rest) input
        lineNumber = length $ BS.lines consumed
        errorMsg = concat [
            "error while parsing json value :\n"
          , "file :" , filePath , "\n"
          , "line number ", show lineNumber ,"\n"
          , "message : ", msg, "\n"
          ]
    Atto.Partial _ -> die $ concat [
            "error while parsing json value :\n"
          , "file :" , filePath , "\n"
          , "truncated input file\n"
          ]
--    Atto.Done extra _ | (not $ BS.null extra) -> die $ concat [
--            "error while parsing json value :\n"
--          , "file :" , filePath , "\n"
--          , "leftover data"
--          ]
    Atto.Done _ value -> return value

parseJSONFile :: (Aeson.Value -> Result x) -> FilePath -> IO x
parseJSONFile parser filePath = do
  value <- scanScriptFile filePath
  case parser value of
    Error err -> die err
    Success script -> return script

parseScriptFileAeson :: FilePath -> IO [Action]
parseScriptFileAeson = parseJSONFile fromJSON

readProtocolParametersFile :: FilePath -> IO ProtocolParameters
readProtocolParametersFile = parseJSONFile fromJSON
