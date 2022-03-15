{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.Benchmarking.Script.Aeson
where

import           Prelude
import           System.Exit
import           Data.Functor.Identity
import           Data.Text (Text)
import           Data.Dependent.Sum
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS (lines)
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.Attoparsec.ByteString as Atto

import qualified Ouroboros.Network.Magic as Ouroboros (NetworkMagic(..))
import           Cardano.Api (ScriptData, ScriptDataJsonSchema(..), NetworkId(..)
                              , scriptDataFromJson, scriptDataToJson)
import           Cardano.Api.Shelley (ProtocolParameters)
import           Cardano.CLI.Types (SigningKeyFile(..))

import           Cardano.Benchmarking.Script.Setters
import           Cardano.Benchmarking.Script.Store
import           Cardano.Benchmarking.Script.Types

testJSONRoundTrip :: [Action] -> Maybe String
testJSONRoundTrip l = case fromJSON $ toJSON l of
  Success r -> if l == r then Nothing else Just "compare: not equal"
  Error err -> Just err

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

jsonOptionsUnTaggedSum :: Options
jsonOptionsUnTaggedSum = defaultOptions { sumEncoding = ObjectWithSingleField }

instance ToJSON ProtocolParametersSource where
  toJSON     = genericToJSON jsonOptionsUnTaggedSum
  toEncoding = genericToEncoding jsonOptionsUnTaggedSum
instance FromJSON ProtocolParametersSource where
  parseJSON = genericParseJSON jsonOptionsUnTaggedSum

-- Orphan instance used in the tx-generator
instance ToJSON ScriptData where
  toJSON = scriptDataToJson ScriptDataJsonNoSchema
instance FromJSON ScriptData where
  parseJSON v = case scriptDataFromJson ScriptDataJsonNoSchema v of
    Right r -> return r
    Left err -> fail $ show err

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

instance ToJSON SpendMode where
  toJSON     = genericToJSON jsonOptionsUnTaggedSum
  toEncoding = genericToEncoding jsonOptionsUnTaggedSum
instance FromJSON SpendMode where
  parseJSON = genericParseJSON jsonOptionsUnTaggedSum

instance ToJSON ScriptBudget where
  toJSON     = genericToJSON jsonOptionsUnTaggedSum
  toEncoding = genericToEncoding jsonOptionsUnTaggedSum
instance FromJSON ScriptBudget where
  parseJSON = genericParseJSON jsonOptionsUnTaggedSum

instance ToJSON (DSum Tag Identity) where
  toJSON     = toJSON . taggedToSum
instance FromJSON (DSum Tag Identity) where
  parseJSON a = sumToTagged <$> parseJSON a

instance ToJSON Sum where
  toJSON     = genericToJSON jsonOptionsUnTaggedSum
  toEncoding = genericToEncoding jsonOptionsUnTaggedSum
instance FromJSON Sum where
  parseJSON = genericParseJSON jsonOptionsUnTaggedSum

instance ToJSON Action where
  toJSON     = genericToJSON jsonOptionsUnTaggedSum
  toEncoding = genericToEncoding jsonOptionsUnTaggedSum
instance FromJSON Action where
  parseJSON = genericParseJSON jsonOptionsUnTaggedSum

scanScriptFile :: FilePath -> IO Value
scanScriptFile filePath = do
  input <- BS.readFile filePath
  case Atto.parse Data.Aeson.json input of
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

parseJSONFile :: (Value -> Result x) -> FilePath -> IO x
parseJSONFile parser filePath = do
  value <- scanScriptFile filePath
  case parser value of
    Error err -> die err
    Success script -> return script

parseScriptFileAeson :: FilePath -> IO [Action]
parseScriptFileAeson = parseJSONFile fromJSON

readProtocolParametersFile :: FilePath -> IO ProtocolParameters
readProtocolParametersFile = parseJSONFile fromJSON

instance ToJSON KeyName         where toJSON (KeyName a) = toJSON a
instance ToJSON ThreadName      where toJSON (ThreadName a) = toJSON a
instance ToJSON WalletName      where toJSON (WalletName a) = toJSON a
instance ToJSON SigningKeyFile  where toJSON (SigningKeyFile a) = toJSON a

instance FromJSON KeyName         where parseJSON a = KeyName <$> parseJSON a
instance FromJSON ThreadName      where parseJSON a = ThreadName <$> parseJSON a
instance FromJSON WalletName      where parseJSON a = WalletName <$> parseJSON a
instance FromJSON SigningKeyFile  where parseJSON a = SigningKeyFile <$> parseJSON a

instance ToJSON NetworkId where
  toJSON Mainnet = "Mainnet"
  toJSON (Testnet (Ouroboros.NetworkMagic t)) = object ["Testnet" .= t]

instance FromJSON NetworkId where
  parseJSON j = case j of
    (String "Mainnet") -> return Mainnet
    (Object v) -> v .:? "Testnet" >>= \case
      Nothing -> failed
      Just w -> return $ Testnet $ Ouroboros.NetworkMagic w
    _invalid -> failed
    where
      failed = fail $ "Parsing of NetworkId failed: " <> show j
