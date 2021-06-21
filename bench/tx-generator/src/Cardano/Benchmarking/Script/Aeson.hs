{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Cardano.Benchmarking.Script.Aeson
where

import           Prelude
import           System.Exit
import           Data.Functor.Identity
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Dependent.Sum
import qualified Data.HashMap.Strict as HashMap (toList, lookup)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS (lines)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Aeson.Encode.Pretty
import qualified Data.Attoparsec.ByteString as Atto

import           Cardano.Api (AnyCardanoEra(..), CardanoEra(..))
import           Cardano.CLI.Types (SigningKeyFile(..))

import           Cardano.Benchmarking.Script.Action
import           Cardano.Benchmarking.Script.Env
import           Cardano.Benchmarking.Script.Setters
import           Cardano.Benchmarking.Script.Store
import           Cardano.Benchmarking.Types (TPSRate(..))

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

instance FromJSON AnyCardanoEra where
  parseJSON = withText "AnyCardanoEra" $ \case
    "Byron"   -> return $ AnyCardanoEra ByronEra
    "Shelley" -> return $ AnyCardanoEra ShelleyEra
    "Allegra" -> return $ AnyCardanoEra AllegraEra
    "Mary"    -> return $ AnyCardanoEra MaryEra
    era -> parseFail ("Error: Cannot parse JSON value '" <> Text.unpack era <> "' to AnyCardanoEra.")

instance ToJSON (DSum Tag Identity) where
  toEncoding = error "DSum Tag Identity"
  toJSON = error "DSum Tag Identity"

instance FromJSON (DSum Tag Identity) where
  parseJSON = error "fromJSON"

instance ToJSON Sum where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Sum

actionToJSON :: Action -> Value
actionToJSON a = case a of
  Set keyVal -> keyValToJSONCompact keyVal -- Remove the inner/ nested Object and add "set" -prefix.
  StartProtocol filePath -> singleton "startProtocol" filePath
  ReadSigningKey (KeyName name) (SigningKeyFile filePath)
    -> object ["readSigningKey" .= name, "filePath" .= filePath]
  SecureGenesisFund (FundName fundName) (KeyName fundKey) (KeyName genesisKey)
    -> object ["secureGenesisFund" .= fundName, "fundKey" .= fundKey, "genesisKey" .= genesisKey ]
  SplitFund newFunds (KeyName newKey) (FundName sourceFund)
    -> object ["splitFund" .= names, "newKey" .= newKey, "sourceFund" .= sourceFund]
    where names = [n | FundName n <- newFunds]
  SplitFundToList (FundListName fundList) (KeyName destKey) (FundName sourceFund)
    -> object ["splitFundToList" .= fundList, "newKey" .= destKey, "sourceFund" .= sourceFund ]
  Delay t -> object ["delay" .= t ]
  PrepareTxList (TxListName name) (KeyName key) (FundListName fund)
    -> object ["prepareTxList" .= name, "newKey" .= key, "fundList" .= fund ]
  AsyncBenchmark (ThreadName t) (TxListName txs) (TPSRate tps) 
    -> object ["asyncBenchmark" .= t, "txList" .= txs, "tps" .= tps]
  WaitBenchmark (ThreadName t) ->  singleton "waitBenchmark" t
  CancelBenchmark (ThreadName t) ->  singleton "cancelBenchmark" t
  WaitForEra era -> singleton "waitForEra" era
  Reserved l -> singleton "reserved" l
 where
  singleton k v = object [ k .= v ]

keyValToJSONCompact :: SetKeyVal -> Value
keyValToJSONCompact keyVal = case parseEither (withObject "internal Error" parseSum) v of
  Right c  -> c
  Left err -> error err
 where
  v = toJSON $ runIdentity $ taggedToSum keyVal
  parseSum obj = do
    key <- obj .: "tag"
    (val :: Value)  <- obj .: "contents"
    return $ object [("set" <> Text.tail key) .= val]

instance ToJSON Action where toJSON = actionToJSON
instance FromJSON Action where parseJSON = jsonToAction

jsonToAction :: Value -> Parser Action
jsonToAction = withObject "Error: Action is not a JSON object." objectToAction

objectToAction :: Object -> Parser Action
objectToAction obj = case obj of
  (HashMap.lookup "startProtocol"     -> Just v)
    -> (withText "Error parsing startProtocol" $ \t -> return $ StartProtocol $ Text.unpack t) v
  (HashMap.lookup "readSigningKey"    -> Just v) -> parseReadSigningKey v
  (HashMap.lookup "secureGenesisFund" -> Just v) -> parseSecureGenesisFund v
  (HashMap.lookup "splitFund"         -> Just v) -> parseSplitFund v
  (HashMap.lookup "splitFundToList"   -> Just v) -> parseSplitFundToList v
  (HashMap.lookup "delay"             -> Just v) -> Delay <$> parseJSON v
  (HashMap.lookup "prepareTxList"     -> Just v) -> parsePrepareTxList v
  (HashMap.lookup "asyncBenchmark"    -> Just v) -> parseAsyncBenchmark v
  (HashMap.lookup "waitBenchmark"     -> Just v) -> WaitBenchmark <$> parseThreadName v
  (HashMap.lookup "cancelBenchmark"   -> Just v) -> CancelBenchmark <$> parseThreadName v
  (HashMap.lookup "waitForEra"        -> Just v) -> WaitForEra <$> parseJSON v
  (HashMap.lookup "reserved"          -> Just v) -> Reserved <$> parseJSON v
  (HashMap.toList -> [(k, v)]                  ) -> parseSetter k v
  _ -> parseFail "Error: cannot parse action Object."
 where
  parseSetter k v = case k of
    (Text.stripPrefix "set" -> Just tag) -> do
        s <- parseJSON $ object [ "tag" .= ("S" <> tag), "contents" .= v]
        return $ Set $ sumToTaggged s
    _ -> parseFail $ "Error: cannot parse action Object with key " <> Text.unpack k

  parseKey f = KeyName <$> parseField obj f
  parseFund f = FundName <$> parseField obj f
  parseThreadName
    = withText "Error parsing ThreadName" $ \t -> return $ ThreadName $ Text.unpack t

  parseReadSigningKey v = ReadSigningKey
    <$> ( KeyName <$> parseJSON v )
    <*> ( SigningKeyFile <$> parseField obj "filePath" )

  parseSecureGenesisFund v = SecureGenesisFund
    <$> ( FundName <$> parseJSON v )
    <*> parseKey "fundKey"
    <*> parseKey "genesisKey"

  parseSplitFund v  = do
    l <- parseJSON v
    k <- parseKey "newKey"
    f <- parseFund "sourceFund"
    return $ SplitFund (map FundName l) k f

  parseSplitFundToList v = SplitFundToList
    <$> ( FundListName <$> parseJSON v )
    <*> parseKey "newKey"
    <*> parseFund "sourceFund"

  parsePrepareTxList v = PrepareTxList
    <$> ( TxListName <$> parseJSON v )
    <*> parseKey "newKey"
    <*> ( FundListName <$>parseField obj "fundList" )

  parseAsyncBenchmark v = AsyncBenchmark
    <$> ( ThreadName <$> parseJSON v )
    <*> ( TxListName <$> parseField obj "txList" )
    <*> ( TPSRate <$> parseField obj "tps" )   

parseScriptFile :: FilePath -> IO [Action]
parseScriptFile filePath = do
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
    Atto.Done _ value -> case fromJSON value of
      Error err -> die err
      Success script -> return script
