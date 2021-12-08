{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Cardano.Benchmarking.Script.AesonLegacy
(
  parseScriptFileLegacy
, actionToJSON
, jsonToAction
)
where

import           Prelude
import           Data.Functor.Identity
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap (toList, lookup)
import           Data.Aeson
import           Data.Aeson.Types

import           Cardano.CLI.Types (SigningKeyFile(..))

import           Cardano.Benchmarking.Script.Aeson (parseJSONFile)
import           Cardano.Benchmarking.Script.Env
import           Cardano.Benchmarking.Script.Setters
import           Cardano.Benchmarking.Script.Store
import           Cardano.Benchmarking.Script.Types
import           Cardano.Benchmarking.Types (NumberOfTxs(..), TPSRate(..))

parseScriptFileLegacy :: FilePath -> IO [Action]
parseScriptFileLegacy = parseJSONFile (parse $ listParser jsonToAction)

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
  ImportGenesisFund submitMode (KeyName genesisKey) (KeyName fundKey)
    -> object ["importGenesisFund" .= genesisKey, "submitMode" .= submitMode, "fundKey" .= fundKey ]
  CreateChange submitMode payMode value count
    -> object ["createChange" .= value, "payMode" .= payMode, "submitMode" .= submitMode, "count" .= count ]
  RunBenchmark submitMode spendMode (ThreadName t) (NumberOfTxs txCount) (TPSRate tps)
    -> object ["runBenchmark" .= t, "submitMode" .= submitMode, "spendMode" .= spendMode, "txCount" .= txCount, "tps" .= tps]
  WaitBenchmark (ThreadName t) ->  singleton "waitBenchmark" t
  CancelBenchmark (ThreadName t) ->  singleton "cancelBenchmark" t
  WaitForEra era -> singleton "waitForEra" era
  Reserved l -> singleton "reserved" l
  other -> error $ "Action not supported in legacy JSON mode : " ++ show other
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
  (HashMap.lookup "importGenesisFund" -> Just v) -> parseImportGenesisFund v
  (HashMap.lookup "createChange"      -> Just v) -> parseCreateChange v
  (HashMap.lookup "runBenchmark"      -> Just v) -> parseRunBenchmark v
  (HashMap.lookup "waitBenchmark"     -> Just v) -> WaitBenchmark <$> parseThreadName v
  (HashMap.lookup "cancelBenchmark"   -> Just v) -> CancelBenchmark <$> parseThreadName v
  (HashMap.lookup "waitForEra"        -> Just v) -> WaitForEra <$> parseJSON v
  (HashMap.lookup "reserved"          -> Just v) -> Reserved <$> parseJSON v
  (HashMap.toList -> [(k, v)]                  ) -> parseSetter k v
  _ -> parseFail "Error: cannot parse action Object."
 where
  parseSetter k v = case k of
    (Text.stripPrefix "set" -> Just tag) -> do
        s <- tagParser $ object [ "tag" .= ("S" <> tag), "contents" .= v]
        return $ Set $ sumToTagged s
    _ -> parseFail $ "Error: cannot parse action Object with key " <> Text.unpack k
    where
      tagParser = genericParseJSON defaultOptions

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

  parseRunBenchmark v = RunBenchmark
    <$> parseField obj "submitMode"
    <*> parseField obj "spendMode"
    <*> ( ThreadName <$> parseJSON v )
    <*> ( NumberOfTxs <$> parseField obj "txCount" )
    <*> ( TPSRate <$> parseField obj "tps" )

  parseImportGenesisFund v = ImportGenesisFund
    <$> parseField obj "submitMode"
    <*> ( KeyName <$> parseJSON v )
    <*> parseKey "fundKey"

  parseCreateChange v = CreateChange
    <$> parseField obj "submitMode"
    <*> parseField obj "payMode"
    <*> parseJSON v
    <*> parseField obj "count"
