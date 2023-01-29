{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.TxSubmit.Config
  ( TxSubmitNodeConfig
  , GenTxSubmitNodeConfig (..)
  , readTxSubmitNodeConfig
  , ToggleLogging(..)
  , ToggleMetrics(..)
  ) where


import           Cardano.Api

import           Control.Exception (IOException, catch)
import           Data.Aeson (FromJSON (..), Object, Value (..), (.:))
import           Data.Aeson.Types (Parser)
import           Data.Bool (bool)
import           Data.ByteString (ByteString)
import           Protolude.Panic (panic)

import qualified Cardano.BM.Configuration as Logging
import qualified Cardano.BM.Configuration.Model as Logging
import qualified Cardano.BM.Data.Configuration as Logging
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Yaml as Yaml

type TxSubmitNodeConfig = GenTxSubmitNodeConfig Logging.Configuration

data ToggleLogging = LoggingOn | LoggingOff deriving (Eq, Show)
data ToggleMetrics = MetricsOn | MetricsOff deriving (Eq, Show)

data GenTxSubmitNodeConfig a = GenTxSubmitNodeConfig
  { tscLoggingConfig :: !a
  , tscToggleLogging :: !ToggleLogging
  , tscToggleMetrics :: !ToggleMetrics
  }

readTxSubmitNodeConfig :: FilePath -> IO TxSubmitNodeConfig
readTxSubmitNodeConfig fp = do
    res <- Yaml.decodeEither' <$> readLoggingConfig
    case res of
      Left err -> panic $ "readTxSubmitNodeConfig: Error parsing config: " <> textShow err
      Right icr -> convertLogging icr
  where
    readLoggingConfig :: IO ByteString
    readLoggingConfig =
      catch (B8.readFile fp) $ \(_ :: IOException) ->
        panic $ "Cannot find the logging configuration file at : " <> T.pack fp

convertLogging :: GenTxSubmitNodeConfig Logging.Representation -> IO TxSubmitNodeConfig
convertLogging tsc = do
  lc <- Logging.setupFromRepresentation $ tscLoggingConfig tsc
  pure $ tsc { tscLoggingConfig = lc }

---------------------------------------------------------------------------------------------------

instance FromJSON (GenTxSubmitNodeConfig Logging.Representation) where
  parseJSON = Aeson.withObject "top-level" parseGenTxSubmitNodeConfig

parseGenTxSubmitNodeConfig :: Object -> Parser (GenTxSubmitNodeConfig Logging.Representation)
parseGenTxSubmitNodeConfig o = GenTxSubmitNodeConfig
    <$> parseJSON (Object o)
    <*> fmap (bool LoggingOff LoggingOn) (o .: "EnableLogging")
    <*> fmap (bool MetricsOff MetricsOn) (o .: "EnableLogMetrics")
