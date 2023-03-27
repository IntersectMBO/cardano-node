{-# LANGUAGE DataKinds #-}

-- | JSON serialisation
--
module Cardano.Api.SerialiseJSON
  ( serialiseToJSON
  , ToJSON(..)
  , ToJSONKey
  , deserialiseFromJSON
  , prettyPrintJSON
  , FromJSON(..)
  , FromJSONKey
  , JsonDecodeError(..)
  , readFileJSON
  , writeFileJSON
  ) where

import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither)
import           Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey)
import qualified Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import           Cardano.Api.Error
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.IO (File (..), FileDirection (..))


newtype JsonDecodeError = JsonDecodeError String
  deriving (Eq, Show)

instance Error JsonDecodeError where
  displayError (JsonDecodeError err) = err


serialiseToJSON :: ToJSON a => a -> ByteString
serialiseToJSON = LBS.toStrict . Aeson.encode

prettyPrintJSON :: ToJSON a => a -> ByteString
prettyPrintJSON = LBS.toStrict . encodePretty

deserialiseFromJSON :: FromJSON a
                    => AsType a
                    -> ByteString
                    -> Either JsonDecodeError a
deserialiseFromJSON _proxy = either (Left . JsonDecodeError) Right
                           . Aeson.eitherDecodeStrict'


readFileJSON :: FromJSON a
             => AsType a
             -> File 'In
             -> IO (Either (FileError JsonDecodeError) a)
readFileJSON ttoken path =
  runExceptT $ do
    content <- handleIOExceptT (FileIOError (unFile path)) $ BS.readFile (unFile path)
    firstExceptT (FileError (unFile path)) $ hoistEither $
      deserialiseFromJSON ttoken content

writeFileJSON :: ToJSON a
              => File 'Out
              -> a
              -> IO (Either (FileError ()) ())
writeFileJSON path x =
    runExceptT $
      handleIOExceptT (FileIOError (unFile path)) $
        BS.writeFile (unFile path) (serialiseToJSON x)
