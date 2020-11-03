
-- | JSON serialisation
--
module Cardano.Api.SerialiseJSON
  ( serialiseToJSON
  , ToJSON(..)
  , deserialiseFromJSON
  , FromJSON(..)
  , JsonDecodeError(..)
  ) where

import           Prelude

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS

import           Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as Aeson

import           Cardano.Api.HasTypeProxy


newtype JsonDecodeError = JsonDecodeError String

serialiseToJSON :: ToJSON a => a -> ByteString
serialiseToJSON = LBS.toStrict . Aeson.encode

--TODO: add prettyPrintJSON :: ToJSON a => a -> ByteString

deserialiseFromJSON :: FromJSON a
                    => AsType a
                    -> ByteString
                    -> Either JsonDecodeError a
deserialiseFromJSON _proxy = either (Left . JsonDecodeError) Right
                           . Aeson.eitherDecodeStrict'

