
-- | JSON serialisation
--
module Cardano.Api.SerialiseJSON
  ( serialiseToJSON
  , ToJSON(..)
  , deserialiseFromJSON
  , FromJSON(..)
  , JsonDecodeError(..)
  , readFileJSON
  , writeFileJSON
  ) where

import           Prelude

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import           Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as Aeson

import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad.Trans.Except.Extra
                   (hoistEither, firstExceptT, handleIOExceptT)

import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Error


newtype JsonDecodeError = JsonDecodeError String
  deriving (Eq, Show)

instance Error JsonDecodeError where
  displayError (JsonDecodeError err) = err


serialiseToJSON :: ToJSON a => a -> ByteString
serialiseToJSON = LBS.toStrict . Aeson.encode

--TODO: add prettyPrintJSON :: ToJSON a => a -> ByteString

deserialiseFromJSON :: FromJSON a
                    => AsType a
                    -> ByteString
                    -> Either JsonDecodeError a
deserialiseFromJSON _proxy = either (Left . JsonDecodeError) Right
                           . Aeson.eitherDecodeStrict'


readFileJSON :: FromJSON a
             => AsType a
             -> FilePath
             -> IO (Either (FileError JsonDecodeError) a)
readFileJSON ttoken path =
    runExceptT $ do
      content <- handleIOExceptT (FileIOError path) $ BS.readFile path
      firstExceptT (FileError path) $ hoistEither $
        deserialiseFromJSON ttoken content

writeFileJSON :: ToJSON a
              => FilePath
              -> a
              -> IO (Either (FileError ()) ())
writeFileJSON path x =
    runExceptT $
      handleIOExceptT (FileIOError path) $
        BS.writeFile path (serialiseToJSON x)

