{-# LANGUAGE ScopedTypeVariables #-}

-- | Raw binary serialisation
--
module Cardano.Api.SerialiseRaw
  ( SerialiseAsRawBytes(..)
  , serialiseToRawBytesHex
  , deserialiseFromRawBytesHex
  , UsingRawBytesHex(..)
  ) where

import           Prelude
import           Cardano.Prelude (decodeEitherBase16)

import           Data.String (IsString(..))
import qualified Data.ByteString.Char8 as BSC
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16

import           Cardano.Api.HasTypeProxy


class HasTypeProxy a => SerialiseAsRawBytes a where

  serialiseToRawBytes :: a -> ByteString

  deserialiseFromRawBytes :: AsType a -> ByteString -> Maybe a

serialiseToRawBytesHex :: SerialiseAsRawBytes a => a -> ByteString
serialiseToRawBytesHex = Base16.encode . serialiseToRawBytes

deserialiseFromRawBytesHex :: SerialiseAsRawBytes a
                           => AsType a -> ByteString -> Maybe a
deserialiseFromRawBytesHex proxy hex = case decodeEitherBase16 hex of
  Right raw -> deserialiseFromRawBytes proxy raw
  Left _ -> Nothing


-- | For use with @deriving via@, to provide 'Show' and\/or 'IsString' instances
-- using a hex encoding, based on the 'SerialiseAsRawBytes' instance.
--
-- > deriving (Show, IsString) via (UsingRawBytesHex Blah)
--
newtype UsingRawBytesHex a = UsingRawBytesHex a

instance SerialiseAsRawBytes a => Show (UsingRawBytesHex a) where
    show (UsingRawBytesHex x) = show (serialiseToRawBytesHex x)

instance SerialiseAsRawBytes a => IsString (UsingRawBytesHex a) where
    fromString str =
      case decodeEitherBase16 (BSC.pack str) of
        Right raw -> case deserialiseFromRawBytes ttoken raw of
          Just x  -> UsingRawBytesHex x
          Nothing -> error ("fromString: cannot deserialise " ++ show str)
        Left msg -> error ("fromString: invalid hex " ++ show str ++ ", " ++ msg)
      where
        ttoken :: AsType a
        ttoken = proxyToAsType Proxy

