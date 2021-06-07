{-# LANGUAGE ScopedTypeVariables #-}

-- | Raw binary serialisation
--
module Cardano.Api.SerialiseUsing
  ( UsingRawBytesHex(..)
  ) where

import           Prelude

import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BSC
import           Data.String (IsString (..))
import           Data.Typeable

import           Cardano.Api.HasTypeProxy
import           Cardano.Api.SerialiseRaw



-- | For use with @deriving via@, to provide instances for any\/all of 'Show',
-- 'IsString', 'ToJSON', 'FromJSON', 'ToJSONKey', FromJSONKey' using a hex
-- encoding, based on the 'SerialiseAsRawBytes' instance.
--
-- > deriving (Show, IsString) via (UsingRawBytesHex Blah)
-- > deriving (ToJSON, FromJSON) via (UsingRawBytesHex Blah)
-- > deriving (ToJSONKey, FromJSONKey) via (UsingRawBytesHex Blah)
--
newtype UsingRawBytesHex a = UsingRawBytesHex a

instance SerialiseAsRawBytes a => Show (UsingRawBytesHex a) where
    show (UsingRawBytesHex x) = show (serialiseToRawBytesHex x)

instance SerialiseAsRawBytes a => IsString (UsingRawBytesHex a) where
    fromString str =
      case Base16.decode (BSC.pack str) of
        Right raw -> case deserialiseFromRawBytes ttoken raw of
          Just x  -> UsingRawBytesHex x
          Nothing -> error ("fromString: cannot deserialise " ++ show str)
        Left msg -> error ("fromString: invalid hex " ++ show str ++ ", " ++ msg)
      where
        ttoken :: AsType a
        ttoken = proxyToAsType Proxy
