module Cardano.Api.CBOR
  ( keyPairFromCBOR
  , keyPairToCBOR
  ) where

import           Cardano.Api.Error
import           Cardano.Api.Types

import           Cardano.Binary (DecoderError (..), Decoder, FromCBOR (..), ToCBOR (..))
import qualified Cardano.Binary as CBOR

import           Cardano.Prelude

import           Data.ByteString.Char8 (ByteString)
-- import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Word (Word8)


keyPairToCBOR :: KeyPair -> ByteString
keyPairToCBOR kp =
  CBOR.serializeEncoding' $
    case kp of
      KeyPairByron vk sk -> mconcat [ toCBOR (0 :: Word8), toCBOR vk, toCBOR sk ]
      KeyPairShelley -> mconcat [ toCBOR (1 :: Word8) ]

keyPairFromCBOR :: ByteString -> Either ApiError KeyPair
keyPairFromCBOR bs =
   first ApiErrorCBOR . CBOR.decodeFullDecoder "KeyPair" decode $ LBS.fromStrict bs
  where
    decode :: Decoder s KeyPair
    decode = do
      tag <- CBOR.decodeWord8
      case tag of
        0  -> KeyPairByron <$> fromCBOR <*> fromCBOR
        1  -> pure KeyPairShelley
        _  -> cborError $ DecoderErrorUnknownTag "KeyPair" tag
