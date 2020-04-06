module Cardano.Api.CBOR
  ( addressFromCBOR
  , addressToCBOR
  , keyPairFromCBOR
  , keyPairToCBOR
  , publicKeyFromCBOR
  , publicKeyToCBOR
  ) where

import           Cardano.Api.Error
import           Cardano.Api.Types

import           Cardano.Binary (DecoderError (..), Decoder, Encoding, FromCBOR (..), ToCBOR (..))
import qualified Cardano.Binary as CBOR

import           Cardano.Prelude

import           Data.ByteString.Char8 (ByteString)
-- import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Word (Word8)


addressFromCBOR :: ByteString -> Either ApiError Address
addressFromCBOR bs =
   first ApiErrorCBOR . CBOR.decodeFullDecoder "Address" decode $ LBS.fromStrict bs
  where
    decode :: Decoder s Address
    decode = do
      tag <- CBOR.decodeWord8
      case tag of
        170  -> AddressByron <$> fromCBOR
        171  -> pure AddressShelley
        _  -> cborError $ DecoderErrorUnknownTag "Address" tag

addressToCBOR :: Address -> ByteString
addressToCBOR kp =
  CBOR.serializeEncoding' $
    case kp of
      AddressByron ba -> mconcat [ toCBOR (170 :: Word8), toCBOR ba ]
      AddressShelley -> toCBOR (171 :: Word8)

keyPairFromCBOR :: ByteString -> Either ApiError KeyPair
keyPairFromCBOR bs =
   first ApiErrorCBOR . CBOR.decodeFullDecoder "KeyPair" decode $ LBS.fromStrict bs
  where
    decode :: Decoder s KeyPair
    decode = do
      tag <- CBOR.decodeWord8
      case tag of
        172  -> KeyPairByron <$> fromCBOR <*> fromCBOR
        173  -> pure KeyPairShelley
        _  -> cborError $ DecoderErrorUnknownTag "KeyPair" tag

keyPairToCBOR :: KeyPair -> ByteString
keyPairToCBOR kp =
  CBOR.serializeEncoding' $
    case kp of
      KeyPairByron vk sk -> mconcat [ toCBOR (172 :: Word8), toCBOR vk, toCBOR sk ]
      KeyPairShelley -> toCBOR (173 :: Word8)

publicKeyFromCBOR :: ByteString -> Either ApiError PublicKey
publicKeyFromCBOR bs =
   first ApiErrorCBOR . CBOR.decodeFullDecoder "PublicKey" decode $ LBS.fromStrict bs
  where
    decode :: Decoder s PublicKey
    decode = do
      tag <- CBOR.decodeWord8
      case tag of
        174  -> PubKeyByron <$> networkFromCBOR <*> fromCBOR
        175  -> pure PubKeyShelley
        _  -> cborError $ DecoderErrorUnknownTag "KeyPair" tag

publicKeyToCBOR :: PublicKey -> ByteString
publicKeyToCBOR pk =
  CBOR.serializeEncoding' $
    case pk of
      PubKeyByron nw vk -> mconcat [ toCBOR (174 :: Word8), networkToCBOR nw, toCBOR vk ]
      PubKeyShelley -> toCBOR (175 :: Word8)

-- -------------------------------------------------------------------------------------------------

networkFromCBOR :: Decoder s Network
networkFromCBOR = do
  tag <- CBOR.decodeWord8
  case tag of
    168  -> pure Mainnet
    169  -> Testnet <$> fromCBOR
    _  -> cborError $ DecoderErrorUnknownTag "Network" tag

networkToCBOR :: Network -> Encoding
networkToCBOR nw =
  case nw of
    Mainnet -> mconcat [toCBOR (168 :: Word8)]
    Testnet pid -> mconcat [toCBOR (169 :: Word8), toCBOR pid]

