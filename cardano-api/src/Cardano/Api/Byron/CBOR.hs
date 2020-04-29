module Cardano.Api.Byron.CBOR
  ( byronAddressFromCBOR
  , byronAddressToCBOR
  , byronKeyPairFromCBOR
  , byronKeyPairToCBOR
  , byronPublicKeyFromCBOR
  , renderByronPublicKeyToCBOR
  ) where

import           Cardano.Api.Error
import           Cardano.Api.Types

import           Cardano.Binary (DecoderError (..), Decoder, Encoding, FromCBOR (..), ToCBOR (..))
import qualified Cardano.Binary as CBOR

import           Cardano.Prelude

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Word (Word8)

byronAddressFromCBOR :: ByteString -> Either ApiError ByronAddress
byronAddressFromCBOR bs =
   first ApiErrorCBOR . CBOR.decodeFullDecoder "ByronAddress" decode $ LBS.fromStrict bs
  where
    decode :: Decoder s ByronAddress
    decode = do
      tag <- CBOR.decodeWord8
      case tag of
        170  -> ByronAddress <$> fromCBOR
        _  -> cborError $ DecoderErrorUnknownTag "ByronAddress" tag
byronAddressToCBOR :: ByronAddress -> ByteString
byronAddressToCBOR (ByronAddress ba) =
  CBOR.serializeEncoding' $ mconcat [ toCBOR (170 :: Word8), toCBOR ba ]
byronKeyPairFromCBOR :: ByteString -> Either ApiError ByronKeyPair
byronKeyPairFromCBOR bs =
   first ApiErrorCBOR . CBOR.decodeFullDecoder "ByronKeyPair" decode $ LBS.fromStrict bs
  where
    decode :: Decoder s ByronKeyPair
    decode = do
      tag <- CBOR.decodeWord8
      case tag of
        172  -> KeyPairByron <$> fromCBOR <*> fromCBOR
        _  -> cborError $ DecoderErrorUnknownTag "ByronKeyPair" tag

byronKeyPairToCBOR :: ByronKeyPair -> ByteString
byronKeyPairToCBOR (KeyPairByron vk sk) =
  CBOR.serializeEncoding' $ mconcat [ toCBOR (172 :: Word8), toCBOR vk, toCBOR sk ]

byronPublicKeyFromCBOR :: ByteString -> Either ApiError ByronPublicKey
byronPublicKeyFromCBOR bs =
   first ApiErrorCBOR . CBOR.decodeFullDecoder "ByronPublicKey" decode $ LBS.fromStrict bs
  where
    decode :: Decoder s ByronPublicKey
    decode = do
      tag <- CBOR.decodeWord8
      case tag of
        174  -> PubKeyByron' <$> networkFromCBOR <*> fromCBOR
        _  -> cborError $ DecoderErrorUnknownTag "ByronKeyPair" tag

renderByronPublicKeyToCBOR :: ByronPublicKey -> ByteString
renderByronPublicKeyToCBOR (PubKeyByron' nw vk) =
  CBOR.serializeEncoding' $ mconcat [ toCBOR (174 :: Word8), networkToCBOR nw, toCBOR vk ]

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