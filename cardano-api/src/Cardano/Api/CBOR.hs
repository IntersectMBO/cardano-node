{-# LANGUAGE PatternSynonyms #-}

module Cardano.Api.CBOR
  ( addressFromCBOR
  , addressToCBOR
  , keyPairFromCBOR
  , keyPairToCBOR
  , publicKeyFromCBOR
  , publicKeyToCBOR

  , shelleyVerificationKeyFromCBOR
  , shelleyVerificationKeyToCBOR

  , txSignedFromCBOR
  , txSignedToCBOR
  , txUnsignedFromCBOR
  , txUnsignedToCBOR
  ) where

import           Cardano.Api.Error
import           Cardano.Api.Types

import           Cardano.Binary (DecoderError (..), Decoder, Encoding, FromCBOR (..), ToCBOR (..))
import qualified Cardano.Binary as CBOR

import           Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm (..))

import           Cardano.Prelude

import           Data.ByteString.Char8 (ByteString)
-- import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Word (Word8)

import           Shelley.Spec.Ledger.Keys (DiscVKey (..), SKey (..), pattern VKey,
                     pattern VKeyGenesis)


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
        173  -> KeyPairShelley <$> decodeShelleyVerificationKey <*> (SKey <$> decodeSignKeyDSIGN)
        _  -> cborError $ DecoderErrorUnknownTag "KeyPair" tag

keyPairToCBOR :: KeyPair -> ByteString
keyPairToCBOR kp =
  CBOR.serializeEncoding' $
    case kp of
      KeyPairByron vk sk -> mconcat [ toCBOR (172 :: Word8), toCBOR vk, toCBOR sk ]
      KeyPairShelley svk (SKey sk) ->
        mconcat
          [ toCBOR (173 :: Word8)
          , encodeShelleyVerificationKey svk
          , encodeSignKeyDSIGN sk
          ]

publicKeyFromCBOR :: ByteString -> Either ApiError PublicKey
publicKeyFromCBOR bs =
   first ApiErrorCBOR . CBOR.decodeFullDecoder "PublicKey" decode $ LBS.fromStrict bs
  where
    decode :: Decoder s PublicKey
    decode = do
      tag <- CBOR.decodeWord8
      case tag of
        174  -> PubKeyByron <$> networkFromCBOR <*> fromCBOR
        175  -> PubKeyShelley <$> networkFromCBOR <*> decodeShelleyVerificationKey
        _  -> cborError $ DecoderErrorUnknownTag "KeyPair" tag

publicKeyToCBOR :: PublicKey -> ByteString
publicKeyToCBOR pk =
  CBOR.serializeEncoding' $
    case pk of
      PubKeyByron nw vk -> mconcat [ toCBOR (174 :: Word8), networkToCBOR nw, toCBOR vk ]
      PubKeyShelley nw vk ->
        mconcat
          [ toCBOR (175 :: Word8)
          , networkToCBOR nw
          , encodeShelleyVerificationKey vk
          ]

txSignedFromCBOR :: ByteString -> Either ApiError TxSigned
txSignedFromCBOR bs =
   first ApiErrorCBOR . CBOR.decodeFullDecoder "TxSigned" decode $ LBS.fromStrict bs
  where
    decode :: Decoder s TxSigned
    decode = do
      tag <- CBOR.decodeWord8
      case tag of
        176  -> TxSignedByron <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
        177  -> pure TxSignedShelley
        _  -> cborError $ DecoderErrorUnknownTag "TxSigned" tag

txSignedToCBOR :: TxSigned -> ByteString
txSignedToCBOR pk =
  CBOR.serializeEncoding' $
    case pk of
      TxSignedByron btx cbor hash wit ->
        mconcat [ toCBOR (176 :: Word8), toCBOR btx, toCBOR cbor, toCBOR hash, toCBOR wit ]
      TxSignedShelley -> toCBOR (177 :: Word8)

txUnsignedFromCBOR :: ByteString -> Either ApiError TxUnsigned
txUnsignedFromCBOR bs =
   first ApiErrorCBOR . CBOR.decodeFullDecoder "TxUnsigned" decode $ LBS.fromStrict bs
  where
    decode :: Decoder s TxUnsigned
    decode = do
      tag <- CBOR.decodeWord8
      case tag of
        178  -> TxUnsignedByron <$> fromCBOR <*> fromCBOR <*> fromCBOR
        179  -> pure TxUnsignedShelley
        _  -> cborError $ DecoderErrorUnknownTag "TxUnsigned" tag

txUnsignedToCBOR :: TxUnsigned -> ByteString
txUnsignedToCBOR pk =
  CBOR.serializeEncoding' $
    case pk of
      TxUnsignedByron btx cbor hash ->
        mconcat [ toCBOR (178 :: Word8), toCBOR btx, toCBOR cbor, toCBOR hash ]
      TxUnsignedShelley -> toCBOR (179 :: Word8)

shelleyVerificationKeyFromCBOR :: ByteString -> Either ApiError ShelleyVerificationKey
shelleyVerificationKeyFromCBOR bs =
   first ApiErrorCBOR
    . CBOR.decodeFullDecoder "ShelleyVerificationKey" decodeShelleyVerificationKey
    $ LBS.fromStrict bs

decodeShelleyVerificationKey :: Decoder s ShelleyVerificationKey
decodeShelleyVerificationKey = do
  tag <- CBOR.decodeWord8
  case tag of
    180 -> GenesisShelleyVerificationKey <$> (VKeyGenesis <$> decodeVerKeyDSIGN)
    181 -> RegularShelleyVerificationKey <$> (VKey <$> decodeVerKeyDSIGN)
    _  -> cborError $ DecoderErrorUnknownTag "ShelleyVerificationKey" tag

shelleyVerificationKeyToCBOR :: ShelleyVerificationKey -> ByteString
shelleyVerificationKeyToCBOR = CBOR.serializeEncoding' . encodeShelleyVerificationKey

encodeShelleyVerificationKey :: ShelleyVerificationKey -> Encoding
encodeShelleyVerificationKey svk =
  case svk of
    GenesisShelleyVerificationKey (DiscVKey vk) ->
      mconcat [toCBOR (180 :: Word8), encodeVerKeyDSIGN vk]
    RegularShelleyVerificationKey (DiscVKey vk) ->
      mconcat [toCBOR (181 :: Word8), encodeVerKeyDSIGN vk]

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
