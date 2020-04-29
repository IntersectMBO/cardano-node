{-# LANGUAGE PatternSynonyms #-}

module Cardano.Api.Shelley.CBOR
  ( decodeShelleyVerificationKey
  , decodeShelleyGenesisVerificationKey
  , encodeShelleyVerificationKey
  , renderShelleyPublicKeyToCBOR
  , shelleyAddressFromCBOR
  , shelleyAddressToCBOR
  , shelleyGenesisVerificationKeyFromCBOR
  , shelleyGenesisVerificationKeyToCBOR
  , shelleyKeyPairFromCBOR
  , shelleyKeyPairToCBOR
  , shelleyPublicKeyFromCBOR
  , shelleyVerificationKeyFromCBOR
  , shelleyVerificationKeyToCBOR
  ) where

import           Cardano.Api.Error
import           Cardano.Api.Types

import           Cardano.Binary (DecoderError (..), Decoder, Encoding, FromCBOR (..), ToCBOR (..))
import qualified Cardano.Binary as CBOR

import           Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm (..))
import           Cardano.Prelude

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Word (Word8)

import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosStandardCrypto)
import           Shelley.Spec.Ledger.Keys (DiscVKey (..), SKey (..), pattern VKey, VKey,
                     pattern VKeyGenesis, VKeyGenesis)
import           Shelley.Spec.Ledger.TxData (Addr)

shelleyAddressFromCBOR :: ByteString -> Either ApiError (Addr TPraosStandardCrypto)
shelleyAddressFromCBOR bs =
   first ApiErrorCBOR . CBOR.decodeFullDecoder "ShelleyAddress" decode $ LBS.fromStrict bs
  where
    decode :: Decoder s (Addr TPraosStandardCrypto)
    decode = do
      tag <- CBOR.decodeWord8
      case tag of
        171  -> fromCBOR
        _  -> cborError $ DecoderErrorUnknownTag "ShelleyAddress" tag

shelleyAddressToCBOR :: Addr TPraosStandardCrypto -> ByteString
shelleyAddressToCBOR addr =
  CBOR.serializeEncoding' $ mconcat [ toCBOR (171 :: Word8), toCBOR addr]

shelleyKeyPairFromCBOR :: ByteString -> Either ApiError ShelleyKeyPair
shelleyKeyPairFromCBOR bs =
   first ApiErrorCBOR . CBOR.decodeFullDecoder "ShelleyKeyPair" decode $ LBS.fromStrict bs
  where
    decode :: Decoder s ShelleyKeyPair
    decode = do
      tag <- CBOR.decodeWord8
      case tag of
        173  -> KeyPairShelley <$> decodeShelleyVerificationKey <*> (SKey <$> decodeSignKeyDSIGN)
        174  -> GenesisKeyPairShelley <$> decodeShelleyGenesisVerificationKey <*> (SKey <$> decodeSignKeyDSIGN)
        _  -> cborError $ DecoderErrorUnknownTag "ShelleyKeyPair" tag

shelleyKeyPairToCBOR :: ShelleyKeyPair -> ByteString
shelleyKeyPairToCBOR kp =
  case kp of
    KeyPairShelley vk (SKey sk) ->
      CBOR.serializeEncoding' $
        mconcat
          [ toCBOR (173 :: Word8)
          , encodeShelleyVerificationKey vk
          , encodeSignKeyDSIGN sk
          ]
    GenesisKeyPairShelley vk (SKey sk) ->
      CBOR.serializeEncoding' $
        mconcat
          [ toCBOR (174 :: Word8)
          , encodeShelleyGenesisVerificationKey vk
          , encodeSignKeyDSIGN sk
          ]

shelleyPublicKeyFromCBOR :: ByteString -> Either ApiError ShelleyPublicKey
shelleyPublicKeyFromCBOR bs =
   first ApiErrorCBOR . CBOR.decodeFullDecoder "ShelleyPublicKey" decode $ LBS.fromStrict bs
  where
    decode :: Decoder s ShelleyPublicKey
    decode = do
      tag <- CBOR.decodeWord8
      case tag of
        175 -> BootStrapPubKeyShelley <$> decodeShelleyVerificationKey
        176 -> GenesisPubKeyShelley <$> decodeShelleyGenesisVerificationKey
        _  -> cborError $ DecoderErrorUnknownTag "ShelleyPublicKey" tag

renderShelleyPublicKeyToCBOR :: ShelleyPublicKey -> ByteString
renderShelleyPublicKeyToCBOR (BootStrapPubKeyShelley vk) =
  CBOR.serializeEncoding' $ mconcat [ toCBOR (175 :: Word8), encodeShelleyVerificationKey vk ]
renderShelleyPublicKeyToCBOR (GenesisPubKeyShelley vk) =
  CBOR.serializeEncoding' $ mconcat [ toCBOR (176 :: Word8), encodeShelleyGenesisVerificationKey vk ]

shelleyVerificationKeyFromCBOR :: ByteString -> Either ApiError (VKey TPraosStandardCrypto)
shelleyVerificationKeyFromCBOR bs =
   first ApiErrorCBOR
    . CBOR.decodeFullDecoder "ShelleyVerificationKey" decodeShelleyVerificationKey
    $ LBS.fromStrict bs

shelleyGenesisVerificationKeyFromCBOR :: ByteString -> Either ApiError (VKeyGenesis TPraosStandardCrypto)
shelleyGenesisVerificationKeyFromCBOR bs =
   first ApiErrorCBOR
    . CBOR.decodeFullDecoder "ShelleyGenesisVerificationKey" decodeShelleyGenesisVerificationKey
    $ LBS.fromStrict bs

decodeShelleyVerificationKey :: Decoder s (VKey TPraosStandardCrypto)
decodeShelleyVerificationKey = do
  tag <- CBOR.decodeWord8
  case tag of
    181 -> VKey <$> decodeVerKeyDSIGN
    _  -> cborError $ DecoderErrorUnknownTag "ShelleyVerificationKey" tag

decodeShelleyGenesisVerificationKey :: Decoder s (VKeyGenesis TPraosStandardCrypto)
decodeShelleyGenesisVerificationKey = do
  tag <- CBOR.decodeWord8
  case tag of
    180 -> VKeyGenesis <$> decodeVerKeyDSIGN
    _  -> cborError $ DecoderErrorUnknownTag "ShelleyGenesisVerificationKey" tag

shelleyGenesisVerificationKeyToCBOR :: VKeyGenesis TPraosStandardCrypto -> ByteString
shelleyGenesisVerificationKeyToCBOR = CBOR.serializeEncoding' . encodeShelleyGenesisVerificationKey

shelleyVerificationKeyToCBOR :: VKey TPraosStandardCrypto -> ByteString
shelleyVerificationKeyToCBOR = CBOR.serializeEncoding' . encodeShelleyVerificationKey

encodeShelleyVerificationKey :: VKey TPraosStandardCrypto -> Encoding
encodeShelleyVerificationKey (DiscVKey vk) =
      mconcat [toCBOR (181 :: Word8), encodeVerKeyDSIGN vk]

encodeShelleyGenesisVerificationKey :: VKeyGenesis TPraosStandardCrypto -> Encoding
encodeShelleyGenesisVerificationKey (DiscVKey vk) =
      mconcat [toCBOR (180 :: Word8), encodeVerKeyDSIGN vk]