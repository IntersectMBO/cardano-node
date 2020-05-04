{-# LANGUAGE PatternSynonyms #-}

module Cardano.Api.CBOR
  ( addressFromCBOR
  , addressToCBOR
  , signingKeyFromCBOR
  , signingKeyToCBOR
  , verificationKeyFromCBOR
  , verificationKeyToCBOR

  , shelleyVerificationKeyFromCBOR
  , shelleyVerificationKeyToCBOR

  , txSignedFromCBOR
  , txSignedToCBOR
  , txUnsignedFromCBOR
  , txUnsignedToCBOR

  -- Export these to avoid "defined but not used" error.
  -- Probably needed later.
  , networkFromCBOR
  , networkToCBOR
  ) where

import           Cardano.Api.Error
import           Cardano.Api.Types

import           Cardano.Binary (DecoderError (..), Decoder, Encoding, FromCBOR (..), ToCBOR (..))
import qualified Cardano.Binary as CBOR

import           Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm (..))
import           Cardano.Prelude

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Data.Word (Word8)

import           Shelley.Spec.Ledger.Keys
                   (DiscVKey (..), SKey (..), pattern VKey)


addressFromCBOR :: ByteString -> Either ApiError Address
addressFromCBOR bs =
   first ApiErrorCBOR . CBOR.decodeFullDecoder "Address" decode $ LBS.fromStrict bs
  where
    decode :: Decoder s Address
    decode = do
      tag <- CBOR.decodeWord8
      case tag of
        170  -> AddressByron <$> fromCBOR
        171  -> AddressShelley <$> fromCBOR
        _  -> cborError $ DecoderErrorUnknownTag "Address" tag

addressToCBOR :: Address -> ByteString
addressToCBOR kp =
  CBOR.serializeEncoding' $
    case kp of
      AddressByron   addr -> mconcat [ toCBOR (170 :: Word8), toCBOR addr ]
      AddressShelley addr -> mconcat [ toCBOR (171 :: Word8), toCBOR addr ]

signingKeyFromCBOR :: ByteString -> Either ApiError SigningKey
signingKeyFromCBOR bs =
   first ApiErrorCBOR . CBOR.decodeFullDecoder "SigningKey" decode $ LBS.fromStrict bs
  where
    decode :: Decoder s SigningKey
    decode = do
      tag <- CBOR.decodeWord8
      case tag of
        172  -> SigningKeyByron <$> fromCBOR
        173  -> SigningKeyShelley . SKey <$> decodeSignKeyDSIGN
        _  -> cborError $ DecoderErrorUnknownTag "SigningKey" tag

signingKeyToCBOR :: SigningKey -> ByteString
signingKeyToCBOR kp =
  CBOR.serializeEncoding' $
    case kp of
      SigningKeyByron sk ->
        mconcat
          [ toCBOR (172 :: Word8)
          , toCBOR sk ]

      SigningKeyShelley (SKey sk) ->
        mconcat
          [ toCBOR (173 :: Word8)
          , encodeSignKeyDSIGN sk
          ]

verificationKeyFromCBOR :: ByteString -> Either ApiError VerificationKey
verificationKeyFromCBOR =
   first ApiErrorCBOR
 . CBOR.decodeFullDecoder "VerificationKey" decode
 . LBS.fromStrict
  where
    decode :: Decoder s VerificationKey
    decode = do
      tag <- CBOR.decodeWord8
      case tag of
        174  -> VerificationKeyByron <$> fromCBOR
        175  -> VerificationKeyShelley <$> decodeShelleyVerificationKey
        _  -> cborError $ DecoderErrorUnknownTag "VerificationKey" tag

verificationKeyToCBOR :: VerificationKey -> ByteString
verificationKeyToCBOR pk =
  CBOR.serializeEncoding' $
    case pk of
      VerificationKeyByron vk -> mconcat [ toCBOR (174 :: Word8), toCBOR vk ]
      VerificationKeyShelley vk ->
        mconcat
          [ toCBOR (175 :: Word8)
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
        177  -> TxSignedShelley <$> decodeShelleyTx bs
        _  -> cborError $ DecoderErrorUnknownTag "TxSigned" tag

txSignedToCBOR :: TxSigned -> ByteString
txSignedToCBOR pk =
  CBOR.serializeEncoding' $
    case pk of
      TxSignedByron btx cbor hash wit ->
        mconcat [ toCBOR (176 :: Word8), toCBOR btx, toCBOR cbor, toCBOR hash, toCBOR wit ]
      TxSignedShelley tx ->
        mconcat [ toCBOR (177 :: Word8), toCBOR tx ]

txUnsignedFromCBOR :: ByteString -> Either ApiError TxUnsigned
txUnsignedFromCBOR bs =
   first ApiErrorCBOR . CBOR.decodeFullDecoder "TxUnsigned" decode $ LBS.fromStrict bs
  where
    decode :: Decoder s TxUnsigned
    decode = do
      tag <- CBOR.decodeWord8
      case tag of
        178  -> TxUnsignedByron <$> fromCBOR <*> fromCBOR <*> fromCBOR
        179  -> TxUnsignedShelley <$> decodeShelleyTxBody bs
        _  -> cborError $ DecoderErrorUnknownTag "TxUnsigned" tag


txUnsignedToCBOR :: TxUnsigned -> ByteString
txUnsignedToCBOR pk =
  CBOR.serializeEncoding' $
    case pk of
      TxUnsignedByron btx cbor hash ->
        mconcat [ toCBOR (178 :: Word8), toCBOR btx, toCBOR cbor, toCBOR hash ]
      TxUnsignedShelley tx ->
        mconcat [ toCBOR (179 :: Word8), encodeShelleyTxBody tx ]

shelleyVerificationKeyFromCBOR :: ByteString -> Either ApiError ShelleyVerificationKey
shelleyVerificationKeyFromCBOR bs =
   first ApiErrorCBOR
    . CBOR.decodeFullDecoder "ShelleyVerificationKey" decodeShelleyVerificationKey
    $ LBS.fromStrict bs

decodeShelleyVerificationKey :: Decoder s ShelleyVerificationKey
decodeShelleyVerificationKey = VKey <$> decodeVerKeyDSIGN

shelleyVerificationKeyToCBOR :: ShelleyVerificationKey -> ByteString
shelleyVerificationKeyToCBOR = CBOR.serializeEncoding' . encodeShelleyVerificationKey

encodeShelleyVerificationKey :: ShelleyVerificationKey -> Encoding
encodeShelleyVerificationKey (DiscVKey vk) = encodeVerKeyDSIGN vk

encodeShelleyTxBody :: ShelleyTxBody -> Encoding
encodeShelleyTxBody = toCBOR

decodeShelleyTxBody :: ByteString -> Decoder s ShelleyTxBody
decodeShelleyTxBody full = do
    atx <- fromCBOR
    return $! CBOR.runAnnotator atx (CBOR.Full (LBS.fromStrict full))

decodeShelleyTx :: ByteString -> Decoder s ShelleyTx
decodeShelleyTx full = do
    atx <- fromCBOR
    return $! CBOR.runAnnotator atx (CBOR.Full (LBS.fromStrict full))

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
