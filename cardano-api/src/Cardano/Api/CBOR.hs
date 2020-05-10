{-# LANGUAGE PatternSynonyms #-}

module Cardano.Api.CBOR
  ( addressFromCBOR
  , addressToCBOR
  , certificateFromCBOR
  , certificateToCBOR
  , signingKeyFromCBOR
  , signingKeyToCBOR
  , verificationKeyFromCBOR
  , verificationKeyToCBOR
  , verificationKeyStakePoolFromCBOR
  , verificationKeyStakePoolToCBOR
  , verificationKeyStakingFromCBOR
  , verificationKeyStakingToCBOR

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

import           Shelley.Spec.Ledger.Keys (VKey (..))


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

certificateFromCBOR :: ByteString -> Either ApiError Certificate
certificateFromCBOR bs =
    first ApiErrorCBOR . CBOR.decodeFullDecoder "ShelleyCertificate" decode $ LBS.fromStrict bs
  where
    decode :: Decoder s Certificate
    decode = do
      tag <- CBOR.decodeWord8
      case tag of
        180 -> ShelleyDelegationCertificate <$> fromCBOR
        181 -> ShelleyStakePoolCertificate <$> fromCBOR
        182 -> ShelleyGenesisDelegationCertificate <$> fromCBOR
        183 -> ShelleyMIRCertificate <$> fromCBOR
        _ -> cborError $ DecoderErrorUnknownTag "ShelleyCertificate" tag

certificateToCBOR :: Certificate -> ByteString
certificateToCBOR sc =
  CBOR.serializeEncoding' $
    case sc of
      ShelleyDelegationCertificate cert -> mconcat [ toCBOR (180 :: Word8) , toCBOR cert]
      ShelleyStakePoolCertificate cert -> mconcat [ toCBOR (181 :: Word8) , toCBOR cert]
      ShelleyGenesisDelegationCertificate cert -> mconcat [ toCBOR (182 :: Word8), toCBOR cert ]
      ShelleyMIRCertificate cert -> mconcat [ toCBOR (183 :: Word8), toCBOR cert ]

signingKeyFromCBOR :: ByteString -> Either ApiError SigningKey
signingKeyFromCBOR bs =
   first ApiErrorCBOR . CBOR.decodeFullDecoder "SigningKey" decode $ LBS.fromStrict bs
  where
    decode :: Decoder s SigningKey
    decode = do
      tag <- CBOR.decodeWord8
      case tag of
        172  -> SigningKeyByron <$> fromCBOR
        173  -> SigningKeyShelley <$> decodeSignKeyDSIGN
        _  -> cborError $ DecoderErrorUnknownTag "SigningKey" tag

signingKeyToCBOR :: SigningKey -> ByteString
signingKeyToCBOR kp =
  CBOR.serializeEncoding' $
    case kp of
      SigningKeyByron sk ->
        mconcat
          [ toCBOR (172 :: Word8)
          , toCBOR sk ]

      SigningKeyShelley sk ->
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

verificationKeyStakePoolFromCBOR :: ByteString -> Either ApiError ShelleyVerificationKeyStakePool
verificationKeyStakePoolFromCBOR =
   first ApiErrorCBOR
 . CBOR.decodeFullDecoder "StakePoolVerificationKey" decode
 . LBS.fromStrict
  where
    decode :: Decoder s ShelleyVerificationKeyStakePool
    decode = do
      tag <- CBOR.decodeWord8
      case tag of
        184 -> fromCBOR
        _  -> cborError $ DecoderErrorUnknownTag "StakePoolVerificationKey" tag

verificationKeyStakePoolToCBOR :: ShelleyVerificationKeyStakePool -> ByteString
verificationKeyStakePoolToCBOR vk =
  CBOR.serializeEncoding' $ mconcat [ toCBOR (184 :: Word8), toCBOR vk ]


verificationKeyStakingFromCBOR :: ByteString -> Either ApiError ShelleyVerificationKeyStaking
verificationKeyStakingFromCBOR =
   first ApiErrorCBOR
 . CBOR.decodeFullDecoder "StakingVerificationKey" decode
 . LBS.fromStrict
  where
    decode :: Decoder s ShelleyVerificationKeyStaking
    decode = do
      tag <- CBOR.decodeWord8
      case tag of
        185 -> fromCBOR
        _  -> cborError $ DecoderErrorUnknownTag "StakingVerificationKey" tag

verificationKeyStakingToCBOR :: ShelleyVerificationKeyStaking -> ByteString
verificationKeyStakingToCBOR vk =
  CBOR.serializeEncoding' $ mconcat [ toCBOR (185 :: Word8), toCBOR vk ]

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
encodeShelleyVerificationKey (VKey vk) = encodeVerKeyDSIGN vk

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
    169  -> Testnet . NetworkMagic <$> fromCBOR
    _  -> cborError $ DecoderErrorUnknownTag "Network" tag

networkToCBOR :: Network -> Encoding
networkToCBOR nw =
  case nw of
    Mainnet -> mconcat [toCBOR (168 :: Word8)]
    Testnet (NetworkMagic nm) -> mconcat [toCBOR (169 :: Word8), toCBOR nm]
