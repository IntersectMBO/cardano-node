module Cardano.Api.View
  ( parseAddressView
  , parseCertificateView
  , parseSigningKeyView
  , parseGenesisVerificationKeyView
  , parsePaymentVerificationKeyView
  , parseStakingVerificationKeyView
  , parseUpdateView
  , parseVerificationKeyVRFView
  , parseTxSignedView
  , parseTxUnsignedView

  , readAddress
  , readCertificate
  , readSigningKey
  , readGenesisVerificationKey
  , readPaymentVerificationKey
  , readStakingVerificationKey
  , readVRFVerificationKey
  , readTxSigned
  , readTxUnsigned
  , readUpdate

  , renderAddressView
  , renderCertificateView
  , renderGenesisVerificationKeyView
  , renderSigningKeyView
  , renderPaymentVerificationKeyView
  , renderStakingVerificationKeyView
  , renderUpdateView
  , renderVerificationKeyVRFView
  , renderTxSignedView
  , renderTxUnsignedView

  , writeAddress
  , writeCertificate
  , writeSigningKey
  , writePaymentVerificationKey
  , writeStakingVerificationKey
  , writeVRFVerificationKey
  , writeTxSigned
  , writeTxUnsigned
  , writeUpdate
  ) where

import           Cardano.Api.CBOR
import           Cardano.Api.Types
import           Cardano.Api.Error

import           Cardano.Api.TextView

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra (handleIOExceptT, hoistEither, runExceptT)

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

--TODO: There are VRF reading and writing functions that also exist in Cardano.Api.Shelley.VRF
-- This needs to be sorted out


parseAddressView :: ByteString -> Either ApiError Address
parseAddressView bs =
  addressFromCBOR . tvRawCBOR =<< first ApiTextView (parseTextView bs)

parseCertificateView :: ByteString -> Either ApiError Certificate
parseCertificateView bs =
  certificateFromCBOR . tvRawCBOR =<< first ApiTextView (parseTextView bs)

parseSigningKeyView :: ByteString -> Either ApiError SigningKey
parseSigningKeyView bs =
  signingKeyFromCBOR . tvRawCBOR =<< first ApiTextView (parseTextView bs)

parseGenesisVerificationKeyView :: ByteString -> Either ApiError GenesisVerificationKey
parseGenesisVerificationKeyView bs =
  genesisVerificationKeyFromCBOR . tvRawCBOR =<< first ApiTextView (parseTextView bs)

parsePaymentVerificationKeyView :: ByteString -> Either ApiError PaymentVerificationKey
parsePaymentVerificationKeyView bs =
  paymentVerificationKeyFromCBOR . tvRawCBOR =<< first ApiTextView (parseTextView bs)

parseStakingVerificationKeyView :: ByteString -> Either ApiError StakingVerificationKey
parseStakingVerificationKeyView bs =
  stakingVerificationKeyFromCBOR . tvRawCBOR =<< first ApiTextView (parseTextView bs)

parseUpdateView :: ByteString -> Either ApiError Update
parseUpdateView bs =
  updateFromCBOR . tvRawCBOR =<< first ApiTextView (parseTextView bs)

parseVRFVerificationKeyView :: ByteString -> Either ApiError ShelleyVRFVerificationKey
parseVRFVerificationKeyView bs =
  verificationKeyVRFFromCBOR . tvRawCBOR =<< first ApiTextView (parseTextView bs)

parseVerificationKeyVRFView :: ByteString -> Either ApiError ShelleyVRFVerificationKey
parseVerificationKeyVRFView bs =
  verificationKeyVRFFromCBOR . tvRawCBOR =<< first ApiTextView (parseTextView bs)


parseTxSignedView :: ByteString -> Either ApiError TxSigned
parseTxSignedView bs =
  txSignedFromCBOR . tvRawCBOR =<< first ApiTextView (parseTextView bs)

parseTxUnsignedView :: ByteString -> Either ApiError TxUnsigned
parseTxUnsignedView bs =
  txUnsignedFromCBOR . tvRawCBOR =<< first ApiTextView (parseTextView bs)

renderAddressView :: Address -> ByteString
renderAddressView addr =
  case addr of
    AddressByron {} -> renderTextView $ TextView "AddressByron" "Free form text" cbor
    AddressShelley {} -> renderTextView $ TextView "AddressShelley" "Free form text" cbor
    AddressShelleyReward {} -> renderTextView $ TextView "AddressShelleyRewardAccount" "Free form text" cbor
  where
    cbor :: ByteString
    cbor = addressToCBOR addr

renderCertificateView :: Certificate -> ByteString
renderCertificateView cert =
  case cert of
    ShelleyDelegationCertificate {} -> renderTextView $ TextView "DelegationCertificateShelley" "Free form text" cbor
    ShelleyStakePoolCertificate {} -> renderTextView $ TextView "StakePoolCertificateShelley" "Free form text" cbor
    ShelleyGenesisDelegationCertificate {} -> renderTextView $ TextView "GenesisDelegationCertificateShelley" "Free form text" cbor
    ShelleyMIRCertificate {} -> renderTextView $ TextView "MIRCertificateShelley" "Free form text" cbor

  where
    cbor :: ByteString
    cbor = certificateToCBOR cert

renderGenesisVerificationKeyView :: GenesisVerificationKey -> ByteString
renderGenesisVerificationKeyView pk =
  renderTextView $ TextView "GenesisVerificationKeyShelley" "Free form text" cbor
  where
    cbor :: ByteString
    cbor = genesisVerificationKeyToCBOR pk

renderSigningKeyView :: SigningKey -> ByteString
renderSigningKeyView kp =
  case kp of
    SigningKeyByron {} -> renderTextView $ TextView "SigningKeyByron" "Free form text" cbor
    SigningKeyShelley {} -> renderTextView $ TextView "SigningKeyShelley" "Free form text" cbor
  where
    cbor :: ByteString
    cbor = signingKeyToCBOR kp

renderUpdateView :: Update -> ByteString
renderUpdateView up =
    renderTextView $ TextView "UpdateShelley" "Free form text" cbor
  where
    cbor :: ByteString
    cbor = updateToCBOR up

renderPaymentVerificationKeyView :: PaymentVerificationKey -> ByteString
renderPaymentVerificationKeyView pk =
  case pk of
    PaymentVerificationKeyByron {} ->
      renderTextView $ TextView "PaymentVerificationKeyByron" "Free form text" cbor
    PaymentVerificationKeyShelley {} ->
      renderTextView $ TextView "PaymentVerificationKeyShelley" "Free form text" cbor
  where
    cbor :: ByteString
    cbor = paymentVerificationKeyToCBOR pk

renderStakingVerificationKeyView :: StakingVerificationKey -> ByteString
renderStakingVerificationKeyView svk =
  case svk of
    StakingVerificationKeyShelley {} ->
      renderTextView $ TextView "StakingVerificationKeyShelley" "Free form text" cbor
  where
    cbor :: ByteString
    cbor = stakingVerificationKeyToCBOR svk

renderVerificationKeyVRFView :: ShelleyVRFVerificationKey -> ByteString
renderVerificationKeyVRFView vk =
    renderTextView $ TextView "VerificationKeyVRF" "Free form text" cbor
  where
    cbor :: ByteString
    cbor = verificationKeyVRFToCBOR vk

renderTxSignedView :: TxSigned -> ByteString
renderTxSignedView ts =
  case ts of
    TxSignedByron {} -> renderTextView $ TextView "TxSignedByron" "Free form text" cbor
    TxSignedShelley {} -> renderTextView $ TextView "TxSignedShelley" "Free form text" cbor
  where
    cbor :: ByteString
    cbor = txSignedToCBOR ts

renderTxUnsignedView :: TxUnsigned -> ByteString
renderTxUnsignedView tu =
  case tu of
    TxUnsignedByron {} -> renderTextView $ TextView "TxUnsignedByron" "Free form text" cbor
    TxUnsignedShelley {} -> renderTextView $ TextView "TxUnsignedShelley" "Free form text" cbor
  where
    cbor :: ByteString
    cbor = txUnsignedToCBOR tu

-- -------------------------------------------------------------------------------------------------


readAddress :: FilePath -> IO (Either ApiError Address)
readAddress path =
  runExceptT $ do
    bs <- handleIOExceptT (ApiErrorIO path) $ BS.readFile path
    hoistEither $ parseAddressView bs

readCertificate :: FilePath -> IO (Either ApiError Certificate)
readCertificate path =
  runExceptT $ do
    bs <- handleIOExceptT (ApiErrorIO path) $ BS.readFile path
    hoistEither $ parseCertificateView bs

readSigningKey :: FilePath -> IO (Either ApiError SigningKey)
readSigningKey path =
  runExceptT $ do
    bs <- handleIOExceptT (ApiErrorIO path) $ BS.readFile path
    hoistEither $ parseSigningKeyView bs

readGenesisVerificationKey :: FilePath -> IO (Either ApiError GenesisVerificationKey)
readGenesisVerificationKey path =
  runExceptT $ do
    bs <- handleIOExceptT (ApiErrorIO path) $ BS.readFile path
    hoistEither $ parseGenesisVerificationKeyView bs

readPaymentVerificationKey :: FilePath -> IO (Either ApiError PaymentVerificationKey)
readPaymentVerificationKey path =
  runExceptT $ do
    bs <- handleIOExceptT (ApiErrorIO path) $ BS.readFile path
    hoistEither $ parsePaymentVerificationKeyView bs

readStakingVerificationKey :: FilePath -> IO (Either ApiError StakingVerificationKey)
readStakingVerificationKey path =
  runExceptT $ do
    bs <- handleIOExceptT (ApiErrorIO path) $ BS.readFile path
    hoistEither $ parseStakingVerificationKeyView bs

readVRFVerificationKey :: FilePath -> IO (Either ApiError ShelleyVRFVerificationKey)
readVRFVerificationKey path =
  runExceptT $ do
    bs <- handleIOExceptT (ApiErrorIO path) $ BS.readFile path
    hoistEither $ parseVRFVerificationKeyView bs

readTxSigned :: FilePath -> IO (Either ApiError TxSigned)
readTxSigned path =
  runExceptT $ do
    bs <- handleIOExceptT (ApiErrorIO path) $ BS.readFile path
    hoistEither $ parseTxSignedView bs

readTxUnsigned :: FilePath -> IO (Either ApiError TxUnsigned)
readTxUnsigned path =
  runExceptT $ do
    bs <- handleIOExceptT (ApiErrorIO path) $ BS.readFile path
    hoistEither $ parseTxUnsignedView bs

readUpdate :: FilePath -> IO (Either ApiError Update)
readUpdate path =
  runExceptT $ do
    bs <- handleIOExceptT (ApiErrorIO path) $ BS.readFile path
    hoistEither $ parseUpdateView bs

writeAddress :: FilePath -> Address -> IO (Either ApiError ())
writeAddress path kp =
  runExceptT .
    handleIOExceptT (ApiErrorIO path) $ BS.writeFile path (renderAddressView kp)

writeCertificate :: FilePath -> Certificate -> IO (Either ApiError ())
writeCertificate path cert =
  runExceptT .
    handleIOExceptT (ApiErrorIO path) $ BS.writeFile path (renderCertificateView cert)

writeSigningKey :: FilePath -> SigningKey -> IO (Either ApiError ())
writeSigningKey path kp =
  runExceptT .
    handleIOExceptT (ApiErrorIO path) $ BS.writeFile path (renderSigningKeyView kp)

writePaymentVerificationKey :: FilePath -> PaymentVerificationKey -> IO (Either ApiError ())
writePaymentVerificationKey path kp =
  runExceptT .
    handleIOExceptT (ApiErrorIO path) $ BS.writeFile path (renderPaymentVerificationKeyView kp)

writeStakingVerificationKey :: FilePath -> StakingVerificationKey -> IO (Either ApiError ())
writeStakingVerificationKey path kp =
  runExceptT .
    handleIOExceptT (ApiErrorIO path) $ BS.writeFile path (renderStakingVerificationKeyView kp)

writeVRFVerificationKey :: FilePath -> ShelleyVRFVerificationKey -> IO (Either ApiError ())
writeVRFVerificationKey path kp =
  runExceptT .
    handleIOExceptT (ApiErrorIO path) $ BS.writeFile path (renderVerificationKeyVRFView kp)

writeTxSigned :: FilePath -> TxSigned -> IO (Either ApiError ())
writeTxSigned path kp =
  runExceptT .
    handleIOExceptT (ApiErrorIO path) $ BS.writeFile path (renderTxSignedView kp)

writeTxUnsigned :: FilePath -> TxUnsigned -> IO (Either ApiError ())
writeTxUnsigned path kp =
  runExceptT .
    handleIOExceptT (ApiErrorIO path) $ BS.writeFile path (renderTxUnsignedView kp)

writeUpdate :: FilePath -> Update -> IO (Either ApiError ())
writeUpdate path kp =
  runExceptT .
    handleIOExceptT (ApiErrorIO path) $ BS.writeFile path (renderUpdateView kp)
