module Cardano.Api.View
  ( parseAddressView
  , parseCertificateView
  , parseSigningKeyView
  , parseVerificationKeyView
  , parseVerificationKeyStakePoolView
  , parseVerificationKeyStakingView
  , parseVerificationKeyVRFView
  , parseTxSignedView
  , parseTxUnsignedView

  , readAddress
  , readCertificate
  , readSigningKey
  , readVerificationKey
  , readVRFVerificationKey
  , readVerificationKeyStakePool
  , readVerificationKeyStaking
  , readTxSigned
  , readTxUnsigned

  , renderAddressView
  , renderCertificateView
  , renderSigningKeyView
  , renderVerificationKeyView
  , renderVerificationKeyStakePoolView
  , renderVerificationKeyStakingView
  , renderVerificationKeyVRFView
  , renderTxSignedView
  , renderTxUnsignedView

  , writeAddress
  , writeCertificate
  , writeSigningKey
  , writeVerificationKey
  , writeVerificationKeyStakePool
  , writeVerificationKeyStaking
  , writeVRFVerificationKey
  , writeTxSigned
  , writeTxUnsigned
  ) where

import           Cardano.Api.CBOR
import           Cardano.Api.Types
import           Cardano.Api.Error

import           Cardano.Config.TextView

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra (handleIOExceptT, hoistEither, runExceptT)

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

--TODO: There are VRF reading and writing functions that also exist in Cardano.Config.Shelley.VRF
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

parseVerificationKeyView :: ByteString -> Either ApiError VerificationKey
parseVerificationKeyView bs =
  verificationKeyFromCBOR . tvRawCBOR =<< first ApiTextView (parseTextView bs)

parseVRFVerificationKeyView :: ByteString -> Either ApiError ShelleyVRFVerificationKey
parseVRFVerificationKeyView bs =
  verificationKeyVRFFromCBOR . tvRawCBOR =<< first ApiTextView (parseTextView bs)

parseVerificationKeyStakePoolView :: ByteString -> Either ApiError ShelleyVerificationKeyStakePool
parseVerificationKeyStakePoolView bs =
  verificationKeyStakePoolFromCBOR . tvRawCBOR =<< first ApiTextView (parseTextView bs)

parseVerificationKeyStakingView :: ByteString -> Either ApiError ShelleyVerificationKeyStaking
parseVerificationKeyStakingView bs =
  verificationKeyStakingFromCBOR . tvRawCBOR =<< first ApiTextView (parseTextView bs)

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

renderSigningKeyView :: SigningKey -> ByteString
renderSigningKeyView kp =
  case kp of
    SigningKeyByron {} -> renderTextView $ TextView "SigningKeyByron" "Free form text" cbor
    SigningKeyShelley {} -> renderTextView $ TextView "SigningKeyShelley" "Free form text" cbor
  where
    cbor :: ByteString
    cbor = signingKeyToCBOR kp

renderVerificationKeyView :: VerificationKey -> ByteString
renderVerificationKeyView pk =
  case pk of
    VerificationKeyByron {} -> renderTextView $ TextView "VerificationKeyByron" "Free form text" cbor
    VerificationKeyShelley {} -> renderTextView $ TextView "VerificationKeyShelley" "Free form text" cbor
  where
    cbor :: ByteString
    cbor = verificationKeyToCBOR pk

renderVerificationKeyStakingView :: ShelleyVerificationKeyStaking -> ByteString
renderVerificationKeyStakingView vk =
    renderTextView $ TextView "VerificationKeyStakingShelley" "Free form text" cbor
  where
    cbor :: ByteString
    cbor = verificationKeyStakingToCBOR vk

renderVerificationKeyStakePoolView :: ShelleyVerificationKeyStakePool -> ByteString
renderVerificationKeyStakePoolView vk =
    renderTextView $ TextView "VerificationKeyStakePoolShelley" "Free form text" cbor
  where
    cbor :: ByteString
    cbor = verificationKeyStakePoolToCBOR vk

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

readVerificationKey :: FilePath -> IO (Either ApiError VerificationKey)
readVerificationKey path =
  runExceptT $ do
    bs <- handleIOExceptT (ApiErrorIO path) $ BS.readFile path
    hoistEither $ parseVerificationKeyView bs

readVRFVerificationKey :: FilePath -> IO (Either ApiError ShelleyVRFVerificationKey)
readVRFVerificationKey path =
  runExceptT $ do
    bs <- handleIOExceptT (ApiErrorIO path) $ BS.readFile path
    hoistEither $ parseVRFVerificationKeyView bs

readVerificationKeyStakePool :: FilePath -> IO (Either ApiError ShelleyVerificationKeyStakePool)
readVerificationKeyStakePool path =
  runExceptT $ do
    bs <- handleIOExceptT (ApiErrorIO path) $ BS.readFile path
    hoistEither $ parseVerificationKeyStakePoolView bs

readVerificationKeyStaking :: FilePath -> IO (Either ApiError ShelleyVerificationKeyStaking)
readVerificationKeyStaking path =
  runExceptT $ do
    bs <- handleIOExceptT (ApiErrorIO path) $ BS.readFile path
    hoistEither $ parseVerificationKeyStakingView bs

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

writeVerificationKey :: FilePath -> VerificationKey -> IO (Either ApiError ())
writeVerificationKey path kp =
  runExceptT .
    handleIOExceptT (ApiErrorIO path) $ BS.writeFile path (renderVerificationKeyView kp)

writeVerificationKeyStaking :: FilePath -> ShelleyVerificationKeyStaking -> IO (Either ApiError ())
writeVerificationKeyStaking path kp =
  runExceptT .
    handleIOExceptT (ApiErrorIO path) $ BS.writeFile path (renderVerificationKeyStakingView kp)

writeVRFVerificationKey :: FilePath -> ShelleyVRFVerificationKey -> IO (Either ApiError ())
writeVRFVerificationKey path kp =
  runExceptT .
    handleIOExceptT (ApiErrorIO path) $ BS.writeFile path (renderVerificationKeyVRFView kp)

writeVerificationKeyStakePool :: FilePath -> ShelleyVerificationKeyStakePool -> IO (Either ApiError ())
writeVerificationKeyStakePool path kp =
  runExceptT .
    handleIOExceptT (ApiErrorIO path) $ BS.writeFile path (renderVerificationKeyStakePoolView kp)

writeTxSigned :: FilePath -> TxSigned -> IO (Either ApiError ())
writeTxSigned path kp =
  runExceptT .
    handleIOExceptT (ApiErrorIO path) $ BS.writeFile path (renderTxSignedView kp)

writeTxUnsigned :: FilePath -> TxUnsigned -> IO (Either ApiError ())
writeTxUnsigned path kp =
  runExceptT .
    handleIOExceptT (ApiErrorIO path) $ BS.writeFile path (renderTxUnsignedView kp)
