module Cardano.Api.View
  ( parseAddressView
  , parseSigningKeyView
  , parseVerificationKeyView
  , parseTxSignedView
  , parseTxUnsignedView

  , readAddress
  , readSigningKey
  , readVerificationKey
  , readTxSigned
  , readTxUnsigned

  , renderAddressView
  , renderSigningKeyView
  , renderVerificationKeyView
  , renderTxSignedView
  , renderTxUnsignedView

  , writeAddress
  , writeSigningKey
  , writeVerificationKey
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


parseAddressView :: ByteString -> Either ApiError Address
parseAddressView bs =
  either convertTextViewError (addressFromCBOR . tvRawCBOR) $ parseTextView bs

parseSigningKeyView :: ByteString -> Either ApiError SigningKey
parseSigningKeyView bs =
  either convertTextViewError (signingKeyFromCBOR . tvRawCBOR) $ parseTextView bs

parseVerificationKeyView :: ByteString -> Either ApiError VerificationKey
parseVerificationKeyView bs =
  either convertTextViewError (verificationKeyFromCBOR . tvRawCBOR) $ parseTextView bs

parseTxSignedView :: ByteString -> Either ApiError TxSigned
parseTxSignedView bs =
  either convertTextViewError (txSignedFromCBOR . tvRawCBOR) $ parseTextView bs

parseTxUnsignedView :: ByteString -> Either ApiError TxUnsigned
parseTxUnsignedView bs =
  either convertTextViewError (txUnsignedFromCBOR . tvRawCBOR) $ parseTextView bs

renderAddressView :: Address -> ByteString
renderAddressView addr =
  case addr of
    AddressByron {} -> renderTextView $ TextView "AddressByron" "Free form text" cbor
    AddressShelley {} -> renderTextView $ TextView "AddressShelley" "Free form text" cbor
  where
    cbor :: ByteString
    cbor = addressToCBOR addr

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

writeSigningKey :: FilePath -> SigningKey -> IO (Either ApiError ())
writeSigningKey path kp =
  runExceptT .
    handleIOExceptT (ApiErrorIO path) $ BS.writeFile path (renderSigningKeyView kp)

writeVerificationKey :: FilePath -> VerificationKey -> IO (Either ApiError ())
writeVerificationKey path kp =
  runExceptT .
    handleIOExceptT (ApiErrorIO path) $ BS.writeFile path (renderVerificationKeyView kp)

writeTxSigned :: FilePath -> TxSigned -> IO (Either ApiError ())
writeTxSigned path kp =
  runExceptT .
    handleIOExceptT (ApiErrorIO path) $ BS.writeFile path (renderTxSignedView kp)

writeTxUnsigned :: FilePath -> TxUnsigned -> IO (Either ApiError ())
writeTxUnsigned path kp =
  runExceptT .
    handleIOExceptT (ApiErrorIO path) $ BS.writeFile path (renderTxUnsignedView kp)
