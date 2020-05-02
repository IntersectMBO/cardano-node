module Cardano.Api.View
  ( parseAddressView
  , parseKeyPairView
  , parseVerificationKeyView
  , parseTxSignedView
  , parseTxUnsignedView

  , readAddress
  , readKeyPair
  , readVerificationKey
  , readTxSigned
  , readTxUnsigned

  , renderAddressView
  , renderKeyPairView
  , renderVerificationKeyView
  , renderTxSignedView
  , renderTxUnsignedView

  , writeAddress
  , writeKeyPair
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
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text


parseAddressView :: ByteString -> Either ApiError Address
parseAddressView bs =
  either convertTextViewError (addressFromCBOR . tvRawCBOR) $ parseTextView bs

parseKeyPairView :: ByteString -> Either ApiError KeyPair
parseKeyPairView bs =
  either convertTextViewError (keyPairFromCBOR . tvRawCBOR) $ parseTextView bs

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

renderKeyPairView :: KeyPair -> ByteString
renderKeyPairView kp =
  case kp of
    KeyPairByron {} -> renderTextView $ TextView "KeyPairByron" "Free form text" cbor
    KeyPairShelley {} -> renderTextView $ TextView "KeyPairShelley" "Free form text" cbor
  where
    cbor :: ByteString
    cbor = keyPairToCBOR kp

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

convertTextViewError :: TextViewError -> Either ApiError b
convertTextViewError err =
  Left $
    case err of
      TextViewFormatError msg -> ApiTextView msg

      TextViewTypeError [expected] actual ->
        ApiTextView $ mconcat
          [ "Expected file type ", Text.decodeLatin1 (unTextViewType expected)
          , ", but got type ", Text.decodeLatin1 (unTextViewType actual)
          ]

      TextViewTypeError expected actual ->
        ApiTextView $ mconcat
          [ "Expected file type to be one of "
          , Text.intercalate ", "
              [ Text.decodeLatin1 (unTextViewType t) | t <- expected ]
          , ", but got type ", Text.decodeLatin1 (unTextViewType actual)
          ]

      TextViewDecodeError derr -> ApiErrorCBOR derr

readAddress :: FilePath -> IO (Either ApiError Address)
readAddress path =
  runExceptT $ do
    bs <- handleIOExceptT (ApiErrorIO path) $ BS.readFile path
    hoistEither $ parseAddressView bs

readKeyPair :: FilePath -> IO (Either ApiError KeyPair)
readKeyPair path =
  runExceptT $ do
    bs <- handleIOExceptT (ApiErrorIO path) $ BS.readFile path
    hoistEither $ parseKeyPairView bs

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

writeKeyPair :: FilePath -> KeyPair -> IO (Either ApiError ())
writeKeyPair path kp =
  runExceptT .
    handleIOExceptT (ApiErrorIO path) $ BS.writeFile path (renderKeyPairView kp)

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
