module Cardano.Api.Byron.View
  ( parseByronAddressView
  , parseByronKeyPairView
  , parseByronPublicKeyView
  , readByronAddress
  , readByronPublicKey
  , renderByronAddressView
  , renderByronKeyPairView
  , renderByronPublicKeyView
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra
                   (handleIOExceptT, hoistEither, runExceptT)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import           Cardano.Api.Byron.CBOR
import           Cardano.Api.Error
import           Cardano.Api.Types

import           Cardano.Config.TextView

parseByronAddressView :: ByteString -> Either ApiError ByronAddress
parseByronAddressView bs =
  either convertTextViewError (byronAddressFromCBOR . tvRawCBOR) $ parseTextView bs

parseByronKeyPairView :: ByteString -> Either ApiError ByronKeyPair
parseByronKeyPairView bs =
  either convertTextViewError (byronKeyPairFromCBOR . tvRawCBOR) $ parseTextView bs

parseByronPublicKeyView :: ByteString -> Either ApiError ByronPublicKey
parseByronPublicKeyView bs =
  either convertTextViewError (byronPublicKeyFromCBOR . tvRawCBOR) $ parseTextView bs

readByronAddress :: FilePath -> IO (Either ApiError ByronAddress)
readByronAddress path =
  runExceptT $ do
    bs <- handleIOExceptT (ApiErrorIO path) $ BS.readFile path
    hoistEither $ parseByronAddressView bs

readByronPublicKey :: FilePath -> IO (Either ApiError ByronPublicKey)
readByronPublicKey path =
  runExceptT $ do
    bs <- handleIOExceptT (ApiErrorIO path) $ BS.readFile path
    hoistEither $ parseByronPublicKeyView bs

renderByronAddressView :: ByronAddress -> ByteString
renderByronAddressView ba = renderTextView $ TextView "ByronAddress" "Free form text" cbor
  where
    cbor :: ByteString
    cbor = byronAddressToCBOR ba

renderByronKeyPairView :: ByronKeyPair -> ByteString
renderByronKeyPairView kp = renderTextView $ TextView "PublicKeyByron" "Free form text" cbor
  where
    cbor :: ByteString
    cbor = byronKeyPairToCBOR kp

renderByronPublicKeyView :: ByronPublicKey -> ByteString
renderByronPublicKeyView pk =
   renderTextView $ TextView "PublicKeyByron" "Free form text" cbor
  where
    cbor :: ByteString
    cbor = renderByronPublicKeyToCBOR pk

