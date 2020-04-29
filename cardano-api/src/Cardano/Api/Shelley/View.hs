module Cardano.Api.Shelley.View
  ( parseShelleyAddressView
  , parseShelleyKeyPairView
  , parseShelleyPublicKeyView
  , readShelleyAddress
  , readShelleyPublicKey
  , renderShelleyAddressView
  , renderShelleyKeyPairView
  , renderShelleyPublicKeyView
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra
                   (handleIOExceptT, hoistEither, runExceptT)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import           Cardano.Api.Shelley.CBOR
import           Cardano.Api.Error
import           Cardano.Api.Types

import           Cardano.Config.TextView

import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosStandardCrypto)
import           Shelley.Spec.Ledger.TxData (Addr(..))

parseShelleyAddressView :: ByteString -> Either ApiError (Addr TPraosStandardCrypto)
parseShelleyAddressView bs =
  either convertTextViewError (shelleyAddressFromCBOR . tvRawCBOR) $ parseTextView bs

readShelleyAddress :: FilePath -> IO (Either ApiError (Addr TPraosStandardCrypto))
readShelleyAddress path =
  runExceptT $ do
    bs <- handleIOExceptT (ApiErrorIO path) $ BS.readFile path
    hoistEither $ parseShelleyAddressView bs

renderShelleyAddressView :: Addr TPraosStandardCrypto -> ByteString
renderShelleyAddressView sa = renderTextView $ TextView "ShelleyAddress" "Free form text" cbor
  where
    cbor :: ByteString
    cbor = shelleyAddressToCBOR sa

parseShelleyKeyPairView :: ByteString -> Either ApiError ShelleyKeyPair
parseShelleyKeyPairView bs =
  either convertTextViewError (shelleyKeyPairFromCBOR . tvRawCBOR) $ parseTextView bs

parseShelleyPublicKeyView :: ByteString -> Either ApiError ShelleyPublicKey
parseShelleyPublicKeyView bs =
  either convertTextViewError (shelleyPublicKeyFromCBOR . tvRawCBOR) $ parseTextView bs

readShelleyPublicKey :: FilePath -> IO (Either ApiError ShelleyPublicKey)
readShelleyPublicKey path =
  runExceptT $ do
    bs <- handleIOExceptT (ApiErrorIO path) $ BS.readFile path
    hoistEither $ parseShelleyPublicKeyView bs

renderShelleyKeyPairView :: ShelleyKeyPair -> ByteString
renderShelleyKeyPairView kp = renderTextView $ TextView "KeyPairShelley" "Free form text" cbor
  where
    cbor :: ByteString
    cbor = shelleyKeyPairToCBOR kp

renderShelleyPublicKeyView :: ShelleyPublicKey -> ByteString
renderShelleyPublicKeyView pk =
    renderTextView $ TextView "ShelleyPublicKey" "Free form text" cbor
  where
    cbor :: ByteString
    cbor = renderShelleyPublicKeyToCBOR pk