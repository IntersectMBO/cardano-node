{-# LANGUAGE OverloadedStrings #-}

module Cardano.Config.Shelley.VRF
  ( VRFError(..)
  , decodeVRFVerificationKey
  , encodeVRFVerificationKey
  , genVRFKeyPair
  , readVRFSigningKey
  , readVRFVerKey
  , renderVRFError
  , writeVRFSigningKey
  , writeVRFVerKey
  ) where

import           Cardano.Prelude

import qualified Cardano.Binary as CBOR
import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)

import           Cardano.Config.TextView
import           Cardano.Crypto.VRF.Class
                   (SignKeyVRF, VerKeyVRF, deriveVerKeyVRF, genKeyVRF)
import           Cardano.Crypto.VRF.Simple (SimpleVRF)


data VRFError = ReadVRFSigningKeyError !TextViewFileError
              | ReadVRFVerKeyError !TextViewFileError
              | WriteVRFSigningKeyError !TextViewFileError
              | WriteVRFVerKeyError !TextViewFileError

encodeVRFSigningKey :: SignKeyVRF SimpleVRF -> TextView
encodeVRFSigningKey vKeyEs =
  encodeToTextView tvType' tvTitle' CBOR.toCBOR vKeyEs
 where
  tvType' = "SignKeyVRF SimpleVRF"
  tvTitle' = "VRF Signing Key"

decodeVRFSigningKey :: TextView -> Either TextViewError (SignKeyVRF SimpleVRF)
decodeVRFSigningKey tView = do
  expectTextViewOfType "SignKeyVRF SimpleVRF" tView
  decodeFromTextView CBOR.fromCBOR tView

encodeVRFVerificationKey :: VerKeyVRF SimpleVRF -> TextView
encodeVRFVerificationKey vKeyEs =
  encodeToTextView tvType' tvTitle' CBOR.toCBOR vKeyEs
 where
  tvType' = "VerKeyVRF SimpleVRF"
  tvTitle' = "VRF Verification Key"

decodeVRFVerificationKey :: TextView -> Either TextViewError (VerKeyVRF SimpleVRF)
decodeVRFVerificationKey tView = do
  expectTextViewOfType "VerKeyVRF SimpleVRF" tView
  decodeFromTextView CBOR.fromCBOR tView

genVRFKeyPair :: IO (SignKeyVRF SimpleVRF, VerKeyVRF SimpleVRF)
genVRFKeyPair = do sKeyVRF <- genKeyVRF
                   pure (sKeyVRF, deriveVerKeyVRF sKeyVRF)

renderVRFError :: VRFError -> Text
renderVRFError vrfErr =
  case vrfErr of
    ReadVRFSigningKeyError err -> "VRF signing key read error: " <> renderTextViewFileError err
    ReadVRFVerKeyError err -> "VRF verification key read error : " <> renderTextViewFileError err
    WriteVRFSigningKeyError err -> "VRF signing key write error: " <> renderTextViewFileError err
    WriteVRFVerKeyError err-> "VRF verification key write error: " <> renderTextViewFileError err

readVRFSigningKey :: FilePath -> ExceptT VRFError IO (SignKeyVRF SimpleVRF)
readVRFSigningKey fp = do
  firstExceptT ReadVRFSigningKeyError
    . newExceptT $ readTextViewEncodedFile decodeVRFSigningKey fp

writeVRFSigningKey :: FilePath -> SignKeyVRF SimpleVRF -> ExceptT VRFError IO ()
writeVRFSigningKey fp vKeyVRF =
  firstExceptT WriteVRFSigningKeyError
    . newExceptT $ writeTextViewEncodedFile encodeVRFSigningKey fp vKeyVRF

readVRFVerKey :: FilePath ->  ExceptT VRFError IO (VerKeyVRF SimpleVRF)
readVRFVerKey fp = do
  firstExceptT ReadVRFVerKeyError
    . newExceptT $ readTextViewEncodedFile decodeVRFVerificationKey fp

writeVRFVerKey :: FilePath -> VerKeyVRF SimpleVRF -> ExceptT VRFError IO ()
writeVRFVerKey fp vKeyVRF =
  firstExceptT WriteVRFVerKeyError
    . newExceptT $ writeTextViewEncodedFile encodeVRFVerificationKey fp vKeyVRF
