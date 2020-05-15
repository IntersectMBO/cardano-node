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

import           Cardano.Crypto.Seed (readSeedFromSystemEntropy)
import           Cardano.Crypto.VRF.Class
                   (deriveVerKeyVRF, genKeyVRF, seedSizeVRF)
import qualified Shelley.Spec.Ledger.Keys as Ledger
import qualified Shelley.Spec.Ledger.Crypto as Ledger
import           Ouroboros.Consensus.Shelley.Protocol.Crypto
                   (TPraosStandardCrypto)


--TODO: There are VRF reading and writing functions that also exist in Cardano.Api.View
-- This needs to be sorted out

-- Local type aliases
type SignKey = Ledger.SignKeyVRF TPraosStandardCrypto
type VerKey  = Ledger.VerKeyVRF  TPraosStandardCrypto
type VRF     = Ledger.VRF TPraosStandardCrypto


data VRFError = ReadVRFSigningKeyError !TextViewFileError
              | ReadVRFVerKeyError !TextViewFileError
              | WriteVRFSigningKeyError !TextViewFileError
              | WriteVRFVerKeyError !TextViewFileError
              deriving Show

encodeVRFSigningKey :: SignKey -> TextView
encodeVRFSigningKey vKeyEs =
  encodeToTextView tvType' tvTitle' CBOR.toCBOR vKeyEs
 where
  tvType' = "SignKeyVRF SimpleVRF"
  tvTitle' = "VRF Signing Key"

decodeVRFSigningKey :: TextView -> Either TextViewError SignKey
decodeVRFSigningKey tView = do
  expectTextViewOfType "SignKeyVRF SimpleVRF" tView
  decodeFromTextView CBOR.fromCBOR tView

encodeVRFVerificationKey :: VerKey -> TextView
encodeVRFVerificationKey vKeyEs =
  encodeToTextView tvType' tvTitle' CBOR.toCBOR vKeyEs
 where
  tvType' = "VerKeyVRF SimpleVRF"
  tvTitle' = "VRF Verification Key"

decodeVRFVerificationKey :: TextView -> Either TextViewError VerKey
decodeVRFVerificationKey tView = do
  expectTextViewOfType "VerKeyVRF SimpleVRF" tView
  decodeFromTextView CBOR.fromCBOR tView

genVRFKeyPair :: IO (SignKey, VerKey)
genVRFKeyPair = do
  seed <- readSeedFromSystemEntropy (seedSizeVRF (Proxy :: Proxy VRF))
  let sKeyVRF = genKeyVRF seed
      vKeyVRF = deriveVerKeyVRF sKeyVRF
  pure (sKeyVRF, vKeyVRF)

renderVRFError :: VRFError -> Text
renderVRFError vrfErr =
  case vrfErr of
    ReadVRFSigningKeyError err -> "VRF signing key read error: " <> renderTextViewFileError err
    ReadVRFVerKeyError err -> "VRF verification key read error : " <> renderTextViewFileError err
    WriteVRFSigningKeyError err -> "VRF signing key write error: " <> renderTextViewFileError err
    WriteVRFVerKeyError err-> "VRF verification key write error: " <> renderTextViewFileError err

readVRFSigningKey :: FilePath -> ExceptT VRFError IO SignKey
readVRFSigningKey fp = do
  firstExceptT ReadVRFSigningKeyError
    . newExceptT $ readTextViewEncodedFile decodeVRFSigningKey fp

writeVRFSigningKey :: FilePath -> SignKey -> ExceptT VRFError IO ()
writeVRFSigningKey fp vKeyVRF =
  firstExceptT WriteVRFSigningKeyError
    . newExceptT $ writeTextViewEncodedFile encodeVRFSigningKey fp vKeyVRF

readVRFVerKey :: FilePath ->  ExceptT VRFError IO VerKey
readVRFVerKey fp = do
  firstExceptT ReadVRFVerKeyError
    . newExceptT $ readTextViewEncodedFile decodeVRFVerificationKey fp

writeVRFVerKey :: FilePath -> VerKey -> ExceptT VRFError IO ()
writeVRFVerKey fp vKeyVRF =
  firstExceptT WriteVRFVerKeyError
    . newExceptT $ writeTextViewEncodedFile encodeVRFVerificationKey fp vKeyVRF
