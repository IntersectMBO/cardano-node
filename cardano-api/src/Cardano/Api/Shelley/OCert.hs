{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.Api.Shelley.OCert
  (
    -- * Reading and writing operational certificates
    readOperationalCert
  , writeOperationalCert

    -- * Reading and writing operational certificate issue counters
  , readOperationalCertIssueCounter
  , writeOperationalCertIssueCounter

    -- * Signing operational certificates
  , signOperationalCertificate
  , KESPeriod (..)

    -- * Errors
  , OperationalCertError(..)
  , renderOperationalCertError
  ) where

import           Cardano.Prelude

import qualified Data.ByteString.Char8 as BSC

import qualified Cardano.Binary as CBOR
import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)

import           Cardano.Api.TextView
import qualified Shelley.Spec.Ledger.Keys as Ledger
import           Shelley.Spec.Ledger.OCert
import           Shelley.Spec.Ledger.Serialization
import           Ouroboros.Consensus.Shelley.Protocol.Crypto
                   (TPraosStandardCrypto)



-- Local aliases for shorter types:
type VerKey    = Ledger.VKey Ledger.BlockIssuer TPraosStandardCrypto
type SignKey   = Ledger.SignKeyDSIGN TPraosStandardCrypto
type VerKeyKES = Ledger.VerKeyKES TPraosStandardCrypto
type Cert      = OCert TPraosStandardCrypto
type Sig       = Ledger.SignedDSIGN TPraosStandardCrypto
                                    (VerKeyKES, Natural, KESPeriod)

operationalCertTextViewType :: TextViewType
operationalCertTextViewType =
    "Node operational certificate"

operationalCertIssueCounterTextViewType :: TextViewType
operationalCertIssueCounterTextViewType =
    "Node operational certificate issue counter"


encodeOperationalCert :: (Cert, VerKey) -> TextView
encodeOperationalCert (oCert,vKey) =
    encodeToTextView
      operationalCertTextViewType
      description
      operationalCertEncoder
      (oCert, vKey)
  where
    description = "" --TODO: include the issuer key hash,
                     -- cert issue counter and KES starting period

decodeOperationalCert :: TextView -> Either TextViewError (Cert, VerKey)
decodeOperationalCert tView = do
  expectTextViewOfType operationalCertTextViewType tView
  decodeFromTextView operationalCertDecoder tView


encodeOperationalCertIssueCounter :: Natural -> TextView
encodeOperationalCertIssueCounter issueCount =
    encodeToTextView
      operationalCertIssueCounterTextViewType
      description
      CBOR.toCBOR
      issueCount
  where
    description = TextViewTitle $ "Next certificate issue number: "
                               <> BSC.pack (show issueCount)


decodeOperationalCertIssueCounter :: TextView -> Either TextViewError Natural
decodeOperationalCertIssueCounter tView = do
  expectTextViewOfType operationalCertIssueCounterTextViewType tView
  decodeFromTextView CBOR.fromCBOR tView


--TODO: this code would be a lot simpler without the extra newtype wrappers
-- that the ledger layers over the types from the Cardano.Crypto classes.

-- | The operational certificate delegates block/vote signing capability
-- to a hot KES key pair. You use a cold key (kept offline) to sign the certificate.
-- This way if your hot key is compromised, you can generate another KES pair and
-- create another operational certificate (signed by your cold key) to re-delegate
-- block/vote signing capability to that newly generated KES key pair.
signOperationalCertificate
  :: VerKeyKES
  -- ^ The operational hot KES verification key we are delegating block/vote
  -- signing capability to.
  -> SignKey
  -- ^ The cold\/offline key we are using to sign the operational certificate with.
  -> Natural
  -- ^ Certificate issue number. This allows us to establish the precedence of operational
  -- certificates. An operational key certificate with a higher counter overrides one
  -- with a lower counter.
  -> KESPeriod
  -- ^ Start of the validity period for this certificate.
  -> Cert
signOperationalCertificate hotKESVerKey signingKey counter kesPeriod' =
  let oCertSig :: Sig
      oCertSig = Ledger.signedDSIGN @TPraosStandardCrypto
                                    signingKey
                                    (hotKESVerKey, counter, kesPeriod')
   in OCert hotKESVerKey counter kesPeriod' oCertSig


data OperationalCertError =
       ReadOperationalCertError  !TextViewFileError
     | WriteOperationalCertError !TextViewFileError
     | ReadOperationalCertIssueCounterError  !TextViewFileError
     | WriteOpertaionalCertIssueCounterError !TextViewFileError
    deriving Show


renderOperationalCertError :: OperationalCertError -> Text
renderOperationalCertError err =
  case err of
    ReadOperationalCertError rErr ->
      "Operational certificate read error: " <> renderTextViewFileError rErr
    WriteOperationalCertError wErr ->
      "Operational certificate write error:" <> renderTextViewFileError wErr
    ReadOperationalCertIssueCounterError rErr ->
        "Operational certificate issue counter read error: "
     <> renderTextViewFileError rErr
    WriteOpertaionalCertIssueCounterError wErr ->
        "Operational certificate issue counter write error:"
     <> renderTextViewFileError wErr


readOperationalCert :: FilePath -> ExceptT OperationalCertError IO (Cert, VerKey)
readOperationalCert fp = do
    firstExceptT ReadOperationalCertError $ newExceptT $
      readTextViewEncodedFile decodeOperationalCert fp


writeOperationalCert :: FilePath -> Cert -> VerKey
                     -> ExceptT OperationalCertError IO ()
writeOperationalCert fp oCert vkey =
    firstExceptT WriteOperationalCertError $ newExceptT $
      writeTextViewEncodedFile encodeOperationalCert fp (oCert, vkey)


readOperationalCertIssueCounter :: FilePath
                                -> ExceptT OperationalCertError IO Natural
readOperationalCertIssueCounter fp = do
    firstExceptT ReadOperationalCertIssueCounterError $ newExceptT $
      readTextViewEncodedFile decodeOperationalCertIssueCounter fp


writeOperationalCertIssueCounter :: FilePath -> Natural
                                 -> ExceptT OperationalCertError IO ()
writeOperationalCertIssueCounter fp counter =
    firstExceptT WriteOpertaionalCertIssueCounterError $ newExceptT $
      writeTextViewEncodedFile encodeOperationalCertIssueCounter fp counter


-- We encode a pair of the operational cert and the corresponding vkey.
-- The 'OCert' type is only an instance of To/FromCBORGroup so to make it
-- into a proper CBOR value we have to go via CBORGroup.

operationalCertDecoder :: CBOR.Decoder s (Cert, VerKey)
operationalCertDecoder = do
    (CBORGroup oCert, vkey) <- CBOR.fromCBOR
    return (oCert, vkey)

operationalCertEncoder :: (Cert, VerKey) -> CBOR.Encoding
operationalCertEncoder (oCert, vkey) =
    CBOR.toCBOR (CBORGroup oCert, vkey)

--TODO: renderOperationalCertError
