{-# LANGUAGE OverloadedStrings #-}

module Cardano.Config.Shelley.OCert
  ( OperationalCertError(..)
  , readOperationalCert
  , renderOperationalCertError
  , signOperationalCertificate
  , writeOperationalCert
  ) where

import           Cardano.Prelude

import qualified Cardano.Binary as CBOR
import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)

import           Cardano.Config.TextView
import qualified Shelley.Spec.Ledger.Keys as Ledger
import           Shelley.Spec.Ledger.OCert
import           Shelley.Spec.Ledger.Serialization
import           Ouroboros.Consensus.Shelley.Protocol.Crypto
                   (TPraosStandardCrypto)



-- Local aliases for shorter types:
type VerKey   = Ledger.VKey TPraosStandardCrypto
type SignKey  = Ledger.SKey TPraosStandardCrypto
type VerKeyES = Ledger.VKeyES TPraosStandardCrypto
type Cert     = OCert TPraosStandardCrypto
type Sig      = Ledger.Sig TPraosStandardCrypto (VerKeyES, Natural, KESPeriod)

encodeOperationalCert :: (Cert,VerKey) -> TextView
encodeOperationalCert (oCert,vKey) =
  encodeToTextView tvType' tvTitle' operationalCertEncoder (oCert, vKey)
 where
  tvType' = "Cert"
  tvTitle' = "Operational Certificate"

decodeOperationalCert :: TextView -> Either TextViewError (Cert, VerKey)
decodeOperationalCert tView = do
  expectTextViewOfType "Cert" tView
  decodeFromTextView operationalCertDecoder tView


--TODO: this code would be a lot simpler without the extra newtype wrappers
-- that the ledger layers over the types from the Cardano.Crypto classes.

signOperationalCertificate
  :: VerKeyES
  -> SignKey
  -- ^ Counter.
  -> Natural
  -- ^ Start of key evolving signature period.
  -> KESPeriod
  -> Cert
signOperationalCertificate hotKESVerKey signingKey counter kesPeriod' =
  let oCertSig :: Sig
      oCertSig = Ledger.sign signingKey (hotKESVerKey, counter, kesPeriod')
   in OCert hotKESVerKey counter kesPeriod' oCertSig

data OperationalCertError = ReadOperationalCertError !TextViewFileError
                          | WriteOpertaionalCertError !TextViewFileError
                          deriving Show

renderOperationalCertError :: OperationalCertError -> Text
renderOperationalCertError err =
  case err of
    ReadOperationalCertError rErr ->
      "Operational certificate read error: " <> renderTextViewFileError rErr
    WriteOpertaionalCertError wErr ->
      "Operational certificate write error:" <> renderTextViewFileError wErr

readOperationalCert :: FilePath -> ExceptT OperationalCertError IO (Cert, VerKey)
readOperationalCert fp = do
    firstExceptT ReadOperationalCertError $ newExceptT $
      readTextViewEncodedFile decodeOperationalCert fp


writeOperationalCert :: FilePath -> Cert -> VerKey
                     -> ExceptT OperationalCertError IO ()
writeOperationalCert fp oCert vkey =
    firstExceptT WriteOpertaionalCertError $ newExceptT $
      writeTextViewEncodedFile encodeOperationalCert fp (oCert, vkey)


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
