{-# LANGUAGE OverloadedStrings #-}

module Cardano.Config.Shelley.OCert
  ( OperationalCertError(..)
  , readOperationalCert
  , signOperationalCertificate
  , writeOperationalCert
  ) where

import           Cardano.Prelude

import qualified Cardano.Binary as CBOR
import           Control.Monad.Trans.Except.Extra
                   (firstExceptT, handleIOExceptT, hoistEither)
import qualified Data.ByteString.Lazy.Char8 as LB

import           Cardano.Crypto.DSIGN.Class
import           Cardano.Crypto.KES.Class
import           Shelley.Spec.Ledger.Keys (SKey(..), VKeyES(..), Sig, sign)
import           Shelley.Spec.Ledger.OCert
import           Shelley.Spec.Ledger.Serialization
import           Ouroboros.Consensus.Shelley.Protocol.Crypto
                   (TPraosStandardCrypto, DSIGN, KES)



-- Local aliases for shorter types:
type VerKey   = VerKeyDSIGN  (DSIGN TPraosStandardCrypto)
type SignKey  = SignKeyDSIGN (DSIGN TPraosStandardCrypto)
type VerKeyES = VerKeyKES    (KES TPraosStandardCrypto)
type Cert     = OCert TPraosStandardCrypto

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
  let oCertSig :: Sig TPraosStandardCrypto (VKeyES TPraosStandardCrypto, Natural, KESPeriod)
      oCertSig = sign (SKey signingKey) (VKeyES hotKESVerKey, counter, kesPeriod')
   in OCert (VKeyES hotKESVerKey) counter kesPeriod' oCertSig

data OperationalCertError = ReadOperationalCertError !FilePath !IOException
                          | DecodeOperationalCertError !FilePath !CBOR.DecoderError
                          | WriteOpertaionalCertError !FilePath !IOException
  deriving Show

readOperationalCert :: FilePath -> ExceptT OperationalCertError IO (Cert, VerKey)
readOperationalCert fp = do
  bs <- handleIOExceptT (ReadOperationalCertError fp) $ LB.readFile fp
  firstExceptT (DecodeOperationalCertError fp) $ hoistEither $
    CBOR.decodeFullDecoder "Operational Cert" decodeOperationalCert bs
  where

writeOperationalCert :: FilePath -> Cert -> VerKey
                     -> ExceptT OperationalCertError IO ()
writeOperationalCert fp oCert vkey =
  handleIOExceptT (WriteOpertaionalCertError fp) $
    LB.writeFile fp $ CBOR.serializeEncoding $
      encodeOperationalCert (oCert, vkey)
  where


-- We encode a pair of the operational cert and the corresponding vkey.
-- The 'OCert' type is only an instance of To/FromCBORGroup so to make it
-- into a proper CBOR value we have to go via CBORGroup.

decodeOperationalCert :: CBOR.Decoder s (Cert, VerKey)
decodeOperationalCert = do
    (CBORGroup oCert, vkey) <- CBOR.fromCBOR
    return (oCert, vkey)

encodeOperationalCert :: (Cert, VerKey) -> CBOR.Encoding
encodeOperationalCert (oCert, vkey) =
    CBOR.toCBOR (CBORGroup oCert, vkey)

--TODO: renderOperationalCertError
