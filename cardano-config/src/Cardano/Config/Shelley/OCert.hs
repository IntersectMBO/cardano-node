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

import           Shelley.Spec.Ledger.Keys (SKey, VKeyES, sign)
import           Shelley.Spec.Ledger.OCert
import           Shelley.Spec.Ledger.Serialization
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosStandardCrypto)


signOperationalCertificate
  :: VKeyES TPraosStandardCrypto
  -> SKey TPraosStandardCrypto
  -- ^ Counter.
  -> Natural
  -- ^ Start of key evolving signature period.
  -> KESPeriod
  -> OCert TPraosStandardCrypto
signOperationalCertificate hotKESVerKey signingKey counter kesPeriod' = do
  let oCertSig = sign signingKey (hotKESVerKey, counter, kesPeriod')
  OCert hotKESVerKey counter kesPeriod' oCertSig

data OperationalCertError = ReadOperationalCertError !FilePath !IOException
                          | DecodeOperationalCertError !FilePath !CBOR.DecoderError
                          | WriteOpertaionalCertError !FilePath !IOException

readOperationalCert :: FilePath -> ExceptT OperationalCertError IO (OCert TPraosStandardCrypto)
readOperationalCert fp = do
  bs <- handleIOExceptT (ReadOperationalCertError fp) $ LB.readFile fp
  firstExceptT (DecodeOperationalCertError fp) . hoistEither $ (CBOR.decodeFullDecoder "Operational Cert" fromCBORGroup bs)

writeOperationalCert :: FilePath -> OCert TPraosStandardCrypto -> ExceptT OperationalCertError IO ()
writeOperationalCert fp oCert =
  handleIOExceptT (WriteOpertaionalCertError fp) $ LB.writeFile fp (CBOR.serializeEncoding $ toCBORGroup oCert)
