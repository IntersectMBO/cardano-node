{-# LANGUAGE OverloadedStrings #-}

module Cardano.Config.Shelley.KES
  ( genKESKeyPair
  , readKESSigningKey
  , readKESVerKey
  , writeKESSigningKey
  , writeKESVerKey
  ) where

import           Cardano.Prelude

import qualified Data.ByteString.Lazy.Char8 as LB

import qualified Cardano.Binary as CBOR
import           Control.Monad.Trans.Except.Extra
                   (firstExceptT, handleIOExceptT, hoistEither)

import           Cardano.Crypto.KES.Class
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosStandardCrypto)
import           Shelley.Spec.Ledger.Keys (SKeyES (..), VKeyES (..))

genKESKeyPair :: Natural -> IO (VKeyES TPraosStandardCrypto, SKeyES TPraosStandardCrypto)
genKESKeyPair duration = do
  signKeyKES <- genKeyKES duration
  let verKeyKes = deriveVerKeyKES signKeyKES
  pure $ (VKeyES verKeyKes,SKeyES signKeyKES)

data KESError = ReadKESSigningKeyError !FilePath !IOException
              | ReadKESVerKeyError !FilePath !IOException
              | DecodeKESSigningKeyError !FilePath !CBOR.DecoderError
              | DecodeKESVerKeyError !FilePath !CBOR.DecoderError
              | WriteKESSigningKeyError !FilePath !IOException
              | WriteKESVerKeyError !FilePath !IOException

readKESSigningKey :: FilePath ->  ExceptT KESError IO (SKeyES TPraosStandardCrypto)
readKESSigningKey fp = do
  bs <- handleIOExceptT (ReadKESSigningKeyError fp) $ LB.readFile fp
  firstExceptT (DecodeKESSigningKeyError fp) . hoistEither $ second SKeyES $ CBOR.decodeFull bs

writeKESSigningKey :: FilePath -> SKeyES TPraosStandardCrypto -> ExceptT KESError IO ()
writeKESSigningKey fp (SKeyES sKeyKES) =
  handleIOExceptT (WriteKESSigningKeyError fp) $ LB.writeFile fp (CBOR.serialize sKeyKES)

readKESVerKey :: FilePath -> ExceptT KESError IO (VKeyES TPraosStandardCrypto)
readKESVerKey fp = do
  bs <- handleIOExceptT (ReadKESVerKeyError fp) $ LB.readFile fp
  firstExceptT (DecodeKESVerKeyError fp) . hoistEither $ CBOR.decodeFull bs

writeKESVerKey :: FilePath -> VKeyES TPraosStandardCrypto -> ExceptT KESError IO ()
writeKESVerKey fp vKeyKES =
  handleIOExceptT (WriteKESVerKeyError fp) $ LB.writeFile fp (CBOR.serialize vKeyKES)
