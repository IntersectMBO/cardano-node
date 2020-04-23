{-# LANGUAGE OverloadedStrings #-}

module Cardano.Config.Shelley.KES
  ( KESError
  , genKESKeyPair
  , readKESSigningKey
  , readKESVerKey
  , renderKESError
  , writeKESSigningKey
  , writeKESVerKey
  ) where

import           Cardano.Prelude
import           Prelude (String)

import qualified Data.ByteString.Lazy.Char8 as LB

import qualified Cardano.Binary as CBOR
import           Control.Monad.Trans.Except.Extra
                   (firstExceptT, handleIOExceptT, hoistEither)

import           Cardano.Crypto.KES.Class
import           Ouroboros.Consensus.Shelley.Protocol.Crypto
                   (TPraosStandardCrypto, KES)


-- Local aliases for shorter types:
type VerKey  = VerKeyKES  (KES TPraosStandardCrypto)
type SignKey = SignKeyKES (KES TPraosStandardCrypto)


genKESKeyPair :: Natural -> IO (VerKey, SignKey)
genKESKeyPair duration = do
  signKeyKES <- genKeyKES duration
  let verKeyKes = deriveVerKeyKES signKeyKES
  pure (verKeyKes, signKeyKES)

data KESError = ReadKESSigningKeyError !FilePath !IOException
              | ReadKESVerKeyError !FilePath !IOException
              | DecodeKESSigningKeyError !FilePath !CBOR.DecoderError
              | DecodeKESVerKeyError !FilePath !CBOR.DecoderError
              | WriteKESSigningKeyError !FilePath !IOException
              | WriteKESVerKeyError !FilePath !IOException
  deriving Show

readKESSigningKey :: FilePath ->  ExceptT KESError IO SignKey
readKESSigningKey fp = do
  bs <- handleIOExceptT (ReadKESSigningKeyError fp) $ LB.readFile fp
  firstExceptT (DecodeKESSigningKeyError fp) . hoistEither $ CBOR.decodeFull bs

renderKESError :: KESError -> String
renderKESError kesErr =
  case kesErr of
    ReadKESSigningKeyError fp ioExcptn -> "KES signing key read error at: " <> fp
                                          <> " Error: " <> show ioExcptn

    ReadKESVerKeyError fp ioExcptn -> "KES verification key read error at: " <> fp
                                      <> " Error: " <> show ioExcptn

    DecodeKESSigningKeyError fp cborDecErr -> "KES signing key decode error at: " <> fp
                                              <> " Error: " <> show cborDecErr

    DecodeKESVerKeyError fp cborDecErr -> "KES verification key decode error at: " <> fp
                                          <> " Error: " <> show cborDecErr

    WriteKESSigningKeyError fp ioExcptn -> "KES signing key write error at: " <> fp
                                           <> " Error: " <> show ioExcptn

    WriteKESVerKeyError fp ioExcptn -> "KES verification key write error at: " <> fp
                                       <> " Error: " <> show ioExcptn


writeKESSigningKey :: FilePath -> SignKey -> ExceptT KESError IO ()
writeKESSigningKey fp sKeyKES =
  handleIOExceptT (WriteKESSigningKeyError fp) $ LB.writeFile fp (CBOR.serialize sKeyKES)

readKESVerKey :: FilePath -> ExceptT KESError IO VerKey
readKESVerKey fp = do
  bs <- handleIOExceptT (ReadKESVerKeyError fp) $ LB.readFile fp
  firstExceptT (DecodeKESVerKeyError fp) . hoistEither $ CBOR.decodeFull bs

writeKESVerKey :: FilePath -> VerKey -> ExceptT KESError IO ()
writeKESVerKey fp vKeyKES =
  handleIOExceptT (WriteKESVerKeyError fp) $ LB.writeFile fp (CBOR.serialize vKeyKES)
