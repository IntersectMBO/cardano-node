{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides convenience functions when dealing with signing keys.
module Cardano.TxGenerator.Setup.SigningKey
       ( parseDRepKeyBase16
       , parsePaymentKeyBase16
       , parseStakeKeyBase16
       , parsePaymentKeyTE
       , readDRepKeyFile
       , readPaymentKeyFile
       , readStakeKeyFile
       , PaymentKey
       , SigningKey
       , module CLI
       )
       where

import           Cardano.Api

import           Cardano.CLI.Types.Common as CLI (SigningKeyFile, VerificationKeyFile)
import           Cardano.TxGenerator.Types (TxGenError (..))

import           Data.Bifunctor (first)
import qualified Data.ByteString as BS (ByteString)
import           Data.ByteString.Base16 as Base16 (decode)


parsePaymentKeyTE :: TextEnvelope -> Either TxGenError (SigningKey PaymentKey)
parsePaymentKeyTE
  = first ApiError . deserialiseFromTextEnvelopeAnyOf acceptedTypes

parsePaymentKeyBase16 :: BS.ByteString -> Either TxGenError (SigningKey PaymentKey)
parsePaymentKeyBase16 k
  = parseSigningKeyBase16 AsPaymentKey acceptedTypes k teTemplate
  where
    teTemplate = TextEnvelope {
        teType = "PaymentSigningKeyShelley_ed25519"
      , teDescription = "Payment Signing Key"
      , teRawCBOR = ""
      }

parseDRepKeyBase16 ::  BS.ByteString -> Either TxGenError (SigningKey DRepKey)
parseDRepKeyBase16 k
  = parseSigningKeyBase16 AsDRepKey [] k teTemplate
  where
    teTemplate = TextEnvelope {
        teType = TextEnvelopeType "DRepSigningKey_ed25519"
      , teDescription = "Delegated Representative Signing Key"
      , teRawCBOR = ""
      }

parseStakeKeyBase16 ::  BS.ByteString -> Either TxGenError (VerificationKey StakeKey)
parseStakeKeyBase16 key
  = do
      key' <- parseBase16 key
      first ApiError $
        deserialiseFromTextEnvelope (AsVerificationKey AsStakeKey) (teTemplate key')
  where
    teTemplate k = TextEnvelope {
        teType = TextEnvelopeType "StakeVerificationKeyShelley_ed25519"
      , teDescription = "Stake Verification Key"
      , teRawCBOR = k
      }

parseBase16 :: BS.ByteString -> Either TxGenError BS.ByteString
parseBase16
  = first (const $ TxGenError "parseBase16: ill-formed base16 encoding")
  . Base16.decode

parseSigningKeyBase16
  :: HasTextEnvelope (SigningKey k)
  => AsType k
  -> [FromSomeType HasTextEnvelope (SigningKey k)]
  -> BS.ByteString -> TextEnvelope -> Either TxGenError (SigningKey k)
parseSigningKeyBase16 k paymentKeys key te = do
  key' <- parseBase16 key
  let te' = te {teRawCBOR = key'}
  first ApiError $ if null paymentKeys
    then deserialiseFromTextEnvelope (AsSigningKey k) te'
    else deserialiseFromTextEnvelopeAnyOf paymentKeys te'

readPaymentKeyFile :: SigningKeyFile In -> IO (Either TxGenError (SigningKey PaymentKey))
readPaymentKeyFile f = first ApiError <$> readFileTextEnvelopeAnyOf acceptedTypes f

readDRepKeyFile :: SigningKeyFile In -> IO (Either TxGenError (SigningKey DRepKey))
readDRepKeyFile f = first ApiError <$> readKeyFileTextEnvelope (AsSigningKey AsDRepKey) f

readStakeKeyFile :: VerificationKeyFile In -> IO (Either TxGenError (VerificationKey StakeKey))
readStakeKeyFile f = first ApiError <$> readKeyFileTextEnvelope (AsVerificationKey AsStakeKey) f

acceptedTypes :: [FromSomeType HasTextEnvelope (SigningKey PaymentKey)]
acceptedTypes =
    [ FromSomeType (AsSigningKey AsGenesisUTxOKey) castSigningKey
    , FromSomeType (AsSigningKey AsPaymentKey) id
    ]
