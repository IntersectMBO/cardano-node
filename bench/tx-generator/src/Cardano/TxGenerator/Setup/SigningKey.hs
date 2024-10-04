{-# LANGUAGE DataKinds #-}

-- | This module provides convenience functions when dealing with signing keys.
module Cardano.TxGenerator.Setup.SigningKey
       ( parseSigningKeyTE
       , parseSigningKeyBase16
       , readDRepKeyFile
       , readSigningKeyFile
       , readStakeKeyFile
       , PaymentKey
       , SigningKey
       , StakeKey
       )
       where

import           Cardano.Api

import           Cardano.CLI.Types.Common (SigningKeyFile)
import           Cardano.TxGenerator.Types (TxGenError (..))

import           Data.Bifunctor (first)
import qualified Data.ByteString as BS (ByteString)
import           Data.ByteString.Base16 as Base16 (decode)


parseSigningKeyTE :: TextEnvelope -> Either TxGenError (SigningKey PaymentKey)
parseSigningKeyTE
  = first ApiError . deserialiseFromTextEnvelopeAnyOf acceptedTypes

parseSigningKeyBase16 :: BS.ByteString -> Either TxGenError (SigningKey PaymentKey)
parseSigningKeyBase16 k
  = either
    (const $ Left $ TxGenError "parseSigningKeyBase16: ill-formed base16 encoding")
    (parseSigningKeyTE . asTE)
    (Base16.decode k)
  where
    asTE addr = TextEnvelope {
          teType = "PaymentSigningKeyShelley_ed25519"
        , teDescription = "Payment Signing Key"
        , teRawCBOR = addr
        }

readSigningKeyFile :: SigningKeyFile In -> IO (Either TxGenError (SigningKey PaymentKey))
readSigningKeyFile f = first ApiError <$> readFileTextEnvelopeAnyOf acceptedTypes f

readDRepKeyFile :: SigningKeyFile In -> IO (Either TxGenError (SigningKey DRepKey))
readDRepKeyFile f = first ApiError <$> readKeyFileTextEnvelope (AsSigningKey AsDRepKey) f

readStakeKeyFile :: SigningKeyFile In -> IO (Either TxGenError (SigningKey StakeKey))
readStakeKeyFile f = first ApiError <$> readKeyFileTextEnvelope (AsSigningKey AsStakeKey) f

acceptedTypes :: [FromSomeType HasTextEnvelope (SigningKey PaymentKey)]
acceptedTypes =
    [ FromSomeType (AsSigningKey AsGenesisUTxOKey) castSigningKey
    , FromSomeType (AsSigningKey AsPaymentKey) id
    ]
