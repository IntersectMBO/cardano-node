{-# LANGUAGE OverloadedStrings #-}

module Cardano.Config.Shelley.ColdKeys
  ( KeyError(..)
  , KeyType(..)
  , OperatorKeyRole(..)
  , decodeVerificationKey
  , encodeVerificationKey
  , genKeyPair
  , readSigningKey
  , readVerKey
  , renderKeyError
  , writeSigningKey
  , writeVerKey
  ) where

import           Cardano.Prelude

import qualified Cardano.Binary as CBOR
import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)

import           Cardano.Crypto.DSIGN.Class
import           Cardano.Crypto.Random (runSecureRandom)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto
                   (TPraosStandardCrypto, DSIGN)

import           Cardano.Config.TextView


data KeyType = GenesisKey | OperatorKey OperatorKeyRole

data OperatorKeyRole = GenesisDelegateKey | StakePoolOperatorKey


renderKeyType :: KeyType -> TextViewType
renderKeyType GenesisKey    = "Genesis"
renderKeyType OperatorKey{} = "Node operator"

renderKeyRole :: KeyType -> TextViewTitle
renderKeyRole GenesisKey                         = "Genesis key"
renderKeyRole (OperatorKey GenesisDelegateKey)   = "Genesis delegate operator key"
renderKeyRole (OperatorKey StakePoolOperatorKey) = "Stake pool operator key"


-- Local aliases for shorter types:
type VerKey  = VerKeyDSIGN  (DSIGN TPraosStandardCrypto)
type SignKey = SignKeyDSIGN (DSIGN TPraosStandardCrypto)


data KeyError = ReadSigningKeyError  !TextViewFileError
              | ReadVerKeyError      !TextViewFileError
              | WriteSigningKeyError !TextViewFileError
              | WriteVerKeyError     !TextViewFileError
  deriving Show

renderKeyError :: KeyError -> Text
renderKeyError keyErr =
  case keyErr of
    ReadSigningKeyError err -> "signing key read error: " <> renderTextViewFileError err
    ReadVerKeyError err -> "verification key read error: " <> renderTextViewFileError err
    WriteSigningKeyError err -> "signing key write error: " <> renderTextViewFileError err
    WriteVerKeyError err -> "verification key write error: " <> renderTextViewFileError err


genKeyPair :: IO (VerKey, SignKey)
genKeyPair = do
  signKey <- runSecureRandom genKeyDSIGN
  let verKey = deriveVerKeyDSIGN signKey
  pure (verKey, signKey)


encodeSigningKey :: KeyType -> SignKey -> TextView
encodeSigningKey kt sKey =
    encodeToTextView fileType fileTitle CBOR.toCBOR sKey
  where
    fileType  = renderKeyType kt <> " signing key"
    fileTitle = renderKeyRole kt


decodeSigningKey :: KeyType -> TextView -> Either TextViewError SignKey
decodeSigningKey kt tView = do
    expectTextViewOfType fileType tView
    decodeFromTextView CBOR.fromCBOR tView
  where
    fileType  = renderKeyType kt <> " signing key"


encodeVerificationKey :: KeyType -> VerKey -> TextView
encodeVerificationKey kt vkey =
    encodeToTextView fileType fileTitle CBOR.toCBOR vkey
  where
    fileType  = renderKeyType kt <> " verification key"
    fileTitle = renderKeyRole kt


decodeVerificationKey :: KeyType -> TextView -> Either TextViewError VerKey
decodeVerificationKey kt tView = do
    expectTextViewOfType fileType tView
    decodeFromTextView CBOR.fromCBOR tView
  where
    fileType  = renderKeyType kt <> " verification key"


readSigningKey :: KeyType -> FilePath -> ExceptT KeyError IO SignKey
readSigningKey kt fp =
  firstExceptT ReadSigningKeyError $ newExceptT $
    readTextViewEncodedFile (decodeSigningKey kt) fp


writeSigningKey :: KeyType -> FilePath -> SignKey -> ExceptT KeyError IO ()
writeSigningKey kt fp sKey =
  firstExceptT WriteSigningKeyError $ newExceptT $
    writeTextViewEncodedFile (encodeSigningKey kt) fp sKey


readVerKey :: KeyType -> FilePath -> ExceptT KeyError IO VerKey
readVerKey kt fp = do
  firstExceptT ReadVerKeyError $ newExceptT $
    readTextViewEncodedFile (decodeVerificationKey kt) fp


writeVerKey :: KeyType -> FilePath -> VerKey -> ExceptT KeyError IO ()
writeVerKey kt fp vKey =
  firstExceptT WriteVerKeyError $ newExceptT $
    writeTextViewEncodedFile (encodeVerificationKey kt) fp vKey
