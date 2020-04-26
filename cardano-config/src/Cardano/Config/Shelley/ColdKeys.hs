{-# LANGUAGE OverloadedStrings #-}

module Cardano.Config.Shelley.ColdKeys
  ( KeyError(..)
  , KeyRole(..)
  , OperatorKeyRole(..)
  , decodeVerificationKey
  , decodeVerificationKeySomeRole
  , encodeVerificationKey
  , encodeSigningKey
  , decodeSigningKey
  , decodeSigningKeySomeRole
  , genKeyPair
  , deriveVerKey
  , readSigningKey
  , readSigningKeySomeRole
  , readVerKey
  , readVerKeySomeRole
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
                   (TPraosStandardCrypto)
import qualified Shelley.Spec.Ledger.Keys as Ledger

import           Cardano.Config.TextView


data KeyRole = GenesisKey
             | GenesisUTxOKey
             | OperatorKey OperatorKeyRole

data OperatorKeyRole = GenesisDelegateKey | StakePoolOperatorKey


-- Local aliases for shorter types:
type VerKey  = Ledger.VKey TPraosStandardCrypto
type SignKey = Ledger.SKey TPraosStandardCrypto


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
  pure (Ledger.DiscVKey verKey, Ledger.SKey signKey)


deriveVerKey :: SignKey -> VerKey
deriveVerKey (Ledger.SKey skey) =
    Ledger.VKey (deriveVerKeyDSIGN skey)


encodeSigningKey :: KeyRole -> SignKey -> TextView
encodeSigningKey role (Ledger.SKey sKey) =
    encodeToTextView fileType fileTitle CBOR.toCBOR sKey
  where
    fileType  = renderKeyType (KeyTypeSigning role)
    fileTitle = renderKeyDescr role


decodeSigningKey :: KeyRole -> TextView -> Either TextViewError SignKey
decodeSigningKey role tView = do
    expectTextViewOfType fileType tView
    decodeFromTextView (Ledger.SKey <$> CBOR.fromCBOR) tView
  where
    fileType  = renderKeyType (KeyTypeSigning role)


decodeSigningKeySomeRole :: [KeyRole]
                         -> TextView
                         -> Either TextViewError (SignKey, KeyRole)
decodeSigningKeySomeRole roles tView = do
    role <- expectTextViewOfTypes fileTypes tView
    skey <- decodeFromTextView (Ledger.SKey <$> CBOR.fromCBOR) tView
    return (skey, role)
  where
    fileTypes = [ (renderKeyType (KeyTypeSigning role), role) | role <- roles ]


encodeVerificationKey :: KeyRole -> VerKey -> TextView
encodeVerificationKey role vkey =
    encodeToTextView fileType fileTitle CBOR.toCBOR vkey
  where
    fileType  = renderKeyType (KeyTypeVerification role)
    fileTitle = renderKeyDescr role


decodeVerificationKey :: KeyRole -> TextView -> Either TextViewError VerKey
decodeVerificationKey role tView = do
    expectTextViewOfType fileType tView
    decodeFromTextView CBOR.fromCBOR tView
  where
    fileType  = renderKeyType (KeyTypeVerification role)


decodeVerificationKeySomeRole :: [KeyRole]
                              -> TextView
                              -> Either TextViewError (VerKey, KeyRole)
decodeVerificationKeySomeRole roles tView = do
    role <- expectTextViewOfTypes fileTypes tView
    vkey <- decodeFromTextView CBOR.fromCBOR tView
    return (vkey, role)
  where
    fileTypes = [ (renderKeyType (KeyTypeVerification role), role)
                | role <- roles ]


readSigningKey :: KeyRole -> FilePath -> ExceptT KeyError IO SignKey
readSigningKey role fp =
  firstExceptT ReadSigningKeyError $ newExceptT $
    readTextViewEncodedFile (decodeSigningKey role) fp


readSigningKeySomeRole :: [KeyRole]
                       -> FilePath
                       -> ExceptT KeyError IO (SignKey, KeyRole)
readSigningKeySomeRole roles fp =
  firstExceptT ReadSigningKeyError $ newExceptT $
    readTextViewEncodedFile (decodeSigningKeySomeRole roles) fp


writeSigningKey :: KeyRole -> FilePath -> SignKey -> ExceptT KeyError IO ()
writeSigningKey role fp sKey =
  firstExceptT WriteSigningKeyError $ newExceptT $
    writeTextViewEncodedFile (encodeSigningKey role) fp sKey


readVerKey :: KeyRole -> FilePath -> ExceptT KeyError IO VerKey
readVerKey role fp = do
  firstExceptT ReadVerKeyError $ newExceptT $
    readTextViewEncodedFile (decodeVerificationKey role) fp


readVerKeySomeRole :: [KeyRole]
                   -> FilePath
                   -> ExceptT KeyError IO (VerKey, KeyRole)
readVerKeySomeRole roles fp = do
  firstExceptT ReadVerKeyError $ newExceptT $
    readTextViewEncodedFile (decodeVerificationKeySomeRole roles) fp


writeVerKey :: KeyRole -> FilePath -> VerKey -> ExceptT KeyError IO ()
writeVerKey role fp vKey =
  firstExceptT WriteVerKeyError $ newExceptT $
    writeTextViewEncodedFile (encodeVerificationKey role) fp vKey


--
-- Key file type strings
--

data KeyType = KeyTypeVerification KeyRole | KeyTypeSigning KeyRole

renderKeyType :: KeyType -> TextViewType
renderKeyType (KeyTypeVerification kr) = renderKeyRole kr <> " verification key"
renderKeyType (KeyTypeSigning      kr) = renderKeyRole kr <> " signing key"

renderKeyRole :: KeyRole -> TextViewType
renderKeyRole GenesisKey     = "Genesis"
renderKeyRole GenesisUTxOKey = "Genesis UTxO"
renderKeyRole OperatorKey{}  = "Node operator"

renderKeyDescr :: KeyRole -> TextViewTitle
renderKeyDescr GenesisKey                         = "Genesis key"
renderKeyDescr GenesisUTxOKey                     = "Genesis initial UTxO key"
renderKeyDescr (OperatorKey GenesisDelegateKey)   = "Genesis delegate operator key"
renderKeyDescr (OperatorKey StakePoolOperatorKey) = "Stake pool operator key"

