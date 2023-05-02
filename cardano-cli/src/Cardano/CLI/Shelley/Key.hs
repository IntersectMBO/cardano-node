{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Shelley CLI option data types and functions for cryptographic keys.
module Cardano.CLI.Shelley.Key
  ( VerificationKeyOrFile (..)
  , readVerificationKeyOrFile
  , readVerificationKeyOrTextEnvFile

  , VerificationKeyTextOrFile (..)
  , VerificationKeyTextOrFileError (..)
  , readVerificationKeyTextOrFileAnyOf
  , renderVerificationKeyTextOrFileError

  , VerificationKeyOrHashOrFile (..)
  , readVerificationKeyOrHashOrFile
  , readVerificationKeyOrHashOrTextEnvFile

  , PaymentVerifier(..)
  , StakeIdentifier(..)
  , StakeVerifier(..)

  , generateKeyPair

  , DelegationTarget(..)
  ) where

import           Cardano.Api
import           Cardano.Api.Pretty

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Bifunctor (Bifunctor (..))
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text

import           Cardano.Api.Shelley (StakePoolKey)
import           Cardano.CLI.Types


------------------------------------------------------------------------------
-- Verification key deserialisation
------------------------------------------------------------------------------

-- | Either a verification key or path to a verification key file.
data VerificationKeyOrFile keyrole
  = VerificationKeyValue !(VerificationKey keyrole)
  -- ^ A verification key.
  | VerificationKeyFilePath !(VerificationKeyFile In)
  -- ^ A path to a verification key file.
  -- Note that this file hasn't been validated at all (whether it exists,
  -- contains a key of the correct type, etc.)

deriving instance Show (VerificationKey keyrole)
  => Show (VerificationKeyOrFile keyrole)

deriving instance Eq (VerificationKey keyrole)
  => Eq (VerificationKeyOrFile keyrole)

-- | Read a verification key or verification key file and return a
-- verification key.
--
-- If a filepath is provided, the file can either be formatted as Bech32, hex,
-- or text envelope.
readVerificationKeyOrFile
  :: ( HasTextEnvelope (VerificationKey keyrole)
     , SerialiseAsBech32 (VerificationKey keyrole)
     )
  => AsType keyrole
  -> VerificationKeyOrFile keyrole
  -> IO (Either (FileError InputDecodeError) (VerificationKey keyrole))
readVerificationKeyOrFile asType verKeyOrFile =
  case verKeyOrFile of
    VerificationKeyValue vk -> pure (Right vk)
    VerificationKeyFilePath (File fp) ->
      readKeyFile
        (AsVerificationKey asType)
        (NE.fromList [InputFormatBech32, InputFormatHex, InputFormatTextEnvelope])
        fp

-- | Read a verification key or verification key file and return a
-- verification key.
--
-- If a filepath is provided, it will be interpreted as a text envelope
-- formatted file.
readVerificationKeyOrTextEnvFile
  :: HasTextEnvelope (VerificationKey keyrole)
  => AsType keyrole
  -> VerificationKeyOrFile keyrole
  -> IO (Either (FileError InputDecodeError) (VerificationKey keyrole))
readVerificationKeyOrTextEnvFile asType verKeyOrFile =
  case verKeyOrFile of
    VerificationKeyValue vk -> pure (Right vk)
    VerificationKeyFilePath fp -> readKeyFileTextEnvelope (AsVerificationKey asType) fp

data PaymentVerifier
  = PaymentVerifierKey VerificationKeyTextOrFile
  | PaymentVerifierScriptFile ScriptFile
  deriving (Eq, Show)

data StakeVerifier
  = StakeVerifierKey (VerificationKeyOrFile StakeKey)
  | StakeVerifierScriptFile ScriptFile
  deriving (Eq, Show)

data StakeIdentifier
  = StakeIdentifierVerifier StakeVerifier
  | StakeIdentifierAddress StakeAddress
  deriving (Eq, Show)

-- | A resource that identifies the delegation target.  At the moment a delegation
-- target can only be a stake pool.
newtype DelegationTarget
  = StakePoolDelegationTarget (VerificationKeyOrHashOrFile StakePoolKey)
  deriving Show

-- | Either an unvalidated text representation of a verification key or a path
-- to a verification key file.
data VerificationKeyTextOrFile
  = VktofVerificationKeyText !Text
  | VktofVerificationKeyFile !(VerificationKeyFile In)
  deriving (Eq, Show)

-- | An error in deserialising a 'VerificationKeyTextOrFile' to a
-- 'VerificationKey'.
data VerificationKeyTextOrFileError
  = VerificationKeyTextError !InputDecodeError
  | VerificationKeyFileError !(FileError InputDecodeError)
  deriving Show

-- | Render an error message for a 'VerificationKeyTextOrFileError'.
renderVerificationKeyTextOrFileError :: VerificationKeyTextOrFileError -> Doc Ann
renderVerificationKeyTextOrFileError vkTextOrFileErr =
  case vkTextOrFileErr of
    VerificationKeyTextError err -> renderInputDecodeError err
    VerificationKeyFileError err -> displayError err

-- | Deserialise a verification key from text or a verification key file.
-- If a filepath is provided, the file can either be formatted as Bech32, hex,
-- or text envelope.
readVerificationKeyTextOrFileAnyOf
  :: VerificationKeyTextOrFile
  -> IO (Either VerificationKeyTextOrFileError SomeAddressVerificationKey)
readVerificationKeyTextOrFileAnyOf verKeyTextOrFile =
  case verKeyTextOrFile of
    VktofVerificationKeyText vkText ->
      pure $ first VerificationKeyTextError $
        deserialiseAnyVerificationKey (Text.encodeUtf8 vkText)
    VktofVerificationKeyFile (File fp) -> do
      vkBs <- liftIO $ BS.readFile fp
      pure $ first VerificationKeyTextError $
        deserialiseAnyVerificationKey vkBs


-- | Verification key, verification key hash, or path to a verification key
-- file.
data VerificationKeyOrHashOrFile keyrole
  = VerificationKeyOrFile !(VerificationKeyOrFile keyrole)
  -- ^ Either a verification key or path to a verification key file.
  | VerificationKeyHash !(Hash keyrole)
  -- ^ A verification key hash.

deriving instance (Show (VerificationKeyOrFile keyrole), Show (Hash keyrole))
  => Show (VerificationKeyOrHashOrFile keyrole)

deriving instance (Eq (VerificationKeyOrFile keyrole), Eq (Hash keyrole))
  => Eq (VerificationKeyOrHashOrFile keyrole)

-- | Read a verification key or verification key hash or verification key file
-- and return a verification key hash.
--
-- If a filepath is provided, the file can either be formatted as Bech32, hex,
-- or text envelope.
readVerificationKeyOrHashOrFile
  :: (Key keyrole, SerialiseAsBech32 (VerificationKey keyrole))
  => AsType keyrole
  -> VerificationKeyOrHashOrFile keyrole
  -> IO (Either (FileError InputDecodeError) (Hash keyrole))
readVerificationKeyOrHashOrFile asType verKeyOrHashOrFile =
  case verKeyOrHashOrFile of
    VerificationKeyOrFile vkOrFile -> do
      eitherVk <- readVerificationKeyOrFile asType vkOrFile
      pure (verificationKeyHash <$> eitherVk)
    VerificationKeyHash vkHash -> pure (Right vkHash)

-- | Read a verification key or verification key hash or verification key file
-- and return a verification key hash.
--
-- If a filepath is provided, it will be interpreted as a text envelope
-- formatted file.
readVerificationKeyOrHashOrTextEnvFile
  :: Key keyrole
  => AsType keyrole
  -> VerificationKeyOrHashOrFile keyrole
  -> IO (Either (FileError InputDecodeError) (Hash keyrole))
readVerificationKeyOrHashOrTextEnvFile asType verKeyOrHashOrFile =
  case verKeyOrHashOrFile of
    VerificationKeyOrFile vkOrFile -> do
      eitherVk <- readVerificationKeyOrTextEnvFile asType vkOrFile
      pure (verificationKeyHash <$> eitherVk)
    VerificationKeyHash vkHash -> pure (Right vkHash)

generateKeyPair :: Key keyrole => AsType keyrole -> IO (VerificationKey keyrole, SigningKey keyrole)
generateKeyPair asType = do
  skey <- generateSigningKey asType
  return (getVerificationKey skey, skey)
