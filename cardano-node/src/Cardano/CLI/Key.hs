{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}



module Cardano.CLI.Key
  ( -- * Keys
    VerificationKeyFile(..)
  , NewSigningKeyFile(..)
  , NewVerificationKeyFile(..)
  , prettyPublicKey
  , readSigningKey
  , readVerificationKey
  , keygen
    -- * Passwords
  , PasswordRequirement(..)
  , PasswordPrompt
  , getPassphrase
  )
where

import           Prelude (String, show)
import           Cardano.Prelude hiding (option, show, trace, (%))

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra
                   (firstExceptT, handleIOExceptT, hoistEither)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.UTF8 as UTF8
import           Data.String (IsString, fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Formatting (build, (%), sformat)

import           System.IO (hSetEcho, hFlush, stdout, stdin)

import qualified Cardano.Chain.Common as Common
import           Cardano.Config.Protocol
import           Cardano.Config.Types (SigningKeyFile(..))
import           Cardano.Crypto (SigningKey(..))
import qualified Cardano.Crypto.Random as Crypto
import qualified Cardano.Crypto.Signing as Crypto

import           Cardano.CLI.Ops


newtype NewSigningKeyFile =
  NewSigningKeyFile FilePath
  deriving (Eq, Ord, Show, IsString)

newtype VerificationKeyFile =
  VerificationKeyFile FilePath
  deriving (Eq, Ord, Show, IsString)

newtype NewVerificationKeyFile =
  NewVerificationKeyFile FilePath
   deriving (Eq, Ord, Show, IsString)

-- | Whether to require a password, or to supply an empty one.
data PasswordRequirement
  =  GetPassword
  |  EmptyPassword
  deriving (Eq, Show)

type PasswordPrompt = String

-- | Print some invariant properties of a public key:
--   its hash and formatted view.
prettyPublicKey :: Crypto.VerificationKey -> Text
prettyPublicKey vk =
  sformat (  "    public key hash: "% build %
           "\npublic key (base64): "% Crypto.fullVerificationKeyF %
           "\n   public key (hex): "% Crypto.fullVerificationKeyHexF)
    (Common.addressHash vk) vk vk

-- TODO:  we need to support password-protected secrets.
-- | Read signing key from a file.  Throw an error if the file can't be read or
-- fails to deserialise.
readSigningKey :: Protocol -> SigningKeyFile -> ExceptT CliError IO SigningKey
readSigningKey ptcl (SigningKeyFile fp) = do
  sK <- handleIOExceptT (ReadSigningKeyFailure fp . T.pack . displayException) $ LB.readFile fp

  -- Signing Key
  hoistEither $ deserialiseDelegateKey ptcl fp sK

-- | Read verification key from a file.  Throw an error if the file can't be read
-- or the key fails to deserialise.
readVerificationKey :: VerificationKeyFile -> ExceptT CliError IO Crypto.VerificationKey
readVerificationKey (VerificationKeyFile fp) = do
  vkB <- handleIOExceptT (ReadVerificationKeyFailure fp . T.pack . displayException) (SB.readFile fp)
  -- Verification Key
  let eVk = hoistEither . Crypto.parseFullVerificationKey . fromString $ UTF8.toString vkB
  -- Convert error to 'CliError'
  firstExceptT (VerificationKeyDeserialisationFailed fp . T.pack . show) $ eVk

-- | Generate a cryptographically random signing key,
--   protected with a (potentially empty) passphrase.
keygen :: Crypto.PassPhrase -> IO SigningKey
keygen passphrase =
  snd <$> Crypto.runSecureRandom (Crypto.safeKeyGen passphrase)

-- | Get a passphrase from the standard input,
--   depending on whether it's required.
getPassphrase :: PasswordPrompt -> PasswordRequirement -> IO Crypto.PassPhrase
getPassphrase desc = \case
  GetPassword -> readPassword desc
  EmptyPassword -> pure Crypto.emptyPassphrase

-- | Obtain a 'Crypto.PassPhrase' from the standard input.
--   Terminal echoing is disabled.
readPassword :: String -> IO Crypto.PassPhrase
readPassword prompt =
  Crypto.PassPhrase . BA.convert <$> loop

  where
    loop :: IO ByteString
    loop = do
      (v1, v2) <- (,) <$> readOne prompt <*> readOne "Repeat to validate: "
      if v1 == v2
        then pure v1
        else hPutStrLn stdout ("Sorry, entered passwords don't match." :: String)
             >> loop

    readOne :: String -> IO ByteString
    readOne pr = do
      hPutStr stdout pr >> hFlush stdout
      hSetEcho stdout False
      pp <- SB.hGetLine stdin
      hSetEcho stdout True
      hPutStrLn stdout ("" :: String)
      pure pp
