{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.CLI.Key
  ( -- * Keys
    SigningKeyFile(..)
  , NewSigningKeyFile(..)
  , VerificationKeyFile(..)
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
import           Cardano.Crypto (SigningKey(..))
import qualified Cardano.Crypto.Random as Crypto
import qualified Cardano.Crypto.Signing as Crypto

import           Cardano.CLI.Ops


newtype SigningKeyFile =
  SigningKeyFile FilePath
  deriving (Eq, Ord, Show, IsString)

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
  sformat ("public key hash: "% build %"\n     public key: "%build)
    (Common.addressHash vk) (Crypto.formatFullVerificationKey vk)

-- TODO:  we need to support password-protected secrets.
-- | Read signing key from a file.  Throw an error if the file can't be read or
-- fails to deserialise.
readSigningKey :: CLIOps IO -> SigningKeyFile -> IO SigningKey
readSigningKey co (SigningKeyFile fp) =
  coDeserialiseDelegateKey co fp =<< LB.readFile fp

-- | Read verification key from a file.  Throw an error if the file can't be read
-- or the key fails to deserialise.
readVerificationKey :: VerificationKeyFile -> IO Crypto.VerificationKey
readVerificationKey (VerificationKeyFile fp) = do
  vkB <- SB.readFile fp
  case Crypto.parseFullVerificationKey . fromString $ UTF8.toString vkB of
    Left e -> throwIO . VerificationKeyDeserialisationFailed fp $ T.pack $ show e
    Right x -> pure x

-- | Generate a cryptographically random signing key,
--   protected with a (potentially empty) passphrase.
keygen :: Crypto.PassPhrase -> IO SigningKey
keygen passphrase =
  Crypto.runSecureRandom (Crypto.safeKeyGen passphrase)
  <&> SigningKey . Crypto.eskPayload . snd

-- | Get a passphrase from the standard input,
--   depending on whether it's required.
getPassphrase :: PasswordPrompt -> PasswordRequirement -> IO Crypto.PassPhrase
getPassphrase desc = \case
  GetPassword -> readPassword desc
  EmptyPassword -> pure Crypto.emptyPassphrase

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
