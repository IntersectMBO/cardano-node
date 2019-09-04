{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.CLI.Key
  ( SigningKeyFile(..)
  , NewSigningKeyFile(..)
  , VerificationKeyFile(..)
  , NewVerificationKeyFile(..)
  , prettySigningKeyPub
  , readSigningKey
  , readVerificationKey
  )
where

import           Prelude (show)
import           Cardano.Prelude hiding (option, show, trace)

import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.UTF8 as UTF8
import           Data.String (IsString, fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as Builder
import qualified Formatting as F

import qualified Cardano.Chain.Common as CC.Common
import           Cardano.Crypto (SigningKey(..))
import qualified Cardano.Crypto.Hashing as Crypto
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


prettySigningKeyPub :: SigningKey -> Text
prettySigningKeyPub (Crypto.toVerification -> vk) = TL.toStrict
  $  "public key hash: " <> (F.format Crypto.hashHexF . CC.Common.addressHash $ vk) <> "\n"
  <> "     public key: " <> (Builder.toLazyText . Crypto.formatFullVerificationKey $ vk)

-- TODO:  we need to support password-protected secrets.
readSigningKey :: CLIOps IO -> SigningKeyFile -> IO SigningKey
readSigningKey co (SigningKeyFile fp) =
  coDeserialiseDelegateKey co fp =<< LB.readFile fp

readVerificationKey :: VerificationKeyFile -> IO Crypto.VerificationKey
readVerificationKey (VerificationKeyFile fp) = do
  vkB <- SB.readFile fp
  case Crypto.parseFullVerificationKey . fromString $ UTF8.toString vkB of
    Left e -> throwIO . VerificationKeyDeserialisationFailed fp $ T.pack $ show e
    Right x -> pure x
