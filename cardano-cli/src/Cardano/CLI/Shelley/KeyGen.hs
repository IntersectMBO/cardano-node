{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.CLI.Shelley.KeyGen
  ( ShelleyKeyGenError
  , runColdKeyGen
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)

import qualified Shelley.Spec.Ledger.Keys as Ledger

import           Cardano.CLI.Shelley.Commands

import           Cardano.Config.Shelley.ColdKeys

data ShelleyKeyGenError = ShelleyColdKeyGenError !KeyError
                        deriving Show

runColdKeyGen :: KeyRole -> VerificationKeyFile -> SigningKeyFile
              -> ExceptT ShelleyKeyGenError IO ()
runColdKeyGen role (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) =
    firstExceptT ShelleyColdKeyGenError $ do
      (vkey, skey) <- liftIO genKeyPair
      -- The Ledger.Genesis role type param here is actually arbitrary
      -- the representation is the same for them all.
      writeVerKey     role vkeyPath (vkey :: VerKey Ledger.Genesis)
      writeSigningKey role skeyPath skey
