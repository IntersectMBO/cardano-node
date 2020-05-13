{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.CLI.Shelley.KeyGen
  ( runColdKeyGen
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)

import qualified Shelley.Spec.Ledger.Keys as Ledger

import           Cardano.CLI.Shelley.Commands
import           Cardano.CLI.Errors (CliError(..))

import           Cardano.Config.Shelley.ColdKeys


runColdKeyGen :: KeyRole -> VerificationKeyFile -> SigningKeyFile
              -> ExceptT CliError IO ()
runColdKeyGen role (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) =
    firstExceptT KeyCliError $ do
      (vkey, skey) <- liftIO genKeyPair
      -- The Ledger.Genesis role type param here is actually arbitrary
      -- the representation is the same for them all.
      writeVerKey     role vkeyPath (vkey :: VerKey Ledger.Genesis)
      writeSigningKey role skeyPath skey

