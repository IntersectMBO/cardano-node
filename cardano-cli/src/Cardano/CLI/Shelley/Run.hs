{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.CLI.Shelley.Run
  ( runShelleyClientCommand
  ) where

import           Cardano.Prelude hiding (option, trace)

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)

import           Cardano.Api (ShelleyKeyDiscriminator (..), shelleyGenKeyPair, writeKeyPair)

import           Cardano.CLI.Key (VerificationKeyFile(..))
import           Cardano.CLI.Ops (CliError (..))
import           Cardano.CLI.Shelley.Parsers (OutputFile (..), ShelleyCommand(..))
import           Cardano.Config.Shelley.KES
                   (genKESKeyPair, writeKESSigningKey, writeKESVerKey)
import           Cardano.Config.Shelley.VRF
                   (genVRFKeyPair, writeVRFSigningKey, writeVRFVerKey)
import           Cardano.Config.Types (SigningKeyFile(..))

runShelleyClientCommand :: ShelleyCommand -> ExceptT CliError IO ()
runShelleyClientCommand cc =
  case cc of
    ShelleyKeyGenerate fpath -> runShelleyKeyGenerate fpath
    ShelleyKESKeyPairGenerate vKeyPath sKeyPath duration -> runShelleyKESKeyPairGeneration vKeyPath sKeyPath duration
    ShelleyVRFKeyPairGenerate vKeyPath sKeyPath -> runShelleyVRFKeyPairGeneration vKeyPath sKeyPath
    _ -> liftIO . putStrLn $ "runShelleyClientCommand: " ++ show cc


runShelleyKeyGenerate :: OutputFile -> ExceptT CliError IO ()
runShelleyKeyGenerate (OutputFile fpath) = do
  kp <- liftIO $ shelleyGenKeyPair RegularShelleyKey
  firstExceptT CardanoApiError $ newExceptT (writeKeyPair fpath kp)

runShelleyKESKeyPairGeneration :: VerificationKeyFile -> SigningKeyFile -> Natural -> ExceptT CliError IO ()
runShelleyKESKeyPairGeneration (VerificationKeyFile vKeyPath) (SigningKeyFile sKeyPath) duration = do
  (vKESKey, sKESKey) <- liftIO $ genKESKeyPair duration
  firstExceptT KESCliError $ writeKESSigningKey sKeyPath sKESKey
  firstExceptT KESCliError $ writeKESVerKey vKeyPath vKESKey

runShelleyVRFKeyPairGeneration :: VerificationKeyFile -> SigningKeyFile  -> ExceptT CliError IO ()
runShelleyVRFKeyPairGeneration (VerificationKeyFile vKeyPath) (SigningKeyFile sKeyPath) = do
  (sKESKey, vKESKey) <- liftIO $ genVRFKeyPair
  firstExceptT VRFCliError $ writeVRFSigningKey sKeyPath sKESKey
  firstExceptT VRFCliError $ writeVRFVerKey vKeyPath vKESKey
