{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.CLI.Shelley.Run.KeyGen
  ( runGenesisKeyGenGenesis
  , runGenesisKeyGenDelegate
  , runGenesisKeyGenUTxO
  , runGenesisKeyHash
  , runGenesisVerKey
  , runColdKeyGen
  ) where

import           Cardano.Prelude hiding (option, trace)

import qualified Data.ByteString.Char8 as BS

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)

import qualified Shelley.Spec.Ledger.Keys as Ledger
import qualified Cardano.Crypto.Hash.Class as Crypto

import           Cardano.CLI.Key (VerificationKeyFile(..))
import           Cardano.CLI.Ops (CliError (..))
import           Cardano.CLI.Shelley.Parsers

import           Cardano.Config.Shelley.ColdKeys
import           Cardano.Config.Shelley.OCert
import           Cardano.Config.Types (SigningKeyFile(..))



runGenesisKeyGenGenesis :: VerificationKeyFile -> SigningKeyFile
                        -> ExceptT CliError IO ()
runGenesisKeyGenGenesis = runColdKeyGen GenesisKey


runGenesisKeyGenDelegate :: VerificationKeyFile
                         -> SigningKeyFile
                         -> OpCertCounterFile
                         -> ExceptT CliError IO ()
runGenesisKeyGenDelegate vkeyPath skeyPath (OpCertCounterFile ocertCtrPath) = do
    runColdKeyGen (OperatorKey GenesisDelegateKey) vkeyPath skeyPath
    firstExceptT OperationalCertError $
      writeOperationalCertIssueCounter ocertCtrPath initialCounter
  where
    initialCounter = 0


runGenesisKeyGenUTxO :: VerificationKeyFile -> SigningKeyFile
                     -> ExceptT CliError IO ()
runGenesisKeyGenUTxO = runColdKeyGen GenesisUTxOKey


runColdKeyGen :: KeyRole -> VerificationKeyFile -> SigningKeyFile
              -> ExceptT CliError IO ()
runColdKeyGen role (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) =
    firstExceptT KeyCliError $ do
      (vkey, skey) <- liftIO genKeyPair
      -- The Ledger.Genesis role type param here is actually arbitrary
      -- the representation is the same for them all.
      writeVerKey     role vkeyPath (vkey :: VerKey Ledger.Genesis)
      writeSigningKey role skeyPath skey


runGenesisKeyHash :: VerificationKeyFile -> ExceptT CliError IO ()
runGenesisKeyHash (VerificationKeyFile vkeyPath) =
    firstExceptT KeyCliError $ do
      (vkey, _role) <- readVerKeySomeRole genesisKeyRoles vkeyPath
      let Ledger.KeyHash khash = Ledger.hashKey (vkey :: VerKey Ledger.Genesis)
      liftIO $ BS.putStrLn $ Crypto.getHashBytesAsHex khash


runGenesisVerKey :: VerificationKeyFile -> SigningKeyFile
                 -> ExceptT CliError IO ()
runGenesisVerKey (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) =
    firstExceptT KeyCliError $ do
      (skey, role) <- readSigningKeySomeRole genesisKeyRoles skeyPath
      let vkey :: VerKey Ledger.Genesis
          vkey = deriveVerKey skey
      writeVerKey role vkeyPath vkey

genesisKeyRoles :: [KeyRole]
genesisKeyRoles =
  [ GenesisKey
  , GenesisUTxOKey
  , OperatorKey GenesisDelegateKey
  ]

