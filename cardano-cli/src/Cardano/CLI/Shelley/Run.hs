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

import qualified Data.ByteString.Char8 as BS

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)

import qualified Shelley.Spec.Ledger.Keys as Ledger
import qualified Cardano.Crypto.Hash.Class as Crypto

import           Cardano.CLI.Key (VerificationKeyFile(..))
import           Cardano.CLI.Ops (CliError (..))
import           Cardano.CLI.Shelley.Parsers

import           Cardano.Config.Shelley.ColdKeys
import           Cardano.Config.Shelley.KES
import           Cardano.Config.Shelley.OCert
import           Cardano.Config.Shelley.VRF
import           Cardano.Config.Types (SigningKeyFile(..))
import           Cardano.CLI.Shelley.Run.Genesis (runGenesisCreate)



--
-- CLI shelley command dispatch
--

runShelleyClientCommand :: ShelleyCommand -> ExceptT CliError IO ()
runShelleyClientCommand (AddressCmd      cmd) = runAddressCmd      cmd
runShelleyClientCommand (StakeAddressCmd cmd) = runStakeAddressCmd cmd
runShelleyClientCommand (TransactionCmd  cmd) = runTransactionCmd  cmd
runShelleyClientCommand (NodeCmd         cmd) = runNodeCmd         cmd
runShelleyClientCommand (PoolCmd         cmd) = runPoolCmd         cmd
runShelleyClientCommand (QueryCmd        cmd) = runQueryCmd        cmd
runShelleyClientCommand (BlockCmd        cmd) = runBlockCmd        cmd
runShelleyClientCommand (SystemCmd       cmd) = runSystemCmd       cmd
runShelleyClientCommand (DevOpsCmd       cmd) = runDevOpsCmd       cmd
runShelleyClientCommand (GenesisCmd      cmd) = runGenesisCmd      cmd

--
-- CLI shelley subcommand dispatch
--

runAddressCmd :: AddressCmd -> ExceptT CliError IO ()
runAddressCmd cmd = liftIO $ putStrLn $ "runAddressCmd: " ++ show cmd


runStakeAddressCmd :: StakeAddressCmd -> ExceptT CliError IO ()
runStakeAddressCmd cmd = liftIO $ putStrLn $ "runStakeAddressCmd: " ++ show cmd


runTransactionCmd :: TransactionCmd -> ExceptT CliError IO ()
runTransactionCmd cmd = liftIO $ putStrLn $ "runTransactionCmd: " ++ show cmd


runNodeCmd :: NodeCmd -> ExceptT CliError IO ()
runNodeCmd (NodeKeyGenCold vk sk ctr) = runNodeKeyGenCold vk sk ctr
runNodeCmd (NodeKeyGenKES  vk sk dur) = runNodeKeyGenKES  vk sk dur
runNodeCmd (NodeKeyGenVRF  vk sk)     = runNodeKeyGenVRF  vk sk
runNodeCmd (NodeIssueOpCert vk sk ctr p out) =
  runNodeIssueOpCert vk sk ctr p out


runPoolCmd :: PoolCmd -> ExceptT CliError IO ()
runPoolCmd cmd = liftIO $ putStrLn $ "runPoolCmd: " ++ show cmd


runQueryCmd :: QueryCmd -> ExceptT CliError IO ()
runQueryCmd cmd = liftIO $ putStrLn $ "runQueryCmd: " ++ show cmd


runBlockCmd :: BlockCmd -> ExceptT CliError IO ()
runBlockCmd cmd = liftIO $ putStrLn $ "runBlockCmd: " ++ show cmd


runSystemCmd:: SystemCmd -> ExceptT CliError IO ()
runSystemCmd cmd = liftIO $ putStrLn $ "runSystemCmd: " ++ show cmd


runDevOpsCmd :: DevOpsCmd -> ExceptT CliError IO ()
runDevOpsCmd cmd = liftIO $ putStrLn $ "runDevOpsCmd: " ++ show cmd


runGenesisCmd :: GenesisCmd -> ExceptT CliError IO ()
runGenesisCmd (GenesisKeyGenGenesis  vk sk)     = runGenesisKeyGenGenesis  vk sk
runGenesisCmd (GenesisKeyGenDelegate vk sk ctr) = runGenesisKeyGenDelegate vk sk ctr
runGenesisCmd (GenesisKeyGenUTxO     vk sk)     = runGenesisKeyGenUTxO     vk sk
runGenesisCmd (GenesisKeyHash        vk)        = runGenesisKeyHash        vk
runGenesisCmd (GenesisVerKey         vk sk)     = runGenesisVerKey         vk sk
runGenesisCmd (GenesisCreate         gd ms am)  = runGenesisCreate         gd ms am

--
-- Node command implementations
--

runNodeKeyGenCold :: VerificationKeyFile
                  -> SigningKeyFile
                  -> OpCertCounterFile
                  -> ExceptT CliError IO ()
runNodeKeyGenCold vkeyPath skeyPath (OpCertCounterFile ocertCtrPath) = do
    runColdKeyGen (OperatorKey StakePoolOperatorKey) vkeyPath skeyPath
    firstExceptT OperationalCertError $
      writeOperationalCertIssueCounter ocertCtrPath initialCounter
  where
    initialCounter = 0


runNodeKeyGenKES :: VerificationKeyFile -> SigningKeyFile -> Natural
                 -> ExceptT CliError IO ()
runNodeKeyGenKES (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) duration =
    firstExceptT KESCliError $ do
      (vkey, skey) <- liftIO $ genKESKeyPair duration
      writeKESVerKey     vkeyPath vkey
      writeKESSigningKey skeyPath skey


runNodeKeyGenVRF :: VerificationKeyFile -> SigningKeyFile
                 -> ExceptT CliError IO ()
runNodeKeyGenVRF (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) =
    firstExceptT VRFCliError $ do
      --FIXME: genVRFKeyPair genKESKeyPair results are in an inconsistent order
      (skey, vkey) <- liftIO genVRFKeyPair
      writeVRFVerKey     vkeyPath vkey
      writeVRFSigningKey skeyPath skey


runNodeIssueOpCert :: VerificationKeyFile
                   -> SigningKeyFile
                   -> OpCertCounterFile
                   -> KESPeriod
                   -> OutputFile
                   -> ExceptT CliError IO ()
runNodeIssueOpCert (VerificationKeyFile vkeyKESPath)
                   (SigningKeyFile skeyPath)
                   (OpCertCounterFile ocertCtrPath)
                   kesPeriod
                   (OutputFile certFile) = do
    issueNumber <- firstExceptT OperationalCertError $
      readOperationalCertIssueCounter ocertCtrPath

    verKeyKes <- firstExceptT KESCliError $
      readKESVerKey vkeyKESPath

    signKey <- firstExceptT KeyCliError $
      readSigningKey (OperatorKey StakePoolOperatorKey) skeyPath

    let cert = signOperationalCertificate
                 verKeyKes signKey
                 issueNumber kesPeriod
        vkey = deriveVerKey signKey

    firstExceptT OperationalCertError $ do
      -- Write the counter first, to reduce the chance of ending up with
      -- a new cert but without updating the counter.
      writeOperationalCertIssueCounter ocertCtrPath (succ issueNumber)
      writeOperationalCert certFile cert vkey


--
-- Genesis command implementations
--

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
      writeVerKey     role vkeyPath vkey
      writeSigningKey role skeyPath skey


runGenesisKeyHash :: VerificationKeyFile -> ExceptT CliError IO ()
runGenesisKeyHash (VerificationKeyFile vkeyPath) =
    firstExceptT KeyCliError $ do
      (vkey, _role) <- readVerKeySomeRole genesisKeyRoles vkeyPath
      let Ledger.KeyHash khash = Ledger.hashKey vkey
      liftIO $ BS.putStrLn $ Crypto.getHashBytesAsHex khash


runGenesisVerKey :: VerificationKeyFile -> SigningKeyFile
                 -> ExceptT CliError IO ()
runGenesisVerKey (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) =
    firstExceptT KeyCliError $ do
      (skey, role) <- readSigningKeySomeRole genesisKeyRoles skeyPath
      let vkey = deriveVerKey skey
      writeVerKey role vkeyPath vkey

genesisKeyRoles :: [KeyRole]
genesisKeyRoles = [ GenesisKey
                  , GenesisUTxOKey
                  , OperatorKey GenesisDelegateKey ]

