{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TypeApplications           #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Cardano.CLI.Run (
    CliError (..)
  , ClientCommand(..)
  , runCommand
  --
  , NewDirectory(..)
  , SigningKeyFile(..)
  , NewSigningKeyFile(..)
  , VerificationKeyFile(..)
  , NewVerificationKeyFile(..)
  , CertificateFile(..)
  , NewCertificateFile(..)
  , TxFile(..)
  , NewTxFile(..)
  ) where

import           Cardano.Prelude hiding (option, show, trace)

import           Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LB
import           Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as Builder
import qualified Formatting as F
import           System.Directory (doesPathExist)

import qualified Cardano.Chain.Common as Common
import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Chain.Slotting (EpochNumber(..))
import qualified Cardano.Chain.UTxO as UTxO
import           Cardano.Crypto (ProtocolMagicId)
import qualified Cardano.Crypto.Hashing as Crypto
import qualified Cardano.Crypto.Signing as Crypto
import qualified Test.Cardano.Chain.Genesis.Dummy as Dummy

import           Cardano.CLI.Delegation
import           Cardano.CLI.Genesis
import           Cardano.CLI.Key
import           Cardano.CLI.Ops
import           Cardano.CLI.Tx
import           Cardano.Config.CommonCLI
import           Cardano.Config.Presets (mainnetConfiguration)
import           Cardano.Common.Orphans ()
import           Cardano.Common.Protocol
import           Cardano.Node.Configuration.Topology


-- | Sub-commands of 'cardano-cli'.
data ClientCommand
  = Genesis
    NewDirectory
    GenesisParameters
  | PrettySigningKeyPublic
    SigningKeyFile
  | MigrateDelegateKeyFrom
    Protocol
    NewSigningKeyFile
    SigningKeyFile
  | DumpHardcodedGenesis
    NewDirectory
  | PrintGenesisHash
    GenesisFile
  | PrintSigningKeyAddress
    Common.NetworkMagic  -- TODO:  consider deprecation in favor of ProtocolMagicId,
                         --        once Byron is out of the picture.
    SigningKeyFile
  | Keygen
    NewSigningKeyFile
    PasswordRequirement
  | ToVerification
    SigningKeyFile
    NewVerificationKeyFile

    --- Delegation Related Commands ---

  | IssueDelegationCertificate
    ProtocolMagicId
    EpochNumber
    -- ^ The epoch from which the delegation is valid.
    SigningKeyFile
    -- ^ The issuer of the certificate, who delegates their right to sign blocks.
    VerificationKeyFile
    -- ^ The delegate, who gains the right to sign blocks on behalf of the issuer.
    NewCertificateFile
    -- ^ Filepath of the newly created delegation certificate.
  | CheckDelegation
    ProtocolMagicId
    CertificateFile
    VerificationKeyFile
    VerificationKeyFile

    -----------------------------------

  | SubmitTx
    TopologyInfo
    TxFile
    -- ^ Filepath of transaction to submit.
    CommonCLI
  | SpendGenesisUTxO
    NewTxFile
    -- ^ Filepath of the newly created transaction.
    SigningKeyFile
    -- ^ Signing key of genesis UTxO owner.
    Common.Address
    -- ^ Genesis UTxO address.
    (NonEmpty UTxO.TxOut)
    -- ^ Tx output.
    CommonCLI
  | SpendUTxO
    NewTxFile
    -- ^ Filepath of the newly created transaction.
    SigningKeyFile
    -- ^ Signing key of Tx underwriter.
    (NonEmpty UTxO.TxIn)
    -- ^ Inputs available for spending to the Tx underwriter's key.
    (NonEmpty UTxO.TxOut)
    -- ^ Genesis UTxO output Address.
    CommonCLI

runCommand :: CLIOps IO -> ClientCommand -> IO ()
runCommand co (Genesis outDir params) =
  uncurry (dumpGenesis co outDir)
    =<< mkGenesis params

runCommand co (DumpHardcodedGenesis dir) =
  dumpGenesis co dir (Genesis.configGenesisData Dummy.dummyConfig) Dummy.dummyGeneratedSecrets

runCommand co (PrettySigningKeyPublic skF) =
  putStrLn
    =<< T.unpack . prettyPublicKey . Crypto.toVerification
    <$> readSigningKey co skF

runCommand co (MigrateDelegateKeyFrom fromVer (NewSigningKeyFile newKey) oldKey) =
  ensureNewFileLBS newKey
    =<< coSerialiseDelegateKey co
    =<< flip readSigningKey oldKey =<< decideCLIOps fromVer

runCommand _ (PrintGenesisHash genesis) =
  putStrLn
    =<< F.format Crypto.hashHexF . Genesis.unGenesisHash . snd
    <$> readGenesis genesis

runCommand co (PrintSigningKeyAddress netMagic skF) =
  putStrLn
    =<< T.unpack . prettyAddress . Common.makeVerKeyAddress netMagic . Crypto.toVerification
    <$> readSigningKey co skF

runCommand co (Keygen (NewSigningKeyFile skF) passReq) =
  ensureNewFileLBS skF =<< coSerialiseDelegateKey co
    =<< keygen
    =<< getPassphrase ("Enter password to encrypt '" <> skF <> "': ") passReq

runCommand co (ToVerification skF (NewVerificationKeyFile vkF)) =
  ensureNewFile TL.writeFile vkF
    <$> Builder.toLazyText . Crypto.formatFullVerificationKey . Crypto.toVerification
    =<< readSigningKey co skF

runCommand co (IssueDelegationCertificate magic epoch issuerSK delegateVK cert) =
  ensureNewFileLBS (nFp cert)
    =<< coSerialiseDelegationCert co
    =<< issueByronGenesisDelegation magic epoch
        <$> readSigningKey co issuerSK
        <*> readVerificationKey delegateVK

runCommand _ (CheckDelegation magic cert issuerVF delegateVF) = do
  issuerVK <- readVerificationKey issuerVF
  delegateVK <- readVerificationKey delegateVF
  checkByronGenesisDelegation cert magic issuerVK delegateVK

runCommand co (SubmitTx topology fp common) = do
  cc <- mkConfiguration mainnetConfiguration common
  tx <- readByronTx fp
  nodeSubmitTx co topology cc tx

runCommand co (SpendGenesisUTxO (NewTxFile ctTx) ctKey genRichAddr outs common) = do
  cc <- mkConfiguration mainnetConfiguration common
  sk <- readSigningKey co ctKey
  ensureNewFileLBS ctTx
    =<< serialise <$> issueGenesisUTxOExpenditure co genRichAddr outs cc sk

runCommand co (SpendUTxO (NewTxFile ctTx) ctKey ins outs common) = do
  cc <- mkConfiguration mainnetConfiguration common
  sk <- readSigningKey co ctKey
  ensureNewFileLBS ctTx
    =<< serialise <$> issueUTxOExpenditure co ins outs cc sk

{-------------------------------------------------------------------------------
  Supporting functions
-------------------------------------------------------------------------------}

-- TODO:  we'd be better served by a combination of a temporary file
--        with an atomic rename.
-- | Checks if a path exists and throws and error if it does.
ensureNewFile :: (FilePath -> a -> IO ()) -> FilePath -> a -> IO ()
ensureNewFile writer outFile blob = do
  exists <- doesPathExist outFile
  when exists $
    throwIO $ OutputMustNotAlreadyExist outFile
  writer outFile blob

ensureNewFileLBS :: FilePath -> LB.ByteString -> IO ()
ensureNewFileLBS = ensureNewFile LB.writeFile
