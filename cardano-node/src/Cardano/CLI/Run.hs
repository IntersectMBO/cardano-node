{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

module Cardano.CLI.Run (
    CliError (..)
  , ClientCommand(..)
  , runCommand
  --
  , GenesisFile(..)
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

import           Prelude (show)
import           Cardano.Prelude hiding (option, show, trace)
import           Test.Cardano.Prelude (canonicalDecodePretty)

import           Codec.Serialise (serialise, deserialiseOrFail)
import           Control.Tracer
import           Data.Bits (shiftL)
import qualified Data.ByteString.Lazy as LB
import           Data.Semigroup ((<>))
import qualified Data.Text.Lazy.Builder as Builder
import qualified Formatting as F
import           Data.Time (UTCTime)

import qualified Cardano.Chain.Common as CC.Common
import           Cardano.Chain.Common
import           Cardano.Chain.Delegation hiding (Map, epoch)
import           Cardano.Chain.Genesis
import           Cardano.Chain.Slotting (EpochNumber(..))
import           Cardano.Chain.UTxO
import           Cardano.Crypto (SigningKey (..), ProtocolMagic, ProtocolMagicId)
import qualified Cardano.Crypto.Hashing as Crypto
import qualified Cardano.Crypto.Random as Crypto
import qualified Cardano.Crypto.Signing as Crypto
import           Cardano.Node.Configuration.Presets (mainnetConfiguration)
import           Ouroboros.Consensus.Protocol hiding (Protocol)
import           Ouroboros.Consensus.Ledger.Byron
import qualified Test.Cardano.Chain.Genesis.Dummy as Dummy

import           Cardano.CLI.Helpers
import           Cardano.CLI.Ops
import           Cardano.Config.CommonCLI
import           Cardano.Common.Protocol
import           Cardano.Node.Orphans ()
import           Cardano.Node.Configuration.Topology
import           Cardano.Node.TxSubmission


data ClientCommand
  = Genesis
    NewDirectory
    UTCTime
    FilePath              -- This one is going to be replaced with elementwise CLI args soon,
    BlockCount            --   so no big gain in newtyping it.
    ProtocolMagic
    TestnetBalanceOptions
    FakeAvvmOptions
    LovelacePortion
    (Maybe Integer)
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
    NetworkMagic         -- TODO:  consider deprecation in favor of ProtocolMagicId,
                         --        once Byron is out of the picture.
    SigningKeyFile
  | Keygen
    NewSigningKeyFile
    Bool
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
    Address
    -- ^ Genesis UTxO address.
    (NonEmpty TxOut)
    -- ^ Tx output.
    CommonCLI
  | SpendUTxO
    NewTxFile
    -- ^ Filepath of the newly created transaction.
    SigningKeyFile
    -- ^ Signing key of Tx underwriter.
    (NonEmpty TxIn)
    -- ^ Inputs available for spending to the Tx underwriter's key.
    (NonEmpty TxOut)
    -- ^ Genesis UTxO output Address.
    CommonCLI

runCommand :: CLIOps IO -> ClientCommand -> IO ()
runCommand co@CLIOps{..}
         (Genesis
           (NewDirectory outDir)
           startTime
           pParamsFile
           blockCount
           protocolMagic
           giTestBalance
           giFakeAvvmBalance
           giAvvmBalanceFactor
           giSeed) = do
  pParamsRaw <- LB.readFile pParamsFile

  pParams <- eitherThrow
               (ProtocolParametersParseFailed pParamsFile)
               (canonicalDecodePretty pParamsRaw)

  -- We're relying on the generator to fake AVVM and delegation.
  mGenesisDlg <- runExceptT $ mkGenesisDelegation []
  genesisDelegation <- eitherThrow DelegationError mGenesisDlg

  seed <- case giSeed of
    Nothing -> Crypto.runSecureRandom . Crypto.randomNumber $ shiftL 1 32
    Just x  -> pure x

  let genesisAvvmBalances = GenesisAvvmBalances mempty
  let mGenesisSpec =
        mkGenesisSpec
        genesisAvvmBalances -- :: !GenesisAvvmBalances
        genesisDelegation   -- :: !GenesisDelegation
        pParams             -- :: !ProtocolParameters
        blockCount          -- :: !BlockCount
        protocolMagic       -- :: !ProtocolMagic
        genesisInitializer  -- :: !GenesisInitializer
      genesisInitializer =
        GenesisInitializer
        giTestBalance       -- :: !TestnetBalanceOptions
        giFakeAvvmBalance   -- :: !FakeAvvmOptions
        giAvvmBalanceFactor -- :: !LovelacePortion
        giUseHeavyDlg       -- :: !Bool
        seed                -- :: !Integer
      giUseHeavyDlg =
        True                -- Not using delegate keys unsupported.

  genesisSpec <- eitherThrow GenesisSpecError mGenesisSpec

  mGData <- runExceptT $ generateGenesisData startTime genesisSpec

  (genesisData, generatedSecrets) <- eitherThrow GenesisGenerationError mGData

  dumpGenesis co outDir genesisData generatedSecrets

runCommand co@CLIOps{..} (PrettySigningKeyPublic skF) = do
  sKey <- readSigningKey co skF
  putTextLn $ prettySigningKeyPub sKey

runCommand co (MigrateDelegateKeyFrom protocol newKeyFP oldKey) = do
  -- Protocol specific operations
  operations <- decideCLIOps protocol
  sKey <- readSigningKey operations oldKey
  newKey <- coSerialiseDelegateKey co sKey
  LB.writeFile (nSkFp newKeyFP) newKey

runCommand co (DumpHardcodedGenesis (NewDirectory dir)) =
  dumpGenesis co dir
              (configGenesisData Dummy.dummyConfig)
              Dummy.dummyGeneratedSecrets

runCommand CLIOps{..} (PrintGenesisHash (GenesisFile genesis)) = do
  gdE <- runExceptT (readGenesisData genesis)
  case gdE of
    Left e  -> throwIO $ GenesisReadError genesis e
    Right x -> putStrLn . F.format Crypto.hashHexF
               . unGenesisHash
               $ snd x

runCommand co@CLIOps{..} (PrintSigningKeyAddress netMagic skF) =
  putTextLn . prettyAddress
            . CC.Common.makeVerKeyAddress netMagic
            . Crypto.toVerification
            =<< readSigningKey co skF

runCommand CLIOps{ coSerialiseDelegateKey } (Keygen skF disablePassword) = do
  passph <- if disablePassword
            then pure Crypto.emptyPassphrase
            else readPassword $
                 "Enter password to encrypt '" <> (nSkFp skF) <> "': "

  (_vk, esk) <- Crypto.runSecureRandom $ Crypto.safeKeyGen passph

  ensureNewFileLBS (nSkFp skF)
    =<< (coSerialiseDelegateKey $ SigningKey $ Crypto.eskPayload esk)

runCommand co (ToVerification skF (NewVerificationKeyFile vkF)) = do
  sKey <- readSigningKey co skF
  ensureNewFileText vkF
    . Builder.toLazyText
    . Crypto.formatFullVerificationKey
    $ Crypto.toVerification sKey

runCommand co (IssueDelegationCertificate pM epoch skF vkF cFp) = do
  sk <- readSigningKey co skF
  vk <- readVerificationKey vkF
  let signer = Crypto.noPassSafeSigner sk
  -- TODO:  we need to support password-protected secrets.
  let cert = signCertificate pM vk epoch signer
  ensureNewFileLBS (nFp cFp) =<< (coSerialiseDelegationCert co $ cert)

runCommand CLIOps{..} (CheckDelegation magic certFp issuerVF delegateVF) = do
  issuerVK'   <- readVerificationKey issuerVF
  delegateVK' <- readVerificationKey delegateVF
  certBS      <- LB.readFile (ceFp certFp)
  cert :: Certificate <- eitherThrow
                           (DlgCertificateDeserialisationFailed (ceFp certFp))
                           (canonicalDecodePretty certBS)
  certificateValidation magic cert delegateVK' issuerVK' certFp

runCommand co (SubmitTx stTopology (TxFile stTx) stCommon) = do
  withRealPBFT co mainnetConfiguration stCommon $
    \cc p@(ProtocolRealPBFT _ _ _ _ _) -> do
      txBS <- LB.readFile stTx
      case deserialiseOrFail txBS of
        Left  e  -> throwIO $ TxDeserialisationFailed stTx e
        Right tx@ByronTx{byronTxId} -> do
          putStrLn $ "transaction hash (TxId): " <> show byronTxId
          handleTxSubmission cc p stTopology tx stdoutTracer

runCommand co@CLIOps{..}
           (SpendGenesisUTxO
            (NewTxFile ctTx) ctKey ctGenRichAddr ctOuts ctCommon) = do
  withRealPBFT co mainnetConfiguration ctCommon $
    \_cc (ProtocolRealPBFT gc _ _ _ _) -> do
      sk <- readSigningKey co ctKey
      let tx@ByronTx{byronTxId} = txSpendGenesisUTxOByronPBFT gc sk ctGenRichAddr ctOuts
      putStrLn $ "genesis protocol magic:  " <> show (configProtocolMagicId gc)
      putStrLn $ "transaction hash (TxId): " <> show byronTxId
      ensureNewFileLBS ctTx (serialise tx)

runCommand co@CLIOps{..}
           (SpendUTxO
            (NewTxFile ctTx) ctKey ctIns ctOuts ctCommon) = do
  withRealPBFT co mainnetConfiguration ctCommon $
    \_cc (ProtocolRealPBFT gc _ _ _ _) -> do
      sk <- readSigningKey co ctKey
      let tx@ByronTx{byronTxId} = txSpendUTxOByronPBFT gc sk ctIns ctOuts
      putStrLn $ "genesis protocol magic:  " <> show (configProtocolMagicId gc)
      putStrLn $ "transaction hash (TxId): " <> show byronTxId
      ensureNewFileLBS ctTx (serialise tx)
