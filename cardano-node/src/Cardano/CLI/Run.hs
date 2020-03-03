{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.CLI.Run (
    CliError (..)
  , ClientCommand(..)
  , runCommand
  --
  , NewDirectory(..)
  , SigningKeyFile(..)
  , VerificationKeyFile(..)
  , NewVerificationKeyFile(..)
  , CertificateFile(..)
  , NewCertificateFile(..)
  , TxFile(..)
  , NewTxFile(..)
  , NumberOfTxs(..)
  , NumberOfInputsPerTx(..)
  , NumberOfOutputsPerTx(..)
  , FeePerTx(..)
  , TPSRate(..)
  , TxAdditionalSize(..)
  , ExplorerAPIEnpoint(..)

  -- * re-exports from Ouroboros-Network
  , AssociateWithIOCP
  , withIOManager
  ) where

import           Cardano.Prelude hiding (option, trace)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (hoistEither, firstExceptT, left)
import qualified Data.ByteString.Lazy as LB
import           Data.Semigroup ((<>))
import           Data.Text (pack)
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as Builder
import           Data.Version (showVersion)
import qualified Formatting as F
import           Paths_cardano_node (version)
import           System.Directory (doesPathExist)
import           System.Info (arch, compilerName, compilerVersion, os)

import qualified Cardano.Chain.Common as Common
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Chain.Slotting (EpochNumber(..))
import qualified Cardano.Chain.UTxO as UTxO
import           Cardano.Chain.Update (ApplicationName(..))

import           Cardano.Crypto (RequiresNetworkMagic(..))
import qualified Cardano.Crypto.Hashing as Crypto
import qualified Cardano.Crypto.Signing as Crypto

import           Ouroboros.Network.NodeToClient ( AssociateWithIOCP
                                                , withIOManager
                                                )

import qualified Ouroboros.Consensus.Cardano as Consensus

import           Cardano.CLI.Delegation
import           Cardano.CLI.Genesis
import           Cardano.CLI.Key
import           Cardano.CLI.Ops
import           Cardano.CLI.Tx
import           Cardano.Tx.Generator
                   (ExplorerAPIEnpoint (..), NumberOfTxs (..)
                   , NumberOfInputsPerTx (..), NumberOfOutputsPerTx (..)
                   , FeePerTx (..), TPSRate (..), TxAdditionalSize (..)
                   , genesisBenchmarkRunner)
import           Cardano.Common.LocalSocket
import           Cardano.Config.Protocol
import           Cardano.Config.Logging (createLoggingFeatureCLI)
import           Cardano.Config.Types

-- | Sub-commands of 'cardano-cli'.
data ClientCommand
  =
  --- Genesis Related Commands ---
    Genesis
    NewDirectory
    GenesisParameters
    Protocol
  | PrintGenesisHash
    GenesisFile

  --- Key Related Commands ---
  | Keygen
    Protocol
    NewSigningKeyFile
    PasswordRequirement
  | ToVerification
    Protocol
    SigningKeyFile
    NewVerificationKeyFile

  | PrettySigningKeyPublic
    Protocol
    SigningKeyFile

  | MigrateDelegateKeyFrom
    Protocol
    -- ^ Old protocol
    SigningKeyFile
    -- ^ Old key
    Protocol
    -- ^ New protocol
    NewSigningKeyFile
    -- ^ New Key

  | PrintSigningKeyAddress
    Protocol
    Common.NetworkMagic  -- TODO:  consider deprecation in favor of ProtocolMagicId,
                         --        once Byron is out of the picture.
    SigningKeyFile

    --- Delegation Related Commands ---

  | IssueDelegationCertificate
    ConfigYamlFilePath
    EpochNumber
    -- ^ The epoch from which the delegation is valid.
    SigningKeyFile
    -- ^ The issuer of the certificate, who delegates their right to sign blocks.
    VerificationKeyFile
    -- ^ The delegate, who gains the right to sign blocks on behalf of the issuer.
    NewCertificateFile
    -- ^ Filepath of the newly created delegation certificate.
  | CheckDelegation
    ConfigYamlFilePath
    CertificateFile
    VerificationKeyFile
    VerificationKeyFile

  | GetLocalNodeTip
    ConfigYamlFilePath
    (Maybe CLISocketPath)

    -----------------------------------

  | SubmitTx
    TxFile
    -- ^ Filepath of transaction to submit.
    ConfigYamlFilePath
    (Maybe CLISocketPath)

  | SpendGenesisUTxO
    ConfigYamlFilePath
    NewTxFile
    -- ^ Filepath of the newly created transaction.
    SigningKeyFile
    -- ^ Signing key of genesis UTxO owner.
    Common.Address
    -- ^ Genesis UTxO address.
    (NonEmpty UTxO.TxOut)
    -- ^ Tx output.
  | SpendUTxO
    ConfigYamlFilePath
    NewTxFile
    -- ^ Filepath of the newly created transaction.
    SigningKeyFile
    -- ^ Signing key of Tx underwriter.
    (NonEmpty UTxO.TxIn)
    -- ^ Inputs available for spending to the Tx underwriter's key.
    (NonEmpty UTxO.TxOut)
    -- ^ Genesis UTxO output Address.

    --- Tx Generator Command ---

  | GenerateTxs
    FilePath
    -- ^ Configuration yaml
    SigningKeyFile
    DelegationCertFile
    GenesisFile
    -- ^ Genesis hash
    SocketPath
    (NonEmpty NodeAddress)
    NumberOfTxs
    NumberOfInputsPerTx
    NumberOfOutputsPerTx
    FeePerTx
    TPSRate
    (Maybe TxAdditionalSize)
    (Maybe ExplorerAPIEnpoint)
    [SigningKeyFile]

    --- Misc Commands ---

  | DisplayVersion

  | ValidateCBOR
    CBORObject
    -- ^ Type of the CBOR object
    FilePath

  | PrettyPrintCBOR
    FilePath
   deriving Show


runCommand :: ClientCommand -> ExceptT CliError IO ()
runCommand DisplayVersion = do
  liftIO . putTextLn
         . toS
         $ concat [ "cardano-cli " <> showVersion version
                  , " - " <> os <> "-" <> arch
                  , " - " <> compilerName <> "-" <> showVersion compilerVersion
                  ]

runCommand (Genesis outDir params ptcl) = do
  gen <- mkGenesis params
  dumpGenesis ptcl outDir `uncurry` gen

runCommand (GetLocalNodeTip configFp mSockPath) =
  withIOManagerE $ \iocp -> liftIO $ getLocalTip configFp mSockPath iocp

runCommand (ValidateCBOR cborObject fp) = do
  bs <- readCBOR fp
  validateCBOR cborObject bs

runCommand (PrettyPrintCBOR fp) = do
  bs <- readCBOR fp
  pPrintCBOR bs

runCommand (PrettySigningKeyPublic ptcl skF) = do
  sK <- readSigningKey ptcl skF
  liftIO . putTextLn . prettyPublicKey $ Crypto.toVerification sK
runCommand (MigrateDelegateKeyFrom oldPtcl oldKey newPtcl (NewSigningKeyFile newKey)) = do
  sk <- readSigningKey oldPtcl oldKey
  sDk <- hoistEither $ serialiseDelegateKey newPtcl sk
  ensureNewFileLBS newKey sDk

runCommand (PrintGenesisHash genFp) = do
  eGen <- readGenesis genFp

  let formatter :: (a, Genesis.GenesisHash)-> Text
      formatter = F.sformat Crypto.hashHexF . Genesis.unGenesisHash . snd

  liftIO . putTextLn $ formatter eGen

runCommand (PrintSigningKeyAddress ptcl netMagic skF) = do
  sK <- readSigningKey ptcl skF
  let sKeyAddress = prettyAddress . Common.makeVerKeyAddress netMagic $ Crypto.toVerification sK
  liftIO $ putTextLn sKeyAddress

runCommand (Keygen ptcl (NewSigningKeyFile skF) passReq) = do
  pPhrase <- liftIO $ getPassphrase ("Enter password to encrypt '" <> skF <> "': ") passReq
  sK <- liftIO $ keygen pPhrase
  serDk <- hoistEither $ serialiseDelegateKey ptcl sK
  ensureNewFileLBS skF serDk

runCommand (ToVerification ptcl skFp (NewVerificationKeyFile vkFp)) = do
  sk <- readSigningKey ptcl skFp
  let vKey = Builder.toLazyText . Crypto.formatFullVerificationKey $ Crypto.toVerification sk
  ensureNewFile TL.writeFile vkFp vKey

runCommand (IssueDelegationCertificate configFp epoch issuerSK delegateVK cert) = do
  nc <- liftIO . parseNodeConfigurationFP $ unConfigPath configFp
  vk <- readVerificationKey delegateVK
  sk <- readSigningKey (ncProtocol nc) issuerSK
  pmId <- readProtocolMagicId $ ncGenesisFile nc
  let byGenDelCert :: Delegation.Certificate
      byGenDelCert = issueByronGenesisDelegation pmId epoch sk vk
  sCert <- hoistEither $ serialiseDelegationCert (ncProtocol nc) byGenDelCert
  ensureNewFileLBS (nFp cert) sCert

runCommand (CheckDelegation configFp cert issuerVF delegateVF) = do
  nc <- liftIO . parseNodeConfigurationFP $ unConfigPath configFp
  issuerVK <- readVerificationKey issuerVF
  delegateVK <- readVerificationKey delegateVF
  pmId <- readProtocolMagicId $ ncGenesisFile nc
  checkByronGenesisDelegation cert pmId issuerVK delegateVK

runCommand (SubmitTx fp configFp mCliSockPath) = withIOManagerE $ \iocp -> do
    nc <- liftIO . parseNodeConfigurationFP $ unConfigPath configFp
    -- Default update value
    let update = Update (ApplicationName "cardano-sl") 1 $ LastKnownBlockVersion 0 2 0
    tx <- readByronTx fp
    genHash <- getGenesisHash (ncGenesisFile nc)

    firstExceptT
      NodeSubmitTxError
      $ nodeSubmitTx
          iocp
          genHash
          Nothing
          (ncGenesisFile nc)
          RequiresNoMagic
          Nothing
          Nothing
          Nothing
          (chooseSocketPath (ncSocketPath nc) mCliSockPath)
          update
          (ncProtocol nc)
          tx
runCommand (SpendGenesisUTxO configFp (NewTxFile ctTx) ctKey genRichAddr outs) = do
    nc <- liftIO . parseNodeConfigurationFP $ unConfigPath configFp
    sk <- readSigningKey (ncProtocol nc) ctKey
    -- Default update value
    let update = Update (ApplicationName "cardano-sl") 1 $ LastKnownBlockVersion 0 2 0

    genHash <- getGenesisHash $ ncGenesisFile nc

    tx <- firstExceptT SpendGenesisUTxOError
            $ issueGenesisUTxOExpenditure
                genRichAddr
                outs
                genHash
                (ncGenesisFile nc)
                RequiresNoMagic
                Nothing
                Nothing
                Nothing
                update
                (ncProtocol nc)
                sk
    ensureNewFileLBS ctTx $ toCborTxAux tx

runCommand (SpendUTxO configFp (NewTxFile ctTx) ctKey ins outs) = do
    nc <- liftIO . parseNodeConfigurationFP $ unConfigPath configFp
    sk <- readSigningKey (ncProtocol nc) ctKey
    -- Default update value
    let update = Update (ApplicationName "cardano-sl") 1 $ LastKnownBlockVersion 0 2 0

    genHash <- getGenesisHash $ ncGenesisFile nc

    gTx <- firstExceptT
             IssueUtxoError
             $ issueUTxOExpenditure
                 ins
                 outs
                 genHash
                 (ncGenesisFile nc)
                 RequiresNoMagic
                 Nothing
                 Nothing
                 Nothing
                 update
                 (ncProtocol nc)
                 sk
    ensureNewFileLBS ctTx $ toCborTxAux gTx

runCommand (GenerateTxs
            logConfigFp
            signingKey
            delegCert
            genFile
            socketFp
            targetNodeAddresses
            numOfTxs
            numOfInsPerTx
            numOfOutsPerTx
            feePerTx
            tps
            txAdditionalSize
            explorerAPIEndpoint
            sigKeysFiles) = withIOManagerE $ \iocp -> do
  -- Default update value
  let update = Update (ApplicationName "cardano-sl") 1 $ LastKnownBlockVersion 0 2 0
  nc <- liftIO $ parseNodeConfigurationFP logConfigFp

  -- Logging layer
  (loggingLayer, _) <- liftIO $ createLoggingFeatureCLI
                                  (pack $ showVersion version)
                                  NoEnvironment
                                  (Just logConfigFp)
                                  (ncLogMetrics nc)

  genHash <- getGenesisHash genFile

  firstExceptT
    GenerateTxsError
    $  withRealPBFT
         genHash
         genFile
         (ncReqNetworkMagic nc)
         Nothing
         (Just delegCert)
         (Just signingKey)
         update
         (ncProtocol nc) $ \protocol@(Consensus.ProtocolRealPBFT _ _ _ _ _) ->
                             firstExceptT GenesisBenchmarkRunnerError
                               $ genesisBenchmarkRunner
                                    loggingLayer
                                    iocp
                                    socketFp
                                    protocol
                                    targetNodeAddresses
                                    numOfTxs
                                    numOfInsPerTx
                                    numOfOutsPerTx
                                    feePerTx
                                    tps
                                    txAdditionalSize
                                    explorerAPIEndpoint
                                    [fp | SigningKeyFile fp <- sigKeysFiles]

{-------------------------------------------------------------------------------
  Supporting functions
-------------------------------------------------------------------------------}

-- TODO:  we'd be better served by a combination of a temporary file
--        with an atomic rename.
-- | Checks if a path exists and throws and error if it does.
ensureNewFile :: (FilePath -> a -> IO ()) -> FilePath -> a -> ExceptT CliError IO ()
ensureNewFile writer outFile blob = do
  exists <- liftIO $ doesPathExist outFile
  when exists $
    left $ OutputMustNotAlreadyExist outFile
  liftIO $ writer outFile blob

ensureNewFileLBS :: FilePath -> LB.ByteString -> ExceptT CliError IO ()
ensureNewFileLBS = ensureNewFile LB.writeFile

withIOManagerE :: (AssociateWithIOCP -> ExceptT e IO a) -> ExceptT e IO a
withIOManagerE k = ExceptT $ withIOManager (runExceptT . k)
