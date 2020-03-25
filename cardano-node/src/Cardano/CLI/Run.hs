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
  , IOManager
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
import           Cardano.Chain.Update (ApplicationName(..))

import           Cardano.Crypto (RequiresNetworkMagic(..))
import qualified Cardano.Crypto.Hashing as Crypto
import qualified Cardano.Crypto.Signing as Crypto

import           Ouroboros.Network.NodeToClient ( IOManager
                                                , withIOManager
                                                )

import qualified Ouroboros.Consensus.Cardano as Consensus

import           Cardano.CLI.Byron.Parsers (ByronCommand(..))
import           Cardano.CLI.Byron.UpdateProposal
                   (createUpdateProposal, serialiseByronUpdateProposal)
import           Cardano.CLI.Delegation
import           Cardano.CLI.Genesis
import           Cardano.CLI.Key
import           Cardano.CLI.Ops
import           Cardano.CLI.Parsers
import           Cardano.CLI.Tx
import           Cardano.Benchmarking.GeneratorTx
                   (ExplorerAPIEnpoint (..), NumberOfTxs (..)
                   , NumberOfInputsPerTx (..), NumberOfOutputsPerTx (..)
                   , FeePerTx (..), TPSRate (..), TxAdditionalSize (..)
                   , genesisBenchmarkRunner)
import           Cardano.Common.LocalSocket
import           Cardano.Config.Logging (createLoggingFeatureCLI)
import           Cardano.Config.Types


runCommand :: ClientCommand -> ExceptT CliError IO ()
runCommand (ByronClientCommand (UpdateProposal dbFp configFp sKey outputFp paramsToUpdate)) = do
  sK <- readSigningKey RealPBFT sKey
  proposal <- createUpdateProposal dbFp configFp sK paramsToUpdate
  ensureNewFileLBS outputFp (serialiseByronUpdateProposal proposal)

runCommand DisplayVersion = do
  liftIO . putTextLn
         . toS
         $ concat [ "cardano-cli " <> showVersion version
                  , " - " <> os <> "-" <> arch
                  , " - " <> compilerName <> "-" <> showVersion compilerVersion
                  ]

runCommand (Genesis outDir params ptcl) = do
  (genData, genSecrets) <- mkGenesis params
  dumpGenesis ptcl outDir genData genSecrets

runCommand (GetLocalNodeTip configFp mSockPath) =
  withIOManagerE $ \iocp -> liftIO $ getLocalTip configFp mSockPath iocp

runCommand (ValidateCBOR cborObject fp) = do
  bs <- readCBOR fp
  res <- hoistEither $ validateCBOR cborObject bs
  liftIO $ putTextLn res

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
  nc <- liftIO $ parseNodeConfigurationFP configFp
  vk <- readVerificationKey delegateVK
  sk <- readSigningKey (ncProtocol nc) issuerSK
  pmId <- readProtocolMagicId $ ncGenesisFile nc
  let byGenDelCert :: Delegation.Certificate
      byGenDelCert = issueByronGenesisDelegation pmId epoch sk vk
  sCert <- hoistEither $ serialiseDelegationCert (ncProtocol nc) byGenDelCert
  ensureNewFileLBS (nFp cert) sCert

runCommand (CheckDelegation configFp cert issuerVF delegateVF) = do
  nc <- liftIO $ parseNodeConfigurationFP configFp
  issuerVK <- readVerificationKey issuerVF
  delegateVK <- readVerificationKey delegateVF
  pmId <- readProtocolMagicId $ ncGenesisFile nc
  checkByronGenesisDelegation cert pmId issuerVK delegateVK

runCommand (SubmitTx fp configFp mCliSockPath) = withIOManagerE $ \iocp -> do
    nc <- liftIO $ parseNodeConfigurationFP configFp
    -- Default update value
    let update = Update (ApplicationName "cardano-sl") 1 $ LastKnownBlockVersion 0 2 0
    tx <- readByronTx fp
    genHash <- getGenesisHashText (ncGenesisFile nc)

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
    nc <- liftIO $ parseNodeConfigurationFP configFp
    sk <- readSigningKey (ncProtocol nc) ctKey
    -- Default update value
    let update = Update (ApplicationName "cardano-sl") 1 $ LastKnownBlockVersion 0 2 0

    genHash <- getGenesisHashText $ ncGenesisFile nc

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
    nc <- liftIO $ parseNodeConfigurationFP configFp
    sk <- readSigningKey (ncProtocol nc) ctKey
    -- Default update value
    let update = Update (ApplicationName "cardano-sl") 1 $ LastKnownBlockVersion 0 2 0

    genHash <- getGenesisHashText $ ncGenesisFile nc

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
            configFp
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
  nc <- liftIO . parseNodeConfigurationFP $ ConfigYamlFilePath configFp

  -- Logging layer
  (loggingLayer, _) <- firstExceptT (\(ConfigErrorFileNotFound fp) -> FileNotFoundError fp) $
                           createLoggingFeatureCLI
                           (pack $ showVersion version)
                           NoEnvironment
                           (Just configFp)
                           (ncLogMetrics nc)

  genHash <- getGenesisHashText genFile

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

withIOManagerE :: (IOManager -> ExceptT e IO a) -> ExceptT e IO a
withIOManagerE k = ExceptT $ withIOManager (runExceptT . k)
