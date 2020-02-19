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
  ) where

import           Cardano.Prelude hiding (option, trace)

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (hoistEither, firstExceptT)
import qualified Data.ByteString.Lazy as LB
import           Data.Semigroup ((<>))
import           Data.Text (pack)
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as Builder
import           Data.Version (showVersion)
import qualified Formatting as F
import           Paths_cardano_node (version)
import           System.Directory (doesPathExist)

import qualified Cardano.Chain.Common as Common
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Chain.Slotting (EpochNumber(..))
import qualified Cardano.Chain.UTxO as UTxO
import           Cardano.Chain.Update (ApplicationName(..))

import           Cardano.Crypto (ProtocolMagicId, RequiresNetworkMagic(..))
import qualified Cardano.Crypto.Hashing as Crypto
import qualified Cardano.Crypto.Signing as Crypto

import qualified Ouroboros.Consensus.Protocol as Consensus

import           Cardano.CLI.Delegation
import           Cardano.CLI.Genesis
import           Cardano.CLI.Key
import           Cardano.CLI.Ops
import           Cardano.CLI.Tx
import           Cardano.CLI.Benchmarking.Tx.Generation
                   ( ExplorerAPIEnpoint (..)
                   , NumberOfTxs (..)
                   , NumberOfInputsPerTx (..)
                   , NumberOfOutputsPerTx (..)
                   , FeePerTx (..), TPSRate (..)
                   , TxAdditionalSize (..)
                   , genesisBenchmarkRunner
                   )
import           Cardano.Common.Orphans ()
import           Cardano.Config.Protocol
import           Cardano.Config.Logging (createLoggingFeatureCLI)
import           Cardano.Config.Types ( CardanoEnvironment(..), DelegationCertFile(..)
                                      , GenesisFile(..), LastKnownBlockVersion(..)
                                      , NodeAddress(..), NodeConfiguration(..)
                                      , SigningKeyFile(..), SocketPath(..), Update(..)
                                      , parseNodeConfigurationFP)

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
    Protocol
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
    TxFile
    -- ^ Filepath of transaction to submit.
    Protocol
    GenesisFile
    SocketPath
    -- ^ Socket path of target node.
  | SpendGenesisUTxO
    Protocol
    GenesisFile
    NewTxFile
    -- ^ Filepath of the newly created transaction.
    SigningKeyFile
    -- ^ Signing key of genesis UTxO owner.
    Common.Address
    -- ^ Genesis UTxO address.
    (NonEmpty UTxO.TxOut)
    -- ^ Tx output.
  | SpendUTxO
    Protocol
    GenesisFile
    NewTxFile
    -- ^ Filepath of the newly created transaction.
    SigningKeyFile
    -- ^ Signing key of Tx underwriter.
    (NonEmpty UTxO.TxIn)
    -- ^ Inputs available for spending to the Tx underwriter's key.
    (NonEmpty UTxO.TxOut)
    -- ^ Genesis UTxO output Address.

    --- Tx Generator Command ----------

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
   deriving Show

runCommand :: ClientCommand -> ExceptT CliError IO ()
runCommand (Genesis outDir params ptcl) = do
  gen <- mkGenesis params
  dumpGenesis ptcl outDir `uncurry` gen

runCommand (PrettySigningKeyPublic ptcl skF) = do
  sK <- readSigningKey ptcl skF
  liftIO . putTextLn . prettyPublicKey $ Crypto.toVerification sK
runCommand (MigrateDelegateKeyFrom oldPtcl oldKey newPtcl (NewSigningKeyFile newKey)) = do
  sk <- readSigningKey oldPtcl oldKey
  sDk <- hoistEither $ serialiseDelegateKey newPtcl sk
  liftIO $ ensureNewFileLBS newKey sDk

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
  liftIO $ ensureNewFileLBS skF serDk

runCommand (ToVerification ptcl skFp (NewVerificationKeyFile vkFp)) = do
  sk <- readSigningKey ptcl skFp
  let vKey = Builder.toLazyText . Crypto.formatFullVerificationKey $ Crypto.toVerification sk
  liftIO $ ensureNewFile TL.writeFile vkFp vKey

runCommand (IssueDelegationCertificate ptcl magic epoch issuerSK delegateVK cert) = do
  vk <- readVerificationKey delegateVK
  sk <- readSigningKey ptcl issuerSK
  let byGenDelCert :: Delegation.Certificate
      byGenDelCert = issueByronGenesisDelegation magic epoch sk vk
  sCert <- hoistEither $ serialiseDelegationCert ptcl byGenDelCert
  liftIO $ ensureNewFileLBS (nFp cert) sCert

runCommand (CheckDelegation magic cert issuerVF delegateVF) = do
  issuerVK <- readVerificationKey issuerVF
  delegateVK <- readVerificationKey delegateVF
  liftIO $ checkByronGenesisDelegation cert magic issuerVK delegateVK

runCommand (SubmitTx fp ptcl genFile socketPath) = do
    -- Default update value
    let update = Update (ApplicationName "cardano-sl") 1 $ LastKnownBlockVersion 0 2 0
    tx <- liftIO $ readByronTx fp
    genHash <- getGenesisHash genFile

    firstExceptT
      NodeSubmitTxError
      $ nodeSubmitTx
          genHash
          Nothing
          genFile
          RequiresNoMagic
          Nothing
          Nothing
          Nothing
          socketPath
          update
          ptcl
          tx
runCommand (SpendGenesisUTxO ptcl genFile (NewTxFile ctTx) ctKey genRichAddr outs) = do
    sk <- readSigningKey ptcl ctKey
    -- Default update value
    let update = Update (ApplicationName "cardano-sl") 1 $ LastKnownBlockVersion 0 2 0

    genHash <- getGenesisHash genFile

    tx <- firstExceptT SpendGenesisUTxOError
            $ issueGenesisUTxOExpenditure
                genRichAddr
                outs
                genHash
                genFile
                RequiresNoMagic
                Nothing
                Nothing
                Nothing
                update
                ptcl
                sk
    liftIO . ensureNewFileLBS ctTx $ toCborTxAux tx

runCommand (SpendUTxO ptcl genFile (NewTxFile ctTx) ctKey ins outs) = do
    sk <- readSigningKey ptcl ctKey
    -- Default update value
    let update = Update (ApplicationName "cardano-sl") 1 $ LastKnownBlockVersion 0 2 0

    genHash <- getGenesisHash genFile

    gTx <- firstExceptT
             IssueUtxoError
             $ issueUTxOExpenditure
                 ins
                 outs
                 genHash
                 genFile
                 RequiresNoMagic
                 Nothing
                 Nothing
                 Nothing
                 update
                 ptcl
                 sk
    liftIO . ensureNewFileLBS ctTx $ toCborTxAux gTx

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
               sigKeysFiles) = do
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
ensureNewFile :: (FilePath -> a -> IO ()) -> FilePath -> a -> IO ()
ensureNewFile writer outFile blob = do
  exists <- doesPathExist outFile
  when exists $
    throwIO $ OutputMustNotAlreadyExist outFile
  writer outFile blob

ensureNewFileLBS :: FilePath -> LB.ByteString -> IO ()
ensureNewFileLBS = ensureNewFile LB.writeFile
