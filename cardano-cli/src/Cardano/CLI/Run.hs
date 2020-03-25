{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.CLI.Run
  ( ClientCommand(..)
  , runClientCommand
  --
  , NewDirectory(..)
  , SigningKeyFile(..)
  , VerificationKeyFile(..)
  , NewVerificationKeyFile(..)
  , CertificateFile(..)
  , NewCertificateFile(..)
  , TxFile(..)
  , NewTxFile(..)

  -- * re-exports from Ouroboros-Network
  , IOManager
  , withIOManager
  ) where

import           Cardano.Prelude hiding (option, trace)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (hoistEither, firstExceptT)
import           Data.Semigroup ((<>))
import qualified Data.Text as Text
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as Builder
import           Data.Version (showVersion)
import qualified Formatting as F
import           Paths_cardano_cli (version)
import           System.Info (arch, compilerName, compilerVersion, os)

import qualified Cardano.Chain.Common as Common
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Chain.Slotting (EpochNumber)
import           Cardano.Chain.UTxO (TxIn, TxOut)

import qualified Cardano.Crypto.Hashing as Crypto
import qualified Cardano.Crypto.Signing as Crypto

import           Ouroboros.Network.NodeToClient ( IOManager
                                                , withIOManager
                                                )

import           Cardano.CLI.Delegation
import           Cardano.CLI.Genesis
import           Cardano.CLI.Key
import           Cardano.CLI.Ops
import           Cardano.CLI.Parsers
import           Cardano.CLI.Byron.Run (runByronClientCommand)
import           Cardano.CLI.Shelley.Run (runShelleyClientCommand)
import           Cardano.CLI.Tx
import           Cardano.Common.LocalSocket
import           Cardano.Config.Types


runClientCommand :: ClientCommand -> ExceptT CliError IO ()
runClientCommand cc =
  case cc of
    ByronClientCommand bc -> runByronClientCommand bc
    ShelleyClientCommand bc -> runShelleyClientCommand bc
    DisplayVersion -> runDisplayVersion
    Genesis outDir params era -> runGenesisCommand outDir params era
    GetLocalNodeTip configFp mSockPath -> runGetLocalNodeTip configFp mSockPath
    ValidateCBOR cborObject fp -> runValidateCBOR cborObject fp
    PrettyPrintCBOR fp -> runPrettyPrintCBOR fp
    PrettySigningKeyPublic era skF -> runPrettySigningKeyPublic era skF
    MigrateDelegateKeyFrom oldEra oldKey newEra nskf -> runMigrateDelegateKeyFrom oldEra oldKey newEra nskf
    PrintGenesisHash genFp -> runPrintGenesisHash genFp
    PrintSigningKeyAddress era netMagic skF -> runPrintSigningKeyAddress era netMagic skF
    Keygen era nskf passReq -> runKeygen era nskf passReq
    ToVerification era skFp nvkFp -> runToVerification era skFp nvkFp
    IssueDelegationCertificate configFp epoch issuerSK delVK cert -> runIssueDelegationCertificate configFp epoch issuerSK delVK cert
    CheckDelegation configFp cert issuerVF delegateVF -> runCheckDelegation configFp cert issuerVF delegateVF
    SubmitTx fp configFp mCliSockPath -> runSubmitTx fp configFp mCliSockPath
    SpendGenesisUTxO configFp nftx ctKey genRichAddr outs -> runSpendGenesisUTxO configFp nftx ctKey genRichAddr outs
    SpendUTxO configFp nftx ctKey ins outs -> runSpendUTxO configFp nftx ctKey ins outs

-- -----------------------------------------------------------------------------

runDisplayVersion :: ExceptT CliError IO ()
runDisplayVersion = do
    liftIO . putTextLn $ mconcat
                [ "cardano-cli " <> renderVersion version
                , " - " <> Text.pack os <> "-" <> Text.pack arch
                , " - " <> Text.pack compilerName <> "-" <> renderVersion compilerVersion
                ]
  where
    renderVersion = Text.pack . showVersion

runGenesisCommand :: NewDirectory -> GenesisParameters -> CardanoEra -> ExceptT CliError IO ()
runGenesisCommand outDir params era = do
  (genData, genSecrets) <- mkGenesis params
  dumpGenesis era outDir genData genSecrets

runGetLocalNodeTip :: ConfigYamlFilePath -> Maybe CLISocketPath -> ExceptT e IO ()
runGetLocalNodeTip configFp mSockPath =
  withIOManagerE $ \iocp -> liftIO $ getLocalTip configFp mSockPath iocp

runValidateCBOR :: CBORObject -> FilePath -> ExceptT CliError IO ()
runValidateCBOR cborObject fp = do
  bs <- readCBOR fp
  res <- hoistEither $ validateCBOR cborObject bs
  liftIO $ putTextLn res

runPrettyPrintCBOR :: FilePath -> ExceptT CliError IO ()
runPrettyPrintCBOR fp = do
  bs <- readCBOR fp
  pPrintCBOR bs

runPrettySigningKeyPublic :: CardanoEra -> SigningKeyFile -> ExceptT CliError IO ()
runPrettySigningKeyPublic era skF = do
  sK <- readSigningKey era skF
  liftIO . putTextLn . prettyPublicKey $ Crypto.toVerification sK

runMigrateDelegateKeyFrom
        :: CardanoEra -> SigningKeyFile -> CardanoEra -> NewSigningKeyFile
        -> ExceptT CliError IO ()
runMigrateDelegateKeyFrom oldEra oldKey newEra (NewSigningKeyFile newKey) = do
  sk <- readSigningKey oldEra oldKey
  sDk <- hoistEither $ serialiseDelegateKey newEra sk
  ensureNewFileLBS newKey sDk

runPrintGenesisHash :: GenesisFile -> ExceptT CliError IO ()
runPrintGenesisHash genFp = do
    gen <- readGenesis genFp
    liftIO . putTextLn $ formatter gen
  where
    formatter :: (a, Genesis.GenesisHash)-> Text
    formatter = F.sformat Crypto.hashHexF . Genesis.unGenesisHash . snd

runPrintSigningKeyAddress :: CardanoEra -> Common.NetworkMagic -> SigningKeyFile -> ExceptT CliError IO ()
runPrintSigningKeyAddress era netMagic skF = do
  sK <- readSigningKey era skF
  let sKeyAddress = prettyAddress . Common.makeVerKeyAddress netMagic $ Crypto.toVerification sK
  liftIO $ putTextLn sKeyAddress

runKeygen :: CardanoEra -> NewSigningKeyFile -> PasswordRequirement -> ExceptT CliError IO ()
runKeygen era (NewSigningKeyFile skF) passReq = do
  pPhrase <- liftIO $ getPassphrase ("Enter password to encrypt '" <> skF <> "': ") passReq
  sK <- liftIO $ keygen pPhrase
  serDk <- hoistEither $ serialiseDelegateKey era sK
  ensureNewFileLBS skF serDk

runToVerification :: CardanoEra -> SigningKeyFile -> NewVerificationKeyFile -> ExceptT CliError IO ()
runToVerification era skFp (NewVerificationKeyFile vkFp) = do
  sk <- readSigningKey era skFp
  let vKey = Builder.toLazyText . Crypto.formatFullVerificationKey $ Crypto.toVerification sk
  ensureNewFile TL.writeFile vkFp vKey

runIssueDelegationCertificate
        :: ConfigYamlFilePath -> EpochNumber -> SigningKeyFile -> VerificationKeyFile -> NewCertificateFile
        -> ExceptT CliError IO ()
runIssueDelegationCertificate configFp epoch issuerSK delegateVK cert = do
  nc <- liftIO $ parseNodeConfigurationFP configFp
  vk <- readVerificationKey delegateVK
  sk <- readSigningKey (ncCardanoEra nc) issuerSK
  pmId <- readProtocolMagicId $ ncGenesisFile nc
  let byGenDelCert :: Delegation.Certificate
      byGenDelCert = issueByronGenesisDelegation pmId epoch sk vk
  sCert <- hoistEither $ serialiseDelegationCert (ncCardanoEra nc) byGenDelCert
  ensureNewFileLBS (nFp cert) sCert

runCheckDelegation
        :: ConfigYamlFilePath -> CertificateFile -> VerificationKeyFile -> VerificationKeyFile
        -> ExceptT CliError IO ()
runCheckDelegation configFp cert issuerVF delegateVF = do
  nc <- liftIO $ parseNodeConfigurationFP configFp
  issuerVK <- readVerificationKey issuerVF
  delegateVK <- readVerificationKey delegateVF
  pmId <- readProtocolMagicId $ ncGenesisFile nc
  checkByronGenesisDelegation cert pmId issuerVK delegateVK

runSubmitTx :: TxFile -> ConfigYamlFilePath -> Maybe CLISocketPath -> ExceptT CliError IO ()
runSubmitTx fp configFp mCliSockPath =
  withIOManagerE $ \iocp -> do
    nc <- liftIO $ parseNodeConfigurationFP configFp
    tx <- readByronTx fp
    --TODO: just override the nc { ncSocketPath }
    let sockPath = chooseSocketPath (ncSocketPath nc) mCliSockPath

    firstExceptT NodeSubmitTxError $
      nodeSubmitTx iocp nc sockPath tx

runSpendGenesisUTxO
        :: ConfigYamlFilePath -> NewTxFile -> SigningKeyFile -> Common.Address -> NonEmpty TxOut
        -> ExceptT CliError IO ()
runSpendGenesisUTxO configFp (NewTxFile ctTx) ctKey genRichAddr outs = do
    nc <- liftIO $ parseNodeConfigurationFP configFp
    sk <- readSigningKey (ncCardanoEra nc) ctKey

    tx <- firstExceptT SpendGenesisUTxOError $
            issueGenesisUTxOExpenditure nc genRichAddr outs sk
    ensureNewFileLBS ctTx $ toCborTxAux tx

runSpendUTxO
        :: ConfigYamlFilePath -> NewTxFile -> SigningKeyFile -> NonEmpty TxIn -> NonEmpty TxOut
        -> ExceptT CliError IO ()
runSpendUTxO configFp (NewTxFile ctTx) ctKey ins outs = do
    nc <- liftIO $ parseNodeConfigurationFP configFp
    sk <- readSigningKey (ncCardanoEra nc) ctKey

    gTx <- firstExceptT IssueUtxoError $
             issueUTxOExpenditure nc ins outs sk
    ensureNewFileLBS ctTx $ toCborTxAux gTx

