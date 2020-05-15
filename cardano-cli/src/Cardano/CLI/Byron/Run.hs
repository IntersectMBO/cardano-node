module Cardano.CLI.Byron.Run
  ( ByronClientCmdError
  , runByronClientCommand
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (hoistEither, firstExceptT)
import           Data.Semigroup ((<>))
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as Builder
import qualified Formatting as F

import           Ouroboros.Network.NodeToClient (IOManager, withIOManager)

import qualified Cardano.Chain.Common as Common
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Chain.Slotting (EpochNumber)
import           Cardano.Chain.UTxO (TxIn, TxOut)

import qualified Cardano.Crypto.Hashing as Crypto
import qualified Cardano.Crypto.Signing as Crypto

import           Cardano.Config.Protocol (CardanoEra, RealPBFTError, ncCardanoEra)
import           Cardano.Config.Types

import           Cardano.Api (Network, toByronNetworkMagic)
import           Cardano.CLI.Byron.Commands
import           Cardano.CLI.Byron.Delegation
import           Cardano.CLI.Byron.Genesis
import           Cardano.CLI.Byron.Key
import           Cardano.CLI.Byron.Query
import           Cardano.CLI.Byron.Tx
import           Cardano.CLI.Byron.UpdateProposal
import           Cardano.CLI.Byron.Vote (ByronVoteError, runVoteCreation, submitByronVote)

import           Cardano.CLI.Helpers

-- | Data type that encompasses all the possible errors of the
-- Byron client.
data ByronClientCmdError
  = ByronCmdDelegationError !ByronDelegationError
  | ByronCmdGenesisError !ByronGenesisError
  | ByronCmdHelpersError !HelpersError
  | ByronCmdKeyFailure !ByronKeyFailure
  | ByronCmdQueryError !ByronQueryError
  | ByronCmdRealPBFTError !RealPBFTError
  | ByronCmdTxError !ByronTxError
  | ByronCmdUpdateProposalError !ByronUpdateProposalError
  | ByronCmdVoteError !ByronVoteError
  deriving Show

runByronClientCommand :: ByronCommand -> ExceptT ByronClientCmdError IO ()
runByronClientCommand c =
  case c of
    NodeCmd bc -> runNodeCmd bc
    Genesis outDir params era -> runGenesisCommand outDir params era
    GetLocalNodeTip network -> firstExceptT ByronCmdQueryError $ runGetLocalNodeTip network
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
    SubmitTx network fp -> runSubmitTx network fp
    SpendGenesisUTxO configFp nftx ctKey genRichAddr outs -> runSpendGenesisUTxO configFp nftx ctKey genRichAddr outs
    SpendUTxO configFp nftx ctKey ins outs -> runSpendUTxO configFp nftx ctKey ins outs


runNodeCmd :: NodeCmd -> ExceptT ByronClientCmdError IO ()
runNodeCmd (CreateVote configFp sKey upPropFp voteBool outputFp) =
  firstExceptT ByronCmdVoteError $ runVoteCreation configFp sKey upPropFp voteBool outputFp

runNodeCmd (SubmitUpdateProposal network proposalFp) =
  withIOManagerE $ \iomgr ->
    firstExceptT ByronCmdUpdateProposalError
      $ submitByronUpdateProposal iomgr network proposalFp

runNodeCmd (SubmitVote network voteFp) =
  withIOManagerE $ \iomgr ->
    firstExceptT ByronCmdVoteError $ submitByronVote iomgr network voteFp

runNodeCmd (UpdateProposal configFp sKey pVer sVer sysTag insHash outputFp params) =
  firstExceptT ByronCmdUpdateProposalError
    $ runProposalCreation configFp sKey pVer sVer sysTag insHash outputFp params

runGenesisCommand :: NewDirectory -> GenesisParameters -> CardanoEra -> ExceptT ByronClientCmdError IO ()
runGenesisCommand outDir params era = do
  (genData, genSecrets) <- firstExceptT ByronCmdGenesisError $ mkGenesis params
  firstExceptT ByronCmdGenesisError $ dumpGenesis era outDir genData genSecrets

runValidateCBOR :: CBORObject -> FilePath -> ExceptT ByronClientCmdError IO ()
runValidateCBOR cborObject fp = do
  bs <- firstExceptT ByronCmdHelpersError $ readCBOR fp
  res <- hoistEither . first ByronCmdHelpersError $ validateCBOR cborObject bs
  liftIO $ putTextLn res

runPrettyPrintCBOR :: FilePath -> ExceptT ByronClientCmdError IO ()
runPrettyPrintCBOR fp = do
  bs <- firstExceptT ByronCmdHelpersError $ readCBOR fp
  firstExceptT ByronCmdHelpersError $ pPrintCBOR bs

runPrettySigningKeyPublic :: CardanoEra -> SigningKeyFile -> ExceptT ByronClientCmdError IO ()
runPrettySigningKeyPublic era skF = do
  sK <- firstExceptT ByronCmdKeyFailure $ readEraSigningKey era skF
  liftIO . putTextLn . prettyPublicKey $ Crypto.toVerification sK

runMigrateDelegateKeyFrom
        :: CardanoEra -> SigningKeyFile -> CardanoEra -> NewSigningKeyFile
        -> ExceptT ByronClientCmdError IO ()
runMigrateDelegateKeyFrom oldEra oldKey newEra (NewSigningKeyFile newKey) = do
  sk <- firstExceptT ByronCmdKeyFailure $ readEraSigningKey oldEra oldKey
  sDk <- hoistEither . first ByronCmdDelegationError $ serialiseDelegateKey newEra sk
  firstExceptT ByronCmdHelpersError $ ensureNewFileLBS newKey sDk

runPrintGenesisHash :: GenesisFile -> ExceptT ByronClientCmdError IO ()
runPrintGenesisHash genFp = do
    gen <- firstExceptT ByronCmdGenesisError $ readGenesis genFp
    liftIO . putTextLn $ formatter gen
  where
    formatter :: (a, Genesis.GenesisHash)-> Text
    formatter = F.sformat Crypto.hashHexF . Genesis.unGenesisHash . snd

runPrintSigningKeyAddress :: CardanoEra -> Network -> SigningKeyFile -> ExceptT ByronClientCmdError IO ()
runPrintSigningKeyAddress era network skF = do
  sK <- firstExceptT ByronCmdKeyFailure $ readEraSigningKey era skF
  let sKeyAddress = prettyAddress
                  . Common.makeVerKeyAddress (toByronNetworkMagic network)
                  . Crypto.toVerification
                  $ sK
  liftIO $ putTextLn sKeyAddress

runKeygen :: CardanoEra -> NewSigningKeyFile -> PasswordRequirement -> ExceptT ByronClientCmdError IO ()
runKeygen era (NewSigningKeyFile skF) passReq = do
  pPhrase <- liftIO $ getPassphrase ("Enter password to encrypt '" <> skF <> "': ") passReq
  sK <- liftIO $ keygen pPhrase
  serDk <- hoistEither . first ByronCmdDelegationError $ serialiseDelegateKey era sK
  firstExceptT ByronCmdHelpersError $ ensureNewFileLBS skF serDk

runToVerification :: CardanoEra -> SigningKeyFile -> NewVerificationKeyFile -> ExceptT ByronClientCmdError IO ()
runToVerification era skFp (NewVerificationKeyFile vkFp) = do
  sk <- firstExceptT ByronCmdKeyFailure $ readEraSigningKey era skFp
  let vKey = Builder.toLazyText . Crypto.formatFullVerificationKey $ Crypto.toVerification sk
  firstExceptT ByronCmdHelpersError $ ensureNewFile TL.writeFile vkFp vKey

runIssueDelegationCertificate
        :: ConfigYamlFilePath -> EpochNumber -> SigningKeyFile -> VerificationKeyFile -> NewCertificateFile
        -> ExceptT ByronClientCmdError IO ()
runIssueDelegationCertificate configFp epoch issuerSK delegateVK cert = do
  nc <- liftIO $ parseNodeConfigurationFP configFp
  vk <- firstExceptT ByronCmdKeyFailure $ readPaymentVerificationKey delegateVK
  sk <- firstExceptT ByronCmdKeyFailure $ readEraSigningKey (ncCardanoEra nc) issuerSK
  pmId <- firstExceptT ByronCmdGenesisError . readProtocolMagicId $ ncGenesisFile nc
  let byGenDelCert :: Delegation.Certificate
      byGenDelCert = issueByronGenesisDelegation pmId epoch sk vk
  sCert <- hoistEither . first ByronCmdDelegationError
             $ serialiseDelegationCert (ncCardanoEra nc) byGenDelCert
  firstExceptT ByronCmdHelpersError $ ensureNewFileLBS (nFp cert) sCert


runCheckDelegation
        :: ConfigYamlFilePath -> CertificateFile -> VerificationKeyFile -> VerificationKeyFile
        -> ExceptT ByronClientCmdError IO ()
runCheckDelegation configFp cert issuerVF delegateVF = do
  nc <- liftIO $ parseNodeConfigurationFP configFp
  issuerVK <- firstExceptT ByronCmdKeyFailure $ readPaymentVerificationKey issuerVF
  delegateVK <- firstExceptT ByronCmdKeyFailure $ readPaymentVerificationKey delegateVF
  pmId <- firstExceptT ByronCmdGenesisError $ readProtocolMagicId $ ncGenesisFile nc
  firstExceptT ByronCmdDelegationError $ checkByronGenesisDelegation cert pmId issuerVK delegateVK

runSubmitTx :: Network -> TxFile -> ExceptT ByronClientCmdError IO ()
runSubmitTx network fp =
  withIOManagerE $ \iomgr -> do
    tx <- firstExceptT ByronCmdTxError $ readByronTx fp
    firstExceptT ByronCmdTxError $ nodeSubmitTx iomgr network tx


runSpendGenesisUTxO
        :: ConfigYamlFilePath -> NewTxFile -> SigningKeyFile -> Common.Address -> NonEmpty TxOut
        -> ExceptT ByronClientCmdError IO ()
runSpendGenesisUTxO configFp (NewTxFile ctTx) ctKey genRichAddr outs = do
    nc <- liftIO $ parseNodeConfigurationFP configFp
    sk <- firstExceptT ByronCmdKeyFailure $ readEraSigningKey (ncCardanoEra nc) ctKey

    tx <- firstExceptT ByronCmdRealPBFTError $
            issueGenesisUTxOExpenditure nc genRichAddr outs sk
    firstExceptT ByronCmdHelpersError $ ensureNewFileLBS ctTx $ toCborTxAux tx

runSpendUTxO
        :: ConfigYamlFilePath -> NewTxFile -> SigningKeyFile -> NonEmpty TxIn -> NonEmpty TxOut
        -> ExceptT ByronClientCmdError IO ()
runSpendUTxO configFp (NewTxFile ctTx) ctKey ins outs = do
    nc <- liftIO $ parseNodeConfigurationFP configFp
    sk <- firstExceptT ByronCmdKeyFailure $ readEraSigningKey (ncCardanoEra nc) ctKey

    gTx <- firstExceptT ByronCmdRealPBFTError $
             issueUTxOExpenditure nc ins outs sk
    firstExceptT ByronCmdHelpersError . ensureNewFileLBS ctTx $ toCborTxAux gTx

withIOManagerE :: (IOManager -> ExceptT e IO a) -> ExceptT e IO a
withIOManagerE k = ExceptT $ withIOManager (runExceptT . k)
