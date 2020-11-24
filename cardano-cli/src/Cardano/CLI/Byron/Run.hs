module Cardano.CLI.Byron.Run
  ( ByronClientCmdError
  , renderByronClientCmdError
  , runByronClientCommand
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither)
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.IO as TL
import qualified Formatting as F

import qualified Cardano.Chain.Common as Common
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Chain.Slotting (EpochNumber)
import           Cardano.Chain.UTxO (TxIn, TxOut)

import qualified Cardano.Crypto.Hashing as Crypto
import qualified Cardano.Crypto.Signing as Crypto

import           Cardano.Api.Typed (NetworkId (..), toByronProtocolMagicId)
import qualified Cardano.Api.Typed as Typed

import           Cardano.CLI.Byron.Commands
import           Cardano.CLI.Byron.Delegation
import           Cardano.CLI.Byron.Genesis
import           Cardano.CLI.Byron.Key
import           Cardano.CLI.Byron.Query
import           Cardano.CLI.Byron.Tx
import           Cardano.CLI.Byron.UpdateProposal
import           Cardano.CLI.Byron.Vote

import           Cardano.CLI.Helpers
import           Cardano.CLI.Types

-- | Data type that encompasses all the possible errors of the
-- Byron client.
data ByronClientCmdError
  = ByronCmdDelegationError !ByronDelegationError
  | ByronCmdGenesisError !ByronGenesisError
  | ByronCmdHelpersError !HelpersError
  | ByronCmdKeyFailure !ByronKeyFailure
  | ByronCmdQueryError !ByronQueryError
  | ByronCmdTxError !ByronTxError
  | ByronCmdUpdateProposalError !ByronUpdateProposalError
  | ByronCmdVoteError !ByronVoteError
  deriving Show

renderByronClientCmdError :: ByronClientCmdError -> Text
renderByronClientCmdError err =
  case err of
    ByronCmdDelegationError e -> renderByronDelegationError e
    ByronCmdGenesisError e -> renderByronGenesisError e
    ByronCmdHelpersError e -> renderHelpersError e
    ByronCmdKeyFailure e -> renderByronKeyFailure e
    ByronCmdQueryError e -> renderByronQueryError e
    ByronCmdTxError e -> renderByronTxError e
    ByronCmdUpdateProposalError e -> renderByronUpdateProposalError e
    ByronCmdVoteError e -> renderByronVoteError e

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
    PrintSigningKeyAddress era networkid skF -> runPrintSigningKeyAddress era networkid skF
    Keygen era nskf passReq -> runKeygen era nskf passReq
    ToVerification era skFp nvkFp -> runToVerification era skFp nvkFp
    IssueDelegationCertificate nw era epoch issuerSK delVK cert ->
      runIssueDelegationCertificate nw era epoch issuerSK delVK cert
    CheckDelegation nw cert issuerVF delegateVF -> runCheckDelegation nw cert issuerVF delegateVF
    SubmitTx network fp -> runSubmitTx network fp
    SpendGenesisUTxO genFp nw era nftx ctKey genRichAddr outs ->
      runSpendGenesisUTxO genFp nw era nftx ctKey genRichAddr outs
    SpendUTxO nw era nftx ctKey ins outs ->
      runSpendUTxO nw era nftx ctKey ins outs


runNodeCmd :: NodeCmd -> ExceptT ByronClientCmdError IO ()
runNodeCmd (CreateVote nw sKey upPropFp voteBool outputFp) =
  firstExceptT ByronCmdVoteError $ runVoteCreation nw sKey upPropFp voteBool outputFp

runNodeCmd (SubmitUpdateProposal network proposalFp) =
    firstExceptT ByronCmdUpdateProposalError
      $ submitByronUpdateProposal network proposalFp

runNodeCmd (SubmitVote network voteFp) =
    firstExceptT ByronCmdVoteError $ submitByronVote network voteFp

runNodeCmd (UpdateProposal nw sKey pVer sVer sysTag insHash outputFp params) =
  firstExceptT ByronCmdUpdateProposalError
    $ runProposalCreation nw sKey pVer sVer sysTag insHash outputFp params

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
    genesis <- firstExceptT ByronCmdGenesisError $
                 readGenesis genFp dummyNetwork
    liftIO . putTextLn $ formatter genesis
  where
    -- For this purpose of getting the hash, it does not matter what network
    -- value we use here.
    dummyNetwork :: NetworkId
    dummyNetwork = Mainnet

    formatter :: Genesis.Config -> Text
    formatter = F.sformat Crypto.hashHexF
              . Genesis.unGenesisHash
              . Genesis.configGenesisHash

runPrintSigningKeyAddress :: CardanoEra -> NetworkId -> SigningKeyFile -> ExceptT ByronClientCmdError IO ()
runPrintSigningKeyAddress era networkid skF = do
  sK <- firstExceptT ByronCmdKeyFailure $ readEraSigningKey era skF
  let sKeyAddress = prettyAddress
                  . Common.makeVerKeyAddress (Typed.toByronNetworkMagic networkid)
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
  :: NetworkId
  -> CardanoEra
  -> EpochNumber
  -> SigningKeyFile
  -> VerificationKeyFile
  -> NewCertificateFile
  -> ExceptT ByronClientCmdError IO ()
runIssueDelegationCertificate nw era epoch issuerSK delegateVK cert = do
  vk <- firstExceptT ByronCmdKeyFailure $ readPaymentVerificationKey delegateVK
  sk <- firstExceptT ByronCmdKeyFailure $ readEraSigningKey era issuerSK
  let byGenDelCert :: Delegation.Certificate
      byGenDelCert = issueByronGenesisDelegation (toByronProtocolMagicId nw) epoch sk vk
      sCert        = serialiseDelegationCert byGenDelCert
  firstExceptT ByronCmdHelpersError $ ensureNewFileLBS (nFp cert) sCert


runCheckDelegation
  :: NetworkId
  -> CertificateFile
  -> VerificationKeyFile
  -> VerificationKeyFile
  -> ExceptT ByronClientCmdError IO ()
runCheckDelegation nw cert issuerVF delegateVF = do
  issuerVK <- firstExceptT ByronCmdKeyFailure $ readPaymentVerificationKey issuerVF
  delegateVK <- firstExceptT ByronCmdKeyFailure $ readPaymentVerificationKey delegateVF
  firstExceptT ByronCmdDelegationError $
    checkByronGenesisDelegation cert (toByronProtocolMagicId nw)
                                issuerVK delegateVK

runSubmitTx :: NetworkId -> TxFile -> ExceptT ByronClientCmdError IO ()
runSubmitTx network fp = do
    tx <- firstExceptT ByronCmdTxError $ readByronTx fp
    firstExceptT ByronCmdTxError $ nodeSubmitTx network tx


runSpendGenesisUTxO
  :: GenesisFile
  -> NetworkId
  -> CardanoEra
  -> NewTxFile
  -> SigningKeyFile
  -> Common.Address
  -> NonEmpty TxOut
  -> ExceptT ByronClientCmdError IO ()
runSpendGenesisUTxO genesisFile nw era (NewTxFile ctTx) ctKey genRichAddr outs = do
    genesis <- firstExceptT ByronCmdGenesisError $ readGenesis genesisFile nw
    sk <- firstExceptT ByronCmdKeyFailure $ readEraSigningKey era ctKey

    let tx = txSpendGenesisUTxOByronPBFT genesis nw sk genRichAddr outs
    firstExceptT ByronCmdHelpersError $ ensureNewFileLBS ctTx $ toCborTxAux tx

runSpendUTxO
  :: NetworkId
  -> CardanoEra
  -> NewTxFile
  -> SigningKeyFile
  -> NonEmpty TxIn
  -> NonEmpty TxOut
  -> ExceptT ByronClientCmdError IO ()
runSpendUTxO nw era (NewTxFile ctTx) ctKey ins outs = do
    sk <- firstExceptT ByronCmdKeyFailure $ readEraSigningKey era ctKey

    let gTx = txSpendUTxOByronPBFT nw sk ins outs
    firstExceptT ByronCmdHelpersError . ensureNewFileLBS ctTx $ toCborTxAux gTx
