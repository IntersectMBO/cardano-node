module Cardano.CLI.Shelley.Run.Node
  ( ShelleyNodeCmdError
  , runNodeCmd
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)

import           Cardano.Api (ApiError, GenesisVerificationKey(..), EpochNo,
                  ShelleyPParamsUpdate, Update (..), createShelleyUpdateProposal,
                  hashKey, readGenesisVerificationKey, writeUpdate)

import           Cardano.Config.Shelley.ColdKeys hiding (writeSigningKey)
import           Cardano.Config.Shelley.KES
import           Cardano.Config.Shelley.OCert
import           Cardano.Config.Shelley.VRF

import           Cardano.Config.Types (SigningKeyFile(..))

import           Cardano.CLI.Shelley.Commands
import           Cardano.CLI.Shelley.KeyGen

data ShelleyNodeCmdError
  = ShelleyNodeCmdKeyError !KeyError
  | ShelleyNodeCmdOperationalCertError !OperationalCertError
  | ShelleyNodeCmdCardanoApiError !ApiError
  | ShelleyNodeCmdKESError !KESError
  | ShelleyNodeCmdVRFError !VRFError
  | ShelleyNodeCmdKeyGenError !ShelleyKeyGenError
  deriving Show


runNodeCmd :: NodeCmd -> ExceptT ShelleyNodeCmdError IO ()
runNodeCmd (NodeKeyGenCold vk sk ctr) = runNodeKeyGenCold vk sk ctr
runNodeCmd (NodeKeyGenKES  vk sk)     = runNodeKeyGenKES  vk sk
runNodeCmd (NodeKeyGenVRF  vk sk)     = runNodeKeyGenVRF  vk sk
runNodeCmd (NodeIssueOpCert vk sk ctr p out) =
  runNodeIssueOpCert vk sk ctr p out
runNodeCmd (NodeUpdateProposal out eNo genVKeys ppUp) = runNodeUpdateProposal out eNo genVKeys ppUp



--
-- Node command implementations
--

runNodeKeyGenCold :: VerificationKeyFile
                  -> SigningKeyFile
                  -> OpCertCounterFile
                  -> ExceptT ShelleyNodeCmdError IO ()
runNodeKeyGenCold vkeyPath skeyPath (OpCertCounterFile ocertCtrPath) = do
    firstExceptT ShelleyNodeCmdKeyGenError $ runColdKeyGen (OperatorKey StakePoolOperatorKey) vkeyPath skeyPath
    firstExceptT ShelleyNodeCmdOperationalCertError $
      writeOperationalCertIssueCounter ocertCtrPath initialCounter
  where
    initialCounter = 0


runNodeKeyGenKES :: VerificationKeyFile
                 -> SigningKeyFile
                 -> ExceptT ShelleyNodeCmdError IO ()
runNodeKeyGenKES (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) =
    firstExceptT ShelleyNodeCmdKESError $ do
      (vkey, skey) <- liftIO $ genKESKeyPair
      writeKESVerKey     vkeyPath vkey
      writeKESSigningKey skeyPath skey


runNodeKeyGenVRF :: VerificationKeyFile -> SigningKeyFile
                 -> ExceptT ShelleyNodeCmdError IO ()
runNodeKeyGenVRF (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) =
    firstExceptT ShelleyNodeCmdVRFError $ do
      --FIXME: genVRFKeyPair genKESKeyPair results are in an inconsistent order
      (skey, vkey) <- liftIO genVRFKeyPair
      writeVRFVerKey     vkeyPath vkey
      writeVRFSigningKey skeyPath skey


runNodeIssueOpCert :: VerificationKeyFile
                   -- ^ This is the hot KES verification key.
                   -> SigningKeyFile
                   -- ^ This is the cold signing key.
                   -> OpCertCounterFile
                   -- ^ Counter that establishes the precedence
                   -- of the operational certificate.
                   -> KESPeriod
                   -- ^ Start of the validity period for this certificate.
                   -> OutputFile
                   -> ExceptT ShelleyNodeCmdError IO ()
runNodeIssueOpCert (VerificationKeyFile vkeyKESPath)
                   (SigningKeyFile skeyPath)
                   (OpCertCounterFile ocertCtrPath)
                   kesPeriod
                   (OutputFile certFile) = do
    issueNumber <- firstExceptT ShelleyNodeCmdOperationalCertError $
      readOperationalCertIssueCounter ocertCtrPath

    verKeyKes <- firstExceptT ShelleyNodeCmdKESError $
      readKESVerKey vkeyKESPath

    signKey <- firstExceptT ShelleyNodeCmdKeyError $
      readSigningKey (OperatorKey StakePoolOperatorKey) skeyPath

    let cert = signOperationalCertificate
                 verKeyKes signKey
                 issueNumber kesPeriod
        vkey = deriveVerKey signKey

    firstExceptT ShelleyNodeCmdOperationalCertError $ do
      -- Write the counter first, to reduce the chance of ending up with
      -- a new cert but without updating the counter.
      writeOperationalCertIssueCounter ocertCtrPath (succ issueNumber)
      writeOperationalCert certFile cert vkey

runNodeUpdateProposal
  :: OutputFile
  -> EpochNo
  -> [VerificationKeyFile]
  -- ^ Genesis verification keys
  -> ShelleyPParamsUpdate
  -> ExceptT ShelleyNodeCmdError IO ()
runNodeUpdateProposal (OutputFile upFile) eNo genVerKeyFiles upPprams = do
  genVKeys <- mapM
                (\(VerificationKeyFile fp) -> do
                  GenesisVerificationKeyShelley gvk <-
                    firstExceptT ShelleyNodeCmdCardanoApiError $ newExceptT $ readGenesisVerificationKey fp
                  pure gvk
                )
                genVerKeyFiles
  let genKeyHashes = map hashKey genVKeys
      upProp = ShelleyUpdate $ createShelleyUpdateProposal eNo genKeyHashes upPprams
  firstExceptT ShelleyNodeCmdCardanoApiError . newExceptT $ writeUpdate upFile upProp
