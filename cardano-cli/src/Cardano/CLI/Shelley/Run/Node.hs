module Cardano.CLI.Shelley.Run.Node
  ( runNodeCmd
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)

import           Cardano.Api (GenesisVerificationKey(..), EpochNo,
                  ShelleyPParamsUpdate, Update (..), createShelleyUpdateProposal,
                  hashKey, readGenesisVerificationKey, writeUpdate)

import           Cardano.Config.Shelley.ColdKeys hiding (writeSigningKey)
import           Cardano.Config.Shelley.KES
import           Cardano.Config.Shelley.OCert
import           Cardano.Config.Shelley.VRF

import           Cardano.Config.Types (SigningKeyFile(..))

import           Cardano.CLI.Errors (CliError(..))
import           Cardano.CLI.Shelley.Commands
import           Cardano.CLI.Shelley.KeyGen

runNodeCmd :: NodeCmd -> ExceptT CliError IO ()
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
                  -> ExceptT CliError IO ()
runNodeKeyGenCold vkeyPath skeyPath (OpCertCounterFile ocertCtrPath) = do
    runColdKeyGen (OperatorKey StakePoolOperatorKey) vkeyPath skeyPath
    firstExceptT OperationalCertError $
      writeOperationalCertIssueCounter ocertCtrPath initialCounter
  where
    initialCounter = 0


runNodeKeyGenKES :: VerificationKeyFile
                 -> SigningKeyFile
                 -> ExceptT CliError IO ()
runNodeKeyGenKES (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) =
    firstExceptT KESCliError $ do
      (vkey, skey) <- liftIO $ genKESKeyPair
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
                   -- ^ This is the hot KES verification key.
                   -> SigningKeyFile
                   -- ^ This is the cold signing key.
                   -> OpCertCounterFile
                   -- ^ Counter that establishes the precedence
                   -- of the operational certificate.
                   -> KESPeriod
                   -- ^ Start of the validity period for this certificate.
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

runNodeUpdateProposal
  :: OutputFile
  -> EpochNo
  -> [VerificationKeyFile]
  -- ^ Genesis verification keys
  -> ShelleyPParamsUpdate
  -> ExceptT CliError IO ()
runNodeUpdateProposal (OutputFile upFile) eNo genVerKeyFiles upPprams = do
  genVKeys <- mapM
                (\(VerificationKeyFile fp) -> do
                  GenesisVerificationKeyShelley gvk <-
                    firstExceptT CardanoApiError $ newExceptT $ readGenesisVerificationKey fp
                  pure gvk
                )
                genVerKeyFiles
  let genKeyHashes = map hashKey genVKeys
      upProp = ShelleyUpdate $ createShelleyUpdateProposal eNo genKeyHashes upPprams
  firstExceptT CardanoApiError . newExceptT $ writeUpdate upFile upProp
