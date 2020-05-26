module Cardano.CLI.Shelley.Run.Node
  ( ShelleyNodeCmdError
  , renderShelleyNodeCmdError
  , runNodeCmd
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)

import           Cardano.Config.Shelley.ColdKeys hiding (writeSigningKey)
import           Cardano.Config.Shelley.KES
import           Cardano.Config.Shelley.OCert
import           Cardano.Config.Shelley.VRF

import           Cardano.Config.Shelley.ColdKeys (KeyError)
import           Cardano.Config.TextView (textShow)
import           Cardano.Config.Types (SigningKeyFile(..))

import           Cardano.CLI.Shelley.Commands
import           Cardano.CLI.Shelley.KeyGen


data ShelleyNodeCmdError
  = ShelleyNodeReadStakePoolSignKeyError !FilePath !KeyError
  | ShelleyNodeWriteOpCertIssueCounterError !FilePath !OperationalCertError
  | ShelleyNodeWriteOperationalCertError !FilePath !OperationalCertError
  | ShelleyNodeReadKESVerKeyError !FilePath !KESError
  | ShelleyNodeWriteKESVerKeyError !FilePath !KESError
  | ShelleyNodeWriteKESSignKeyError !FilePath !KESError
  | ShelleyNodeWriteVRFSignKeyError !FilePath !VRFError
  | ShelleyNodeWriteVRFVerKeyError !FilePath !VRFError
  | ShelleyNodeReadOperationalCertCounterError !FilePath !OperationalCertError
  | ShelleyNodeOperatorKeyGenError !VerificationKeyFile !SigningKeyFile !ShelleyKeyGenError
  -- TODO: Create a module for the shelley update proposal stuff and
  -- create a custom error type there i.e ShelleyUpdateProposalError
  deriving Show

renderShelleyNodeCmdError :: ShelleyNodeCmdError -> Text
renderShelleyNodeCmdError err =
  case err of
    ShelleyNodeOperatorKeyGenError (VerificationKeyFile vkFp) (SigningKeyFile skFp) shellKeyGenErr ->
      "Error generating the operator key pair at: "
        <> textShow vkFp
        <> " "
        <> textShow skFp
        <> " Error: " <> renderShelleyKeyGenError shellKeyGenErr
    ShelleyNodeReadOperationalCertCounterError fp opCertErr ->
      "Error reading the operational certificate issue counter at: " <> textShow fp <> " Error: " <> renderOperationalCertError opCertErr

    ShelleyNodeReadStakePoolSignKeyError fp keyErr ->
      "Error reading the stake pool operator signing key at: " <> textShow fp <> " Error: " <> renderKeyError keyErr

    ShelleyNodeReadKESVerKeyError fp kesErr ->
      "Error reading the KES verification key at: " <> textShow fp <> " Error: " <> renderKESError kesErr

    ShelleyNodeWriteOpCertIssueCounterError fp opCertErr ->
      "Error writing the operational certificate issue counter at: " <> textShow fp <> " Error: " <> renderOperationalCertError opCertErr

    ShelleyNodeWriteOperationalCertError fp opCertErr ->
      "Error writing the operational certificate at: " <> textShow fp <> " Error: " <> renderOperationalCertError opCertErr

    ShelleyNodeWriteKESVerKeyError fp kesErr ->
      "Error writing the KES verification key at: " <> textShow fp <> " Error: " <> renderKESError kesErr

    ShelleyNodeWriteKESSignKeyError fp kesErr ->
      "Error writing the KES signing key at: " <> textShow fp <> " Error: " <> renderKESError kesErr

    ShelleyNodeWriteVRFVerKeyError fp vrfErr ->
      "Error writing the VRF verification key at: " <> textShow fp <> " Error: " <> renderVRFError vrfErr

    ShelleyNodeWriteVRFSignKeyError fp vrfErr ->
      "Error writing the VRF signing key at: " <> textShow fp <> " Error: " <> renderVRFError vrfErr


runNodeCmd :: NodeCmd -> ExceptT ShelleyNodeCmdError IO ()
runNodeCmd (NodeKeyGenCold vk sk ctr) = runNodeKeyGenCold vk sk ctr
runNodeCmd (NodeKeyGenKES  vk sk)     = runNodeKeyGenKES  vk sk
runNodeCmd (NodeKeyGenVRF  vk sk)     = runNodeKeyGenVRF  vk sk
runNodeCmd (NodeIssueOpCert vk sk ctr p out) =
  runNodeIssueOpCert vk sk ctr p out



--
-- Node command implementations
--

runNodeKeyGenCold :: VerificationKeyFile
                  -> SigningKeyFile
                  -> OpCertCounterFile
                  -> ExceptT ShelleyNodeCmdError IO ()
runNodeKeyGenCold vkFile skFile (OpCertCounterFile ocertCtrPath) = do
    firstExceptT (ShelleyNodeOperatorKeyGenError vkFile skFile) $
      runColdKeyGen (OperatorKey StakePoolOperatorKey) vkFile skFile
    firstExceptT (ShelleyNodeWriteOpCertIssueCounterError ocertCtrPath) $
      writeOperationalCertIssueCounter ocertCtrPath initialCounter
  where
    initialCounter = 0


runNodeKeyGenKES :: VerificationKeyFile
                 -> SigningKeyFile
                 -> ExceptT ShelleyNodeCmdError IO ()
runNodeKeyGenKES (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) = do
  (vkey, skey) <- liftIO $ genKESKeyPair
  firstExceptT (ShelleyNodeWriteKESVerKeyError vkeyPath) $ writeKESVerKey vkeyPath vkey
  firstExceptT (ShelleyNodeWriteKESSignKeyError skeyPath) $ writeKESSigningKey skeyPath skey


runNodeKeyGenVRF :: VerificationKeyFile -> SigningKeyFile
                 -> ExceptT ShelleyNodeCmdError IO ()
runNodeKeyGenVRF (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) = do
      --FIXME: genVRFKeyPair genKESKeyPair results are in an inconsistent order
      (skey, vkey) <- liftIO genVRFKeyPair
      firstExceptT (ShelleyNodeWriteVRFVerKeyError vkeyPath) $ writeVRFVerKey vkeyPath vkey
      firstExceptT (ShelleyNodeWriteVRFSignKeyError skeyPath) $ writeVRFSigningKey skeyPath skey


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
    issueNumber <- firstExceptT (ShelleyNodeReadOperationalCertCounterError ocertCtrPath) $
      readOperationalCertIssueCounter ocertCtrPath

    verKeyKes <- firstExceptT (ShelleyNodeReadKESVerKeyError vkeyKESPath) $
      readKESVerKey vkeyKESPath

    signKey <- firstExceptT (ShelleyNodeReadStakePoolSignKeyError skeyPath) $
      readSigningKey (OperatorKey StakePoolOperatorKey) skeyPath

    let cert = signOperationalCertificate
                 verKeyKes signKey
                 issueNumber kesPeriod
        vkey = deriveVerKey signKey

    firstExceptT (ShelleyNodeWriteOpCertIssueCounterError ocertCtrPath) $
      -- Write the counter first, to reduce the chance of ending up with
      -- a new cert but without updating the counter.
      writeOperationalCertIssueCounter ocertCtrPath (succ issueNumber)
    firstExceptT (ShelleyNodeWriteOperationalCertError certFile) $ writeOperationalCert certFile cert vkey
