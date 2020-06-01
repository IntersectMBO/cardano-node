module Cardano.CLI.Shelley.Run.Node
  ( ShelleyNodeCmdError
  , renderShelleyNodeCmdError
  , runNodeCmd
  ) where

import           Cardano.Prelude

import qualified Data.Text as Text

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT, hoistEither)

import           Cardano.Api.Typed as Api

import           Cardano.Config.TextView (textShow)
import           Cardano.Config.Types (SigningKeyFile(..))

import           Cardano.CLI.Shelley.Commands


data ShelleyNodeCmdError
  = ShelleyNodeReadStakePoolSignKeyError !FilePath !(Api.FileError Api.TextEnvelopeError)
  | ShelleyNodeOperationalCertIssueError !Api.OperationalCertIssueError
  | ShelleyNodeWriteOpCertIssueCounterError !FilePath !(Api.FileError ())
  | ShelleyNodeWriteOperationalCertError !FilePath !(Api.FileError ())
  | ShelleyNodeReadKESVerKeyError !FilePath !(Api.FileError Api.TextEnvelopeError)
  | ShelleyNodeWriteKESVerKeyError !FilePath !(Api.FileError ())
  | ShelleyNodeWriteKESSignKeyError !FilePath !(Api.FileError ())
  | ShelleyNodeWriteVRFSignKeyError !FilePath !(Api.FileError ())
  | ShelleyNodeWriteVRFVerKeyError !FilePath !(Api.FileError ())
  | ShelleyNodeReadOperationalCertCounterError !FilePath !(Api.FileError Api.TextEnvelopeError)
  | ShelleyNodeOperatorKeyGenError !FilePath !FilePath !(FileError ())
  -- TODO: Create a module for the shelley update proposal stuff and
  -- create a custom error type there i.e ShelleyUpdateProposalError
  deriving Show

renderShelleyNodeCmdError :: ShelleyNodeCmdError -> Text
renderShelleyNodeCmdError err =
  case err of
    ShelleyNodeOperatorKeyGenError vkFp skFp keyErr ->
      "Error generating the operator key pair at: "
        <> textShow vkFp
        <> " "
        <> textShow skFp
        <> " Error: " <> Text.pack (displayError keyErr)

    ShelleyNodeOperationalCertIssueError opCertErr ->
        "Error while issuing the operational certificate: "
     <> Text.pack (displayError opCertErr)


    ShelleyNodeReadOperationalCertCounterError fp opCertErr ->
        "Error reading the operational certificate issue counter at: "
     <> textShow fp <> " Error: " <> Text.pack (displayError opCertErr)

    ShelleyNodeReadStakePoolSignKeyError fp keyErr ->
        "Error reading the stake pool operator signing key at: "
     <> textShow fp <> " Error: " <> Text.pack (displayError keyErr)

    ShelleyNodeReadKESVerKeyError fp kesErr ->
        "Error reading the KES verification key at: "
     <> textShow fp <> " Error: " <> Text.pack (displayError kesErr)

    ShelleyNodeWriteOpCertIssueCounterError fp opCertErr ->
        "Error writing the operational certificate issue counter at: "
     <> textShow fp <> " Error: " <> Text.pack (displayError opCertErr)

    ShelleyNodeWriteOperationalCertError fp opCertErr ->
        "Error writing the operational certificate at: "
     <> textShow fp <> " Error: " <> Text.pack (displayError opCertErr)

    ShelleyNodeWriteKESVerKeyError fp kesErr ->
        "Error writing the KES verification key at: "
     <> textShow fp <> " Error: " <> Text.pack (displayError kesErr)

    ShelleyNodeWriteKESSignKeyError fp kesErr ->
        "Error writing the KES signing key at: "
     <> textShow fp <> " Error: " <> Text.pack (displayError kesErr)

    ShelleyNodeWriteVRFVerKeyError fp vrfErr ->
        "Error writing the VRF verification key at: "
     <> textShow fp <> " Error: " <> Text.pack (displayError vrfErr)

    ShelleyNodeWriteVRFSignKeyError fp vrfErr ->
        "Error writing the VRF signing key at: "
     <> textShow fp <> " Error: " <> Text.pack (displayError vrfErr)


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
runNodeKeyGenCold (VerificationKeyFile vkFile) (SigningKeyFile skFile)
                  (OpCertCounterFile ocertCtrPath) = do
    skey <- liftIO $ Api.generateSigningKey Api.AsStakePoolKey
    let vkey = getVerificationKey skey
        initialCounter = OperationalCertificateIssueCounter 0 vkey
    firstExceptT (ShelleyNodeOperatorKeyGenError vkFile skFile) $ newExceptT $
      Api.writeFileTextEnvelope vkFile Nothing vkey
    firstExceptT (ShelleyNodeOperatorKeyGenError vkFile skFile) $ newExceptT $
      Api.writeFileTextEnvelope skFile Nothing skey
    firstExceptT (ShelleyNodeWriteOpCertIssueCounterError ocertCtrPath) $ newExceptT $
      Api.writeFileTextEnvelope ocertCtrPath Nothing initialCounter


runNodeKeyGenKES :: VerificationKeyFile
                 -> SigningKeyFile
                 -> ExceptT ShelleyNodeCmdError IO ()
runNodeKeyGenKES (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) = do
    skey <- liftIO $ Api.generateSigningKey Api.AsKesKey
    let vkey = Api.getVerificationKey skey
    firstExceptT (ShelleyNodeWriteKESVerKeyError vkeyPath) $ newExceptT $
      Api.writeFileTextEnvelope vkeyPath Nothing vkey
    firstExceptT (ShelleyNodeWriteKESSignKeyError skeyPath) $ newExceptT $
      Api.writeFileTextEnvelope skeyPath Nothing skey


runNodeKeyGenVRF :: VerificationKeyFile -> SigningKeyFile
                 -> ExceptT ShelleyNodeCmdError IO ()
runNodeKeyGenVRF (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) = do
    skey <- liftIO $ Api.generateSigningKey Api.AsVrfKey
    let vkey = Api.getVerificationKey skey
    firstExceptT (ShelleyNodeWriteVRFVerKeyError vkeyPath) $ newExceptT $
      Api.writeFileTextEnvelope vkeyPath Nothing vkey
    firstExceptT (ShelleyNodeWriteVRFSignKeyError skeyPath) $ newExceptT $
      Api.writeFileTextEnvelope skeyPath Nothing skey


runNodeIssueOpCert :: VerificationKeyFile
                   -- ^ This is the hot KES verification key.
                   -> SigningKeyFile
                   -- ^ This is the cold signing key.
                   -> OpCertCounterFile
                   -- ^ Counter that establishes the precedence
                   -- of the operational certificate.
                   -> Api.KESPeriod
                   -- ^ Start of the validity period for this certificate.
                   -> OutputFile
                   -> ExceptT ShelleyNodeCmdError IO ()
runNodeIssueOpCert (VerificationKeyFile vkeyKESPath)
                   (SigningKeyFile skeyPath)
                   (OpCertCounterFile ocertCtrPath)
                   kesPeriod
                   (OutputFile certFile) = do

    counter <- firstExceptT (ShelleyNodeReadOperationalCertCounterError ocertCtrPath) $ newExceptT $
      Api.readFileTextEnvelope Api.AsOperationalCertificateIssueCounter ocertCtrPath

    verKeyKes <- firstExceptT (ShelleyNodeReadKESVerKeyError vkeyKESPath) $ newExceptT $
      Api.readFileTextEnvelope (Api.AsVerificationKey Api.AsKesKey) vkeyKESPath

    signKey <- firstExceptT (ShelleyNodeReadStakePoolSignKeyError skeyPath) $ newExceptT $
      Api.readFileTextEnvelope (Api.AsSigningKey Api.AsStakePoolKey) skeyPath

    (opcert, counter') <- firstExceptT ShelleyNodeOperationalCertIssueError $
      hoistEither $
      Api.issueOperationalCertificate
        verKeyKes
        signKey
        kesPeriod
        counter

    firstExceptT (ShelleyNodeWriteOpCertIssueCounterError ocertCtrPath) $ newExceptT $
      -- Write the counter first, to reduce the chance of ending up with
      -- a new cert but without updating the counter.
      Api.writeFileTextEnvelope ocertCtrPath Nothing counter'

    firstExceptT (ShelleyNodeWriteOperationalCertError certFile) $ newExceptT $
      Api.writeFileTextEnvelope certFile Nothing opcert

