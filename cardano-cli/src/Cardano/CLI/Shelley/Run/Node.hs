module Cardano.CLI.Shelley.Run.Node
  ( ShelleyNodeCmdError
  , renderShelleyNodeCmdError
  , runNodeCmd
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, newExceptT)

import qualified Data.Text as Text

import           Cardano.Api.Typed (AsType (..), Error (..), FileError,
                   KESPeriod, OperationalCertificateIssueCounter (..),
                   OperationalCertIssueError, TextEnvelopeError,
                   generateSigningKey, getVerificationKey,
                   issueOperationalCertificate, readFileTextEnvelope,
                   writeFileTextEnvelope)

import           Cardano.Api.TextView (TextViewTitle (..))
import           Cardano.Config.Types (SigningKeyFile(..))

import           Cardano.CLI.Shelley.Commands


data ShelleyNodeCmdError
  = ShelleyNodeReadFileError !(FileError TextEnvelopeError)
  | ShelleyNodeWriteFileError !(FileError ())
  | ShelleyNodeOperationalCertificateIssueError !OperationalCertIssueError
  deriving Show

renderShelleyNodeCmdError :: ShelleyNodeCmdError -> Text
renderShelleyNodeCmdError err =
  case err of
    ShelleyNodeReadFileError fileErr -> Text.pack (displayError fileErr)

    ShelleyNodeWriteFileError fileErr -> Text.pack (displayError fileErr)

    ShelleyNodeOperationalCertificateIssueError issueErr ->
      Text.pack (displayError issueErr)


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
runNodeKeyGenCold (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath)
                  (OpCertCounterFile ocertCtrPath) = do
    skey <- liftIO $ generateSigningKey AsStakePoolKey
    let vkey = getVerificationKey skey
    firstExceptT ShelleyNodeWriteFileError
      . newExceptT
      $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
    firstExceptT ShelleyNodeWriteFileError
      . newExceptT
      $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey
    firstExceptT ShelleyNodeWriteFileError
      . newExceptT
      $ writeFileTextEnvelope ocertCtrPath (Just ocertCtrDesc)
      -- TODO: Commenting this out as we're temporarily supporting the old op
      -- cert issue counter format.
      -- \$ OperationalCertificateIssueCounter initialCounter vkey
      $ OperationalCertificateIssueCounter initialCounter
  where
    skeyDesc, vkeyDesc, ocertCtrDesc :: TextViewTitle
    skeyDesc = TextViewTitle "Stake Pool Operator Signing Key"
    vkeyDesc = TextViewTitle "Stake Pool Operator Verification Key"
    ocertCtrDesc = TextViewTitle $ "Next certificate issue number: " <> show initialCounter

    initialCounter :: Natural
    initialCounter = 0


runNodeKeyGenKES :: VerificationKeyFile
                 -> SigningKeyFile
                 -> ExceptT ShelleyNodeCmdError IO ()
runNodeKeyGenKES (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) = do
    skey <- liftIO $ generateSigningKey AsKesKey
    let vkey = getVerificationKey skey
    firstExceptT ShelleyNodeWriteFileError
      . newExceptT
      $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
    firstExceptT ShelleyNodeWriteFileError
      . newExceptT
      $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey
  where
    skeyDesc, vkeyDesc :: TextViewTitle
    skeyDesc = TextViewTitle "KES Signing Key"
    vkeyDesc = TextViewTitle "KES Verification Key"


runNodeKeyGenVRF :: VerificationKeyFile -> SigningKeyFile
                 -> ExceptT ShelleyNodeCmdError IO ()
runNodeKeyGenVRF (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) = do
    skey <- liftIO $ generateSigningKey AsVrfKey
    let vkey = getVerificationKey skey
    firstExceptT ShelleyNodeWriteFileError
      . newExceptT
      $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
    firstExceptT ShelleyNodeWriteFileError
      . newExceptT
      $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey
  where
    skeyDesc, vkeyDesc :: TextViewTitle
    skeyDesc = TextViewTitle "VRF Signing Key"
    vkeyDesc = TextViewTitle "VRF Verification Key"

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
runNodeIssueOpCert (VerificationKeyFile vkeyKesPath)
                   (SigningKeyFile skeyStakePoolPath)
                   (OpCertCounterFile ocertCtrPath)
                   kesPeriod
                   (OutputFile certFile) = do

    ocertIssueCounter <- firstExceptT ShelleyNodeReadFileError
      . newExceptT
      $ readFileTextEnvelope AsOperationalCertificateIssueCounter ocertCtrPath

    verKeyKes <- firstExceptT ShelleyNodeReadFileError
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsKesKey) vkeyKesPath

    signKeyStakePool <- firstExceptT ShelleyNodeReadFileError
      . newExceptT
      $ readFileTextEnvelope (AsSigningKey AsStakePoolKey) skeyStakePoolPath

    (ocert, nextOcertCtr) <-
      firstExceptT ShelleyNodeOperationalCertificateIssueError
        . hoistEither
        $ issueOperationalCertificate
            verKeyKes
            signKeyStakePool
            kesPeriod
            ocertIssueCounter

    -- Write the counter first, to reduce the chance of ending up with
    -- a new cert but without updating the counter.
    firstExceptT ShelleyNodeWriteFileError
      . newExceptT
      $ writeFileTextEnvelope
        ocertCtrPath
        (Just $ ocertCtrDesc $ getCounter nextOcertCtr)
        nextOcertCtr

    firstExceptT ShelleyNodeWriteFileError
      . newExceptT
      $ writeFileTextEnvelope certFile Nothing ocert
  where
    getCounter :: OperationalCertificateIssueCounter -> Natural
    -- TODO: Commenting this out as we're temporarily supporting the old op
    -- cert issue counter format.
    -- getCounter (OperationalCertificateIssueCounter n _) = n
    getCounter (OperationalCertificateIssueCounter n) = n

    ocertCtrDesc :: Natural -> TextViewTitle
    ocertCtrDesc n = TextViewTitle $ "Next certificate issue number: " <> show n
