module Cardano.CLI.Shelley.Run.Node
  ( ShelleyNodeCmdError
  , renderShelleyNodeCmdError
  , runNodeCmd
  ) where

import           Cardano.Prelude
import           Prelude (id)

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, newExceptT)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text

import           Cardano.Api.Typed

import           Cardano.Api.TextView (TextViewDescription (..))

import           Cardano.CLI.Shelley.Commands
import           Cardano.CLI.Types (SigningKeyFile (..), VerificationKeyFile (..))


data ShelleyNodeCmdError
  = ShelleyNodeCmdReadFileError !(FileError TextEnvelopeError)
  | ShelleyNodeCmdWriteFileError !(FileError ())
  | ShelleyNodeCmdOperationalCertificateIssueError !OperationalCertIssueError
  deriving Show

renderShelleyNodeCmdError :: ShelleyNodeCmdError -> Text
renderShelleyNodeCmdError err =
  case err of
    ShelleyNodeCmdReadFileError fileErr -> Text.pack (displayError fileErr)

    ShelleyNodeCmdWriteFileError fileErr -> Text.pack (displayError fileErr)

    ShelleyNodeCmdOperationalCertificateIssueError issueErr ->
      Text.pack (displayError issueErr)


runNodeCmd :: NodeCmd -> ExceptT ShelleyNodeCmdError IO ()
runNodeCmd (NodeKeyGenCold vk sk ctr) = runNodeKeyGenCold vk sk ctr
runNodeCmd (NodeKeyGenKES  vk sk)     = runNodeKeyGenKES  vk sk
runNodeCmd (NodeKeyGenVRF  vk sk)     = runNodeKeyGenVRF  vk sk
runNodeCmd (NodeKeyHashVRF vk mOutFp) = runNodeKeyHashVRF vk mOutFp
runNodeCmd (NodeNewCounter vk ctr out) = runNodeNewCounter vk ctr out
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
    firstExceptT ShelleyNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
    firstExceptT ShelleyNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey
    firstExceptT ShelleyNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope ocertCtrPath (Just ocertCtrDesc)
      $ OperationalCertificateIssueCounter initialCounter vkey
  where
    skeyDesc, vkeyDesc, ocertCtrDesc :: TextViewDescription
    skeyDesc = TextViewDescription "Stake Pool Operator Signing Key"
    vkeyDesc = TextViewDescription "Stake Pool Operator Verification Key"
    ocertCtrDesc = TextViewDescription $ "Next certificate issue number: " <> BS.pack (show initialCounter)

    initialCounter :: Word64
    initialCounter = 0


runNodeKeyGenKES :: VerificationKeyFile
                 -> SigningKeyFile
                 -> ExceptT ShelleyNodeCmdError IO ()
runNodeKeyGenKES (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) = do
    skey <- liftIO $ generateSigningKey AsKesKey
    let vkey = getVerificationKey skey
    firstExceptT ShelleyNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
    firstExceptT ShelleyNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey
  where
    skeyDesc, vkeyDesc :: TextViewDescription
    skeyDesc = TextViewDescription "KES Signing Key"
    vkeyDesc = TextViewDescription "KES Verification Key"


runNodeKeyGenVRF :: VerificationKeyFile -> SigningKeyFile
                 -> ExceptT ShelleyNodeCmdError IO ()
runNodeKeyGenVRF (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) = do
    skey <- liftIO $ generateSigningKey AsVrfKey
    let vkey = getVerificationKey skey
    firstExceptT ShelleyNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
    firstExceptT ShelleyNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey
  where
    skeyDesc, vkeyDesc :: TextViewDescription
    skeyDesc = TextViewDescription "VRF Signing Key"
    vkeyDesc = TextViewDescription "VRF Verification Key"

runNodeKeyHashVRF :: VerificationKeyFile -> Maybe OutputFile
                  -> ExceptT ShelleyNodeCmdError IO ()
runNodeKeyHashVRF (VerificationKeyFile vkeyPath) mOutputFp = do
  vkey <- firstExceptT ShelleyNodeCmdReadFileError
    . newExceptT
    $ readFileTextEnvelope (AsVerificationKey AsVrfKey) vkeyPath

  let hexKeyHash = serialiseToRawBytesHex (verificationKeyHash vkey)

  case mOutputFp of
    Just (OutputFile fpath) -> liftIO $ BS.writeFile fpath hexKeyHash
    Nothing -> liftIO $ BS.putStrLn hexKeyHash


runNodeNewCounter :: VerificationKeyFile
                  -> Word
                  -> OpCertCounterFile
                  -> ExceptT ShelleyNodeCmdError IO ()
runNodeNewCounter (VerificationKeyFile vkeyPath) counter
                  (OpCertCounterFile ocertCtrPath) = do

    vkey <- firstExceptT ShelleyNodeCmdReadFileError . newExceptT $
              readFileTextEnvelopeAnyOf
                [ FromSomeType (AsVerificationKey AsStakePoolKey) id
                , FromSomeType (AsVerificationKey AsGenesisDelegateKey)
                               castVerificationKey
                ]
                vkeyPath

    let ocertIssueCounter =
          OperationalCertificateIssueCounter (fromIntegral counter) vkey

    firstExceptT ShelleyNodeCmdWriteFileError . newExceptT $
      writeFileTextEnvelope ocertCtrPath Nothing ocertIssueCounter


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

    ocertIssueCounter <- firstExceptT ShelleyNodeCmdReadFileError
      . newExceptT
      $ readFileTextEnvelope AsOperationalCertificateIssueCounter ocertCtrPath

    verKeyKes <- firstExceptT ShelleyNodeCmdReadFileError
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsKesKey) vkeyKesPath

    signKey <- firstExceptT ShelleyNodeCmdReadFileError
      . newExceptT
      $ readFileTextEnvelopeAnyOf possibleBlockIssuers skeyStakePoolPath

    (ocert, nextOcertCtr) <-
      firstExceptT ShelleyNodeCmdOperationalCertificateIssueError
        . hoistEither
        $ issueOperationalCertificate
            verKeyKes
            signKey
            kesPeriod
            ocertIssueCounter

    -- Write the counter first, to reduce the chance of ending up with
    -- a new cert but without updating the counter.
    firstExceptT ShelleyNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope
        ocertCtrPath
        (Just $ ocertCtrDesc $ getCounter nextOcertCtr)
        nextOcertCtr

    firstExceptT ShelleyNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope certFile Nothing ocert
  where
    getCounter :: OperationalCertificateIssueCounter -> Word64
    getCounter (OperationalCertificateIssueCounter n _) = n

    ocertCtrDesc :: Word64 -> TextViewDescription
    ocertCtrDesc n = TextViewDescription $ "Next certificate issue number: " <> BS.pack (show n)

    possibleBlockIssuers
      :: [FromSomeType HasTextEnvelope
                       (Either (SigningKey StakePoolKey)
                               (SigningKey GenesisDelegateExtendedKey))]
    possibleBlockIssuers =
      [ FromSomeType (AsSigningKey AsStakePoolKey)        Left
      , FromSomeType (AsSigningKey AsGenesisDelegateKey) (Left . castSigningKey)
      , FromSomeType (AsSigningKey AsGenesisDelegateExtendedKey) Right
      ]
