module Cardano.CLI.Shelley.Run.Node
  ( ShelleyNodeCmdError(ShelleyNodeCmdReadFileError)
  , renderShelleyNodeCmdError
  , runNodeCmd
  , runNodeIssueOpCert
  , runNodeKeyGenCold
  , runNodeKeyGenKES
  , runNodeKeyGenVRF
  , readColdVerificationKeyOrFile
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, newExceptT)
import qualified Data.ByteString.Char8 as BS
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word (Word64)

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Shelley.Commands
import           Cardano.CLI.Shelley.Key (VerificationKeyOrFile, readVerificationKeyOrFile)
import           Cardano.CLI.Types (KeyOutputFormat (..), SigningKeyFile (..),
                   VerificationKeyFile (..))

{- HLINT ignore "Reduce duplication" -}

data ShelleyNodeCmdError
  = ShelleyNodeCmdReadFileError !(FileError TextEnvelopeError)
  | ShelleyNodeCmdReadKeyFileError !(FileError InputDecodeError)
  | ShelleyNodeCmdWriteFileError !(FileError ())
  | ShelleyNodeCmdOperationalCertificateIssueError !OperationalCertIssueError
  | ShelleyNodeCmdVrfSigningKeyCreationError
      FilePath
      -- ^ Target path
      FilePath
      -- ^ Temp path
  deriving Show

renderShelleyNodeCmdError :: ShelleyNodeCmdError -> Text
renderShelleyNodeCmdError err =
  case err of
    ShelleyNodeCmdVrfSigningKeyCreationError targetPath tempPath ->
      Text.pack $ "Error creating VRF signing key file. Target path: " <> targetPath
      <> " Temporary path: " <> tempPath

    ShelleyNodeCmdReadFileError fileErr -> Text.pack (displayError fileErr)

    ShelleyNodeCmdReadKeyFileError fileErr -> Text.pack (displayError fileErr)

    ShelleyNodeCmdWriteFileError fileErr -> Text.pack (displayError fileErr)

    ShelleyNodeCmdOperationalCertificateIssueError issueErr ->
      Text.pack (displayError issueErr)


runNodeCmd :: NodeCmd -> ExceptT ShelleyNodeCmdError IO ()
runNodeCmd (NodeKeyGenCold fmt vk sk ctr) = runNodeKeyGenCold fmt vk sk ctr
runNodeCmd (NodeKeyGenKES  fmt vk sk)     = runNodeKeyGenKES fmt vk sk
runNodeCmd (NodeKeyGenVRF  fmt vk sk)     = runNodeKeyGenVRF fmt vk sk
runNodeCmd (NodeKeyHashVRF vk mOutFp) = runNodeKeyHashVRF vk mOutFp
runNodeCmd (NodeNewCounter vk ctr out) = runNodeNewCounter vk ctr out
runNodeCmd (NodeIssueOpCert vk sk ctr p out) =
  runNodeIssueOpCert vk sk ctr p out



--
-- Node command implementations
--

runNodeKeyGenCold :: KeyOutputFormat
                  -> VerificationKeyFile
                  -> SigningKeyFile
                  -> OpCertCounterFile
                  -> ExceptT ShelleyNodeCmdError IO ()
runNodeKeyGenCold fmt (VerificationKeyFile vkeyPath)
                  (SigningKeyFile skeyPath)
                  (OpCertCounterFile ocertCtrPath) = do
    skey <- liftIO $ generateSigningKey AsStakePoolKey
    let vkey = getVerificationKey skey

    case fmt of
      KeyOutputFormatTextEnvelope ->
        firstExceptT ShelleyNodeCmdWriteFileError
          . newExceptT
          $ writeLazyByteStringFile skeyPath
          $ textEnvelopeToJSON (Just skeyDesc) skey
      KeyOutputFormatBech32 ->
        firstExceptT ShelleyNodeCmdWriteFileError
          . newExceptT
          $ writeTextFile skeyPath
          $ serialiseToBech32 skey

    case fmt of
      KeyOutputFormatTextEnvelope ->
        firstExceptT ShelleyNodeCmdWriteFileError
          . newExceptT
          $ writeLazyByteStringFile vkeyPath
          $ textEnvelopeToJSON (Just vkeyDesc) vkey
      KeyOutputFormatBech32 ->
        firstExceptT ShelleyNodeCmdWriteFileError
          . newExceptT
          $ writeTextFile vkeyPath
          $ serialiseToBech32 vkey

    firstExceptT ShelleyNodeCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringFile ocertCtrPath
      $ textEnvelopeToJSON (Just ocertCtrDesc)
      $ OperationalCertificateIssueCounter initialCounter vkey
  where
    skeyDesc :: TextEnvelopeDescr
    skeyDesc = "Stake Pool Operator Signing Key"

    vkeyDesc :: TextEnvelopeDescr
    vkeyDesc = "Stake Pool Operator Verification Key"

    ocertCtrDesc :: TextEnvelopeDescr
    ocertCtrDesc = "Next certificate issue number: "
                <> fromString (show initialCounter)

    initialCounter :: Word64
    initialCounter = 0


runNodeKeyGenKES
  :: KeyOutputFormat
  -> VerificationKeyFile
  -> SigningKeyFile
  -> ExceptT ShelleyNodeCmdError IO ()
runNodeKeyGenKES fmt (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) = do
  skey <- liftIO $ generateSigningKey AsKesKey

  let vkey = getVerificationKey skey

  case fmt of
    KeyOutputFormatTextEnvelope ->
      firstExceptT ShelleyNodeCmdWriteFileError
        . newExceptT
        $ writeLazyByteStringFileWithOwnerPermissions skeyPath
        $ textEnvelopeToJSON (Just skeyDesc) skey
    KeyOutputFormatBech32 ->
      firstExceptT ShelleyNodeCmdWriteFileError
        . newExceptT
        $ writeTextFile skeyPath
        $ serialiseToBech32 skey

  case fmt of
    KeyOutputFormatTextEnvelope ->
      firstExceptT ShelleyNodeCmdWriteFileError
        . newExceptT
        $ writeLazyByteStringFile vkeyPath
        $ textEnvelopeToJSON (Just vkeyDesc) vkey
    KeyOutputFormatBech32 ->
      firstExceptT ShelleyNodeCmdWriteFileError
        . newExceptT
        $ writeTextFile vkeyPath
        $ serialiseToBech32 vkey

  where
    skeyDesc :: TextEnvelopeDescr
    skeyDesc = "KES Signing Key"

    vkeyDesc :: TextEnvelopeDescr
    vkeyDesc = "KES Verification Key"

runNodeKeyGenVRF
  :: KeyOutputFormat
  -> VerificationKeyFile
  -> SigningKeyFile
  -> ExceptT ShelleyNodeCmdError IO ()
runNodeKeyGenVRF fmt (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) = do
  skey <- liftIO $ generateSigningKey AsVrfKey

  let vkey = getVerificationKey skey

  case fmt of
    KeyOutputFormatTextEnvelope ->
      firstExceptT ShelleyNodeCmdWriteFileError
        . newExceptT
        $ writeLazyByteStringFileWithOwnerPermissions skeyPath
        $ textEnvelopeToJSON (Just skeyDesc) skey
    KeyOutputFormatBech32 ->
      firstExceptT ShelleyNodeCmdWriteFileError
        . newExceptT
        $ writeTextFile skeyPath
        $ serialiseToBech32 skey

  case fmt of
    KeyOutputFormatTextEnvelope ->
      firstExceptT ShelleyNodeCmdWriteFileError
        . newExceptT
        $ writeLazyByteStringFile vkeyPath
        $ textEnvelopeToJSON (Just vkeyDesc) vkey
    KeyOutputFormatBech32 ->
      firstExceptT ShelleyNodeCmdWriteFileError
        . newExceptT
        $ writeTextFile vkeyPath
        $ serialiseToBech32 vkey
  where
    skeyDesc, vkeyDesc :: TextEnvelopeDescr
    skeyDesc = "VRF Signing Key"
    vkeyDesc = "VRF Verification Key"

runNodeKeyHashVRF :: VerificationKeyOrFile VrfKey
                  -> Maybe OutputFile
                  -> ExceptT ShelleyNodeCmdError IO ()
runNodeKeyHashVRF verKeyOrFile mOutputFp = do
  vkey <- firstExceptT ShelleyNodeCmdReadKeyFileError
    . newExceptT
    $ readVerificationKeyOrFile AsVrfKey verKeyOrFile

  let hexKeyHash = serialiseToRawBytesHex (verificationKeyHash vkey)

  case mOutputFp of
    Just (OutputFile fpath) -> liftIO $ BS.writeFile fpath hexKeyHash
    Nothing -> liftIO $ BS.putStrLn hexKeyHash


runNodeNewCounter :: ColdVerificationKeyOrFile
                  -> Word
                  -> OpCertCounterFile
                  -> ExceptT ShelleyNodeCmdError IO ()
runNodeNewCounter coldVerKeyOrFile counter
                  (OpCertCounterFile ocertCtrPath) = do

    vkey <- firstExceptT ShelleyNodeCmdReadFileError . newExceptT $
      readColdVerificationKeyOrFile coldVerKeyOrFile

    let ocertIssueCounter =
          OperationalCertificateIssueCounter (fromIntegral counter) vkey

    firstExceptT ShelleyNodeCmdWriteFileError . newExceptT
      $ writeLazyByteStringFile ocertCtrPath
      $ textEnvelopeToJSON Nothing ocertIssueCounter


runNodeIssueOpCert :: VerificationKeyOrFile KesKey
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
runNodeIssueOpCert kesVerKeyOrFile
                   (SigningKeyFile stakePoolSKeyFile)
                   (OpCertCounterFile ocertCtrPath)
                   kesPeriod
                   (OutputFile certFile) = do

    ocertIssueCounter <- firstExceptT ShelleyNodeCmdReadFileError
      . newExceptT
      $ readFileTextEnvelope AsOperationalCertificateIssueCounter ocertCtrPath

    verKeyKes <- firstExceptT ShelleyNodeCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsKesKey kesVerKeyOrFile

    signKey <- firstExceptT ShelleyNodeCmdReadKeyFileError
      . newExceptT
      $ readKeyFileAnyOf
          bech32PossibleBlockIssuers
          textEnvPossibleBlockIssuers
          stakePoolSKeyFile

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
      $ writeLazyByteStringFile ocertCtrPath
      $ textEnvelopeToJSON (Just $ ocertCtrDesc $ getCounter nextOcertCtr) nextOcertCtr

    firstExceptT ShelleyNodeCmdWriteFileError
      . newExceptT
      $ writeLazyByteStringFile certFile
      $ textEnvelopeToJSON Nothing ocert
  where
    getCounter :: OperationalCertificateIssueCounter -> Word64
    getCounter (OperationalCertificateIssueCounter n _) = n

    ocertCtrDesc :: Word64 -> TextEnvelopeDescr
    ocertCtrDesc n = "Next certificate issue number: " <> fromString (show n)

    textEnvPossibleBlockIssuers
      :: [FromSomeType HasTextEnvelope
                       (Either (SigningKey StakePoolKey)
                               (SigningKey GenesisDelegateExtendedKey))]
    textEnvPossibleBlockIssuers =
      [ FromSomeType (AsSigningKey AsStakePoolKey)        Left
      , FromSomeType (AsSigningKey AsGenesisDelegateKey) (Left . castSigningKey)
      , FromSomeType (AsSigningKey AsGenesisDelegateExtendedKey) Right
      ]

    bech32PossibleBlockIssuers
      :: [FromSomeType SerialiseAsBech32
                       (Either (SigningKey StakePoolKey)
                               (SigningKey GenesisDelegateExtendedKey))]
    bech32PossibleBlockIssuers =
      [FromSomeType (AsSigningKey AsStakePoolKey) Left]

-- | Read a cold verification key or file.
--
-- If a filepath is provided, it will be interpreted as a text envelope
-- formatted file.
readColdVerificationKeyOrFile
  :: ColdVerificationKeyOrFile
  -> IO (Either (FileError TextEnvelopeError) (VerificationKey StakePoolKey))
readColdVerificationKeyOrFile coldVerKeyOrFile =
  case coldVerKeyOrFile of
    ColdStakePoolVerificationKey vk -> pure (Right vk)
    ColdGenesisDelegateVerificationKey vk ->
      pure $ Right (castVerificationKey vk)
    ColdVerificationKeyFile (VerificationKeyFile fp) ->
      readFileTextEnvelopeAnyOf
        [ FromSomeType (AsVerificationKey AsStakePoolKey) id
        , FromSomeType (AsVerificationKey AsGenesisDelegateKey) castVerificationKey
        ]
        fp
