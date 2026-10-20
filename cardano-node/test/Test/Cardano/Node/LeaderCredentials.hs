{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for the file-based leader credentials of
-- "Cardano.Node.Protocol.Shelley": a KES signing key file given on the
-- command line is handed to consensus as a path ('PraosCredentialsFile'),
-- after the node's own existence and envelope-type checks.
module Test.Cardano.Node.LeaderCredentials
  ( tests
  ) where

import           Cardano.Api hiding (FileError)
import           Cardano.Api.Experimental.Certificate (KESPeriod (..),
                   OperationalCertificateIssueCounter (..), issueOperationalCertificate)

import           Cardano.Crypto.KES.Class (algorithmNameKES)
import           Cardano.Crypto.KES.EnvelopeLocked (kesWithPeriodTypeString)
import           Cardano.Node.Protocol.Shelley (PraosLeaderCredentialsError (..),
                   readLeaderCredentials)
import           Cardano.Node.Types (KESSource (..), ProtocolFilepaths (..))
import           Cardano.Protocol.Crypto (KES)
import           Ouroboros.Consensus.Protocol.Praos.Common (PraosCanBeLeader (..),
                   PraosCredentialsSource (..))
import           Ouroboros.Consensus.Shelley.Node (ShelleyLeaderCredentials (..))

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text.Encoding as T
import           System.Directory (removeFile)
import           System.FilePath ((</>))

import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- | Write a fresh cold key, KES key, VRF key and operational certificate to
-- the directory in cardano-cli's text envelope format and return the
-- command-line file set that names them.
writeCredentials :: (MonadIO m, H.MonadTest m) => FilePath -> m ProtocolFilepaths
writeCredentials dir = do
  coldSKey <- H.evalIO $ generateSigningKey AsStakePoolKey
  kesSKey <- H.evalIO $ generateSigningKey AsKesKey
  vrfSKey <- H.evalIO $ generateSigningKey AsVrfKey
  let counter = OperationalCertificateIssueCounter 0 (getVerificationKey coldSKey)
  (opCert, _) <- H.evalEither $
    issueOperationalCertificate
      (getVerificationKey kesSKey)
      (Left (AnyStakePoolNormalSigningKey coldSKey))
      (KESPeriod 0)
      counter
  let kesFile = dir </> "kes.skey"
      vrfFile = dir </> "vrf.skey"
      certFile = dir </> "opcert.cert"
  H.evalIO (writeFileTextEnvelope (File kesFile) Nothing kesSKey) >>= H.evalEither
  H.evalIO (writeFileTextEnvelope (File vrfFile) Nothing vrfSKey) >>= H.evalEither
  H.evalIO (writeFileTextEnvelope (File certFile) Nothing opCert) >>= H.evalEither
  pure ProtocolFilepaths
    { byronCertFile = Nothing
    , byronKeyFile = Nothing
    , shelleyKESSource = Just (KESKeyFilePath kesFile)
    , shelleyVRFFile = Just vrfFile
    , shelleyCertFile = Just certFile
    , shelleyBulkCredsFile = Nothing
    }

kesKeyPath :: ProtocolFilepaths -> FilePath
kesKeyPath ProtocolFilepaths { shelleyKESSource = Just (KESKeyFilePath kesFile) } = kesFile
kesKeyPath _ = error "kesKeyPath: not a file-based KES source"

readCredentials
  :: (MonadIO m, H.MonadTest m)
  => ProtocolFilepaths
  -> m (Either PraosLeaderCredentialsError [ShelleyLeaderCredentials StandardCrypto])
readCredentials pfp = H.evalIO $ runExceptT $ readLeaderCredentials (Just pfp)

-- | Rewrite the @type@ field of the text envelope in the file.
setEnvelopeType :: (MonadIO m, H.MonadTest m) => FilePath -> Text -> m ()
setEnvelopeType file ty = do
  envelope <- H.evalIO (Aeson.eitherDecodeFileStrict' file) >>= H.evalEither
  obj <- case envelope of
    Aeson.Object o -> pure o
    _ -> H.failure
  H.evalIO $ Aeson.encodeFile file $ Aeson.Object $ KeyMap.insert "type" (Aeson.String ty) obj

-- | File mode hands consensus the opcert and the path of the KES key file,
-- not the decoded key.
prop_fileModeYieldsPraosCredentialsFile :: Property
prop_fileModeYieldsPraosCredentialsFile = H.propertyOnce $ H.moduleWorkspace "kes" $ \dir -> do
  pfp <- writeCredentials dir
  creds <- readCredentials pfp >>= H.evalEither
  case creds of
    [ ShelleyLeaderCredentials
        { shelleyLeaderCredentialsCanBeLeader =
            PraosCanBeLeader { praosCanBeLeaderCredentialsSource = PraosCredentialsFile _ path Nothing }
        }
      ] -> path === kesKeyPath pfp
    _ -> do
      H.note_ "expected exactly one credential with a PraosCredentialsFile source"
      H.failure

-- | A missing KES key file is reported as 'FileDoesNotExistError', the
-- classification 'Cardano.Node.Run.updateBlockForging' relies on to disable
-- forging on SIGHUP.
prop_missingKesKeyFileIsFileDoesNotExistError :: Property
prop_missingKesKeyFileIsFileDoesNotExistError = H.propertyOnce $ H.moduleWorkspace "kes" $ \dir -> do
  pfp <- writeCredentials dir
  H.evalIO $ removeFile (kesKeyPath pfp)
  result <- readCredentials pfp
  case result of
    Left (FileError (FileDoesNotExistError path)) -> path === kesKeyPath pfp
    _ -> do
      H.note_ "expected Left (FileError (FileDoesNotExistError _))"
      H.failure

-- | A KES key file of the period-carrying envelope type (the one the node
-- writes when it persists the evolved key) passes the node's header check
-- and is handed to consensus as a path, like the evolution-zero type.
prop_periodCarryingKesKeyTypeIsAccepted :: Property
prop_periodCarryingKesKeyTypeIsAccepted = H.propertyOnce $ H.moduleWorkspace "kes" $ \dir -> do
  pfp <- writeCredentials dir
  let withPeriodType =
        T.decodeUtf8 (kesWithPeriodTypeString (algorithmNameKES (Proxy @(KES StandardCrypto))))
  withPeriodType === "KesSigningKeyWithPeriod_ed25519_kes_2^6"
  setEnvelopeType (kesKeyPath pfp) withPeriodType
  creds <- readCredentials pfp >>= H.evalEither
  case creds of
    [ ShelleyLeaderCredentials
        { shelleyLeaderCredentialsCanBeLeader =
            PraosCanBeLeader { praosCanBeLeaderCredentialsSource = PraosCredentialsFile _ path Nothing }
        }
      ] -> path === kesKeyPath pfp
    _ -> do
      H.note_ "expected exactly one credential with a PraosCredentialsFile source"
      H.failure

-- | A KES key file whose envelope type is neither of the two KES signing key
-- types is refused before the node starts.
prop_unknownKesKeyTypeIsRefused :: Property
prop_unknownKesKeyTypeIsRefused = H.propertyOnce $ H.moduleWorkspace "kes" $ \dir -> do
  pfp <- writeCredentials dir
  let unknownType = "KesSigningKey_ed25519_kes_2^7"
  setEnvelopeType (kesKeyPath pfp) unknownType
  result <- readCredentials pfp
  case result of
    Left (UnsupportedKesKeyType path ty) -> do
      path === kesKeyPath pfp
      ty === unknownType
    _ -> do
      H.note_ "expected Left (UnsupportedKesKeyType _ _)"
      H.failure

-- | A file that is not a text envelope at all is refused as an envelope
-- parse error (without echoing its contents).
prop_nonEnvelopeKesKeyFileIsRefused :: Property
prop_nonEnvelopeKesKeyFileIsRefused = H.propertyOnce $ H.moduleWorkspace "kes" $ \dir -> do
  pfp <- writeCredentials dir
  H.evalIO $ writeFile (kesKeyPath pfp) "[\"not\", \"an\", \"envelope\"]"
  result <- readCredentials pfp
  case result of
    Left (EnvelopeParseError path _) -> path === kesKeyPath pfp
    _ -> do
      H.note_ "expected Left (EnvelopeParseError _ _)"
      H.failure

tests :: IO Bool
tests =
  H.checkSequential $ H.Group "Test.Cardano.Node.LeaderCredentials"
    [ ("prop_fileModeYieldsPraosCredentialsFile", prop_fileModeYieldsPraosCredentialsFile)
    , ("prop_missingKesKeyFileIsFileDoesNotExistError", prop_missingKesKeyFileIsFileDoesNotExistError)
    , ("prop_periodCarryingKesKeyTypeIsAccepted", prop_periodCarryingKesKeyTypeIsAccepted)
    , ("prop_unknownKesKeyTypeIsRefused", prop_unknownKesKeyTypeIsRefused)
    , ("prop_nonEnvelopeKesKeyFileIsRefused", prop_nonEnvelopeKesKeyFileIsRefused)
    ]
