{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Node.BulkCredentials
  ( tests
  ) where

import           Cardano.Api
import           Cardano.Node.Protocol.Shelley (readLeaderCredentials)
import           Cardano.Node.Types (ProtocolFilepaths (..))

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import           Hedgehog (Property, PropertyT, checkParallel, evalIO, property, success, withTests, (===))
import           Hedgehog.Internal.Property (Group (..), failWith)
import           System.Directory (removeFile)
import           System.IO (hClose, openTempFile)

prop_readLeaderCredentialsBulk_acceptsLegacyTupleFormat :: Property
prop_readLeaderCredentialsBulk_acceptsLegacyTupleFormat =
  withTests 1 . property $ do
    credentialEnvelopes <- evalIO mkValidCredentialEnvelopes
    withBulkCredentialsFile
      (Aeson.encode [legacyTuplePayload credentialEnvelopes])
      $ \bulkFile -> do
          credentials <- evalIO $ runExceptT $
            readLeaderCredentials (Just emptyProtocolFilepaths { shelleyBulkCredsFile = Just bulkFile })
          case credentials of
            Left err -> failWith Nothing $ show err
            Right parsed -> length parsed === 1

prop_readLeaderCredentialsBulk_acceptsNamedObjectFormat :: Property
prop_readLeaderCredentialsBulk_acceptsNamedObjectFormat =
  withTests 1 . property $ do
    credentialEnvelopes <- evalIO mkValidCredentialEnvelopes
    withBulkCredentialsFile
      (Aeson.encode [namedObjectPayload credentialEnvelopes])
      $ \bulkFile -> do
          credentials <- evalIO $ runExceptT $
            readLeaderCredentials (Just emptyProtocolFilepaths { shelleyBulkCredsFile = Just bulkFile })
          case credentials of
            Left err -> failWith Nothing $ show err
            Right parsed -> length parsed === 1

data ValidCredentialEnvelopes = ValidCredentialEnvelopes
  { vceCert :: TextEnvelope
  , vceVrf  :: TextEnvelope
  , vceKes  :: TextEnvelope
  }

mkValidCredentialEnvelopes :: IO ValidCredentialEnvelopes
mkValidCredentialEnvelopes = do
  coldSKey <- generateSigningKey AsStakePoolKey
  kesSKey <- generateSigningKey AsKesKey
  vrfSKey <- generateSigningKey AsVrfKey
  let counter = OperationalCertificateIssueCounter 0 (getVerificationKey coldSKey)
  opCert <- either (fail . show) pure $
    fst <$> issueOperationalCertificate
      (getVerificationKey kesSKey)
      (Left $ AnyStakePoolNormalSigningKey coldSKey)
      (KESPeriod 0)
      counter
  pure $
    ValidCredentialEnvelopes
      { vceCert = serialiseToTextEnvelope Nothing opCert
      , vceVrf  = serialiseToTextEnvelope Nothing vrfSKey
      , vceKes  = serialiseToTextEnvelope Nothing kesSKey
      }

legacyTuplePayload :: ValidCredentialEnvelopes -> Aeson.Value
legacyTuplePayload ValidCredentialEnvelopes { vceCert, vceVrf, vceKes } =
  Aeson.toJSON (vceCert, vceVrf, vceKes)

namedObjectPayload :: ValidCredentialEnvelopes -> Aeson.Value
namedObjectPayload ValidCredentialEnvelopes { vceCert, vceVrf, vceKes } =
  Aeson.object
    [ "cert" Aeson..= vceCert
    , "vrf"  Aeson..= vceVrf
    , "kes"  Aeson..= vceKes
    ]

withBulkCredentialsFile
  :: LBS.ByteString
  -> (FilePath -> PropertyT IO ())
  -> PropertyT IO ()
withBulkCredentialsFile payload action = do
  (fp, h) <- evalIO $ openTempFile "." "bulk-credentials.json"
  evalIO $ LBS.hPut h payload
  evalIO $ hClose h
  action fp
  evalIO $ removeFile fp
  success

emptyProtocolFilepaths :: ProtocolFilepaths
emptyProtocolFilepaths =
  ProtocolFilepaths
    { byronCertFile = Nothing
    , byronKeyFile = Nothing
    , shelleyKESSource = Nothing
    , shelleyVRFFile = Nothing
    , shelleyCertFile = Nothing
    , shelleyBulkCredsFile = Nothing
    }

tests :: IO Bool
tests =
  checkParallel $ Group "Test.Cardano.Node.BulkCredentials"
    [ ("prop_readLeaderCredentialsBulk_acceptsLegacyTupleFormat", prop_readLeaderCredentialsBulk_acceptsLegacyTupleFormat)
    , ("prop_readLeaderCredentialsBulk_acceptsNamedObjectFormat", prop_readLeaderCredentialsBulk_acceptsNamedObjectFormat)
    ]
