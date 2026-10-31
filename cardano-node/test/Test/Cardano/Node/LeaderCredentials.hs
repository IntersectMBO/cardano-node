{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Node.LeaderCredentials
  ( tests
  ) where

import           Cardano.Api hiding (FileError)
import qualified Cardano.Api as Api
import           Cardano.Api.Experimental.Certificate
                   (KESPeriod (..), OperationalCertificateIssueCounter (..), issueOperationalCertificate)

import           Cardano.Crypto.Seed (mkSeedFromBytes)
import           Cardano.Node.Protocol.Shelley
                   (PraosLeaderCredentialsError (..), readLeaderCredentials)
import           Cardano.Node.Types (ProtocolFilepaths (..))
import           Ouroboros.Consensus.Protocol.Praos.Common (PraosCanBeLeader (..))
import           Ouroboros.Consensus.Shelley.Node (ShelleyLeaderCredentials (..))

import           Control.Exception (bracket, finally)
import           Control.Monad (forM_, unless)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Word (Word8)
import           System.Directory (getTemporaryDirectory, removeFile)
import           System.IO (hClose, hIsClosed, openBinaryTempFile)

import           Hedgehog

signingKey :: Key key => AsType key -> Word8 -> SigningKey key
signingKey keyType byte =
  deterministicSigningKey keyType $ mkSeedFromBytes $
    BS.replicate (fromIntegral $ deterministicSigningKeySeedSize keyType) byte

blsKey :: Word8 -> SigningKey BlsKey
blsKey = signingKey AsBlsKey

envelope :: HasTextEnvelope a => a -> Aeson.Value
envelope = Aeson.toJSON . serialiseToTextEnvelope Nothing

bulkEntry :: Word8 -> [Aeson.Value]
bulkEntry byte =
  case issueOperationalCertificate (getVerificationKey kesKey)
         (Left $ AnyStakePoolNormalSigningKey coldKey) (KESPeriod 0)
         (OperationalCertificateIssueCounter 0 $ getVerificationKey coldKey) of
    Left err -> error $ "Operational certificate fixture failed: " <> show err
    Right (cert, _) -> [envelope cert, envelope vrfKey, envelope kesKey]
 where
  coldKey = signingKey AsStakePoolKey byte
  vrfKey = signingKey AsVrfKey byte
  kesKey = signingKey AsKesKey byte

withBulkFile :: Aeson.Value -> (FilePath -> IO a) -> IO a
withBulkFile value action = do
  directory <- getTemporaryDirectory
  bracket (openBinaryTempFile directory "node-bulk-credentials.json")
    (\(fp, handle) ->
      (hIsClosed handle >>= \closed -> unless closed $ hClose handle)
        `finally` removeFile fp)
    (\(fp, handle) -> do
      LBS.hPut handle $ Aeson.encode value
      hClose handle
      action fp)

readBulk :: FilePath -> IO (Either PraosLeaderCredentialsError [ShelleyLeaderCredentials StandardCrypto])
readBulk fp = runExceptT $ readLeaderCredentials $ Just ProtocolFilepaths
  { byronCertFile = Nothing
  , byronKeyFile = Nothing
  , shelleyKESSource = Nothing
  , shelleyVRFFile = Nothing
  , shelleyBLSFile = Nothing
  , shelleyCertFile = Nothing
  , shelleyBulkCredsFile = Just fp
  }

loadBulk :: Aeson.Value -> IO (Either PraosLeaderCredentialsError [ShelleyLeaderCredentials StandardCrypto])
loadBulk value = withBulkFile value readBulk

votingKeys :: [ShelleyLeaderCredentials StandardCrypto] -> [[VerificationKey BlsKey]]
votingKeys = map $ map (getVerificationKey . BlsSigningKey)
  . praosCanBeLeaderSignKeyBLS . shelleyLeaderCredentialsCanBeLeader

prop_mixedPools :: Property
prop_mixedPools = withTests 1 $ property $ do
  result <- evalIO $ loadBulk $ Aeson.toJSON
    [ bulkEntry 1
    , bulkEntry 2 ++ [envelope $ blsKey 11]
    , bulkEntry 3 ++ [Aeson.toJSON [envelope $ blsKey 21, envelope $ blsKey 22]]
    ]
  credentials <- evalEither result
  votingKeys credentials === map (map $ getVerificationKey . blsKey) [[], [11], [21, 22]]

prop_optionalValues :: Property
prop_optionalValues = withTests 1 $ property $ do
  result <- evalIO $ loadBulk $ Aeson.toJSON
    [bulkEntry 1, bulkEntry 2 ++ [Aeson.Null], bulkEntry 3 ++ [Aeson.toJSON ([] :: [Aeson.Value])]]
  credentials <- evalEither result
  votingKeys credentials === [[], [], []]
  empty <- evalIO $ loadBulk $ Aeson.toJSON ([] :: [Aeson.Value])
  emptyCredentials <- evalEither empty
  votingKeys emptyCredentials === []

prop_malformedShape :: Property
prop_malformedShape = withTests 1 $ property $
  forM_ malformed $ \value -> do
    result <- evalIO $ loadBulk value
    case result of
      Left (EnvelopeParseError _ _) -> success
      Left err -> annotateShow err >> failure
      Right _ -> failure
 where
  malformed =
    [ Aeson.toJSON [take 2 $ bulkEntry 1]
    , Aeson.toJSON [bulkEntry 1 ++ [Aeson.Null, Aeson.Null]]
    , Aeson.toJSON [bulkEntry 1 ++ [Aeson.String "bls.skey"]]
    , Aeson.object []
    ]

prop_invalidKeys :: Property
prop_invalidKeys = withTests 1 $ property $
  forM_ [envelope $ signingKey AsVrfKey 30, corruptBls] $ \invalid -> do
    let value = Aeson.toJSON
          [ bulkEntry 1
          , bulkEntry 2 ++ [Aeson.toJSON [envelope $ blsKey 11, invalid]]
          ]
    (fp, result) <- evalIO $ withBulkFile value $ \fp -> do
      result <- readBulk fp
      pure (fp, result)
    case result of
      Left (FileError (Api.FileError location _)) -> location === fp <> ".1bls.1"
      Left err -> annotateShow err >> failure
      Right _ -> failure
 where
  corruptBls = case envelope $ blsKey 22 of
    Aeson.Object fields -> Aeson.Object $ KeyMap.insert "cborHex" (Aeson.String "00") fields
    _ -> error "BLS text envelope fixture must be an object"

tests :: IO Bool
tests = checkParallel $ Group "Leader credentials"
  [ ("mixed pools preserve BLS identities and order", prop_mixedPools)
  , ("optional BLS values and empty bulk file", prop_optionalValues)
  , ("malformed bulk entry shapes", prop_malformedShape)
  , ("invalid supplied BLS keys fail at their indexed location", prop_invalidKeys)
  ]
