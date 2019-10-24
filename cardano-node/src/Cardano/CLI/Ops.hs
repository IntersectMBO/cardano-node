{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.CLI.Ops
  ( CLIOps(..)
  , decideCLIOps
  , deserialiseDelegateKey
  , serialiseDelegationCert
  , serialiseDelegateKey
  , serialiseGenesis
  , serialisePoorKey
  , serialiseSigningKey'
  , CliError(..)
  ) where

import qualified Prelude as Prelude
import           Cardano.Prelude hiding (option)
import           Test.Cardano.Prelude (canonicalEncodePretty)

import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Text.JSON.Canonical as CanonicalJSON

import qualified Cardano.Chain.Delegation as Dlg
import           Cardano.Crypto (SigningKey (..))
import           Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import           Codec.CBOR.Write (toLazyByteString)
import qualified Cardano.Crypto.Signing as Crypto
import qualified Cardano.Chain.Genesis as Genesis

import           Cardano.Config.Protocol (Protocol(..))
import           Cardano.Config.Types
import qualified Cardano.CLI.Legacy.Byron as Legacy


-- | Generic operations for a specific system era.
data CLIOps m
  = CLIOps
  { coSerialiseGenesisKey :: SigningKey -> m LB.ByteString
  , coSerialiseDelegateKey :: SigningKey -> m LB.ByteString
  , coSerialisePoorKey :: Genesis.PoorSecret -> m LB.ByteString
  , coSerialiseGenesis :: Genesis.GenesisData -> m LB.ByteString
  , coSerialiseDelegationCert :: Dlg.Certificate -> m LB.ByteString
  , coDeserialiseDelegateKey :: FilePath -> LB.ByteString -> m SigningKey
  , coProtocol :: Protocol
  }

-- | Supply the corresponding 'CLIOps' for a given system era designator.
decideCLIOps :: Protocol -> IO (CLIOps IO)
decideCLIOps protocol =
  case protocol of
    ByronLegacy ->
      pure CLIOps
      { coSerialiseGenesisKey          = pure . serialiseSigningKey
      , coSerialiseDelegateKey         = \sk ->
          pure . toLazyByteString . Legacy.encodeLegacyDelegateKey $ Legacy.LegacyDelegateKey sk
      , coSerialisePoorKey             = pure . serialiseSigningKey
                                              . Genesis.poorSecretToKey
      , coSerialiseGenesis             = pure . canonicalEncodePretty
      , coSerialiseDelegationCert      = pure . canonicalEncodePretty
      , coDeserialiseDelegateKey       = \f ->
          flip (.) (deserialiseFromBytes Legacy.decodeLegacyDelegateKey) $
          \case Left  e -> throwIO $ SigningKeyDeserialisationFailed f e
                Right x -> pure . Legacy.lrkSigningKey . snd $ x
      , coProtocol = protocol
      }
    RealPBFT ->
      pure CLIOps
      { coSerialiseGenesisKey          = pure . serialiseSigningKey
      , coSerialiseDelegateKey         = pure . serialiseSigningKey
      , coSerialisePoorKey             = pure . serialiseSigningKey
                                              . Genesis.poorSecretToKey
      , coSerialiseGenesis             = pure . canonicalEncodePretty
      , coSerialiseDelegationCert      = pure . canonicalEncodePretty
      , coDeserialiseDelegateKey       = \f ->
          flip (.) (deserialiseFromBytes Crypto.fromCBORXPrv) $
          \case Left  e -> throwIO $ SigningKeyDeserialisationFailed f e
                Right x -> pure . SigningKey . snd $ x
      , coProtocol = protocol
      }
    x ->
      throwIO $ ProtocolNotSupported x
    where
      serialiseSigningKey (SigningKey x) = toLazyByteString $ Crypto.toCBORXPrv x

deserialiseDelegateKey :: Protocol -> FilePath -> LB.ByteString -> Either CliError SigningKey
deserialiseDelegateKey ByronLegacy fp delSkey =
  case deserialiseFromBytes Legacy.decodeLegacyDelegateKey delSkey of
    Left deSerFail -> Left $ SigningKeyDeserialisationFailed fp deSerFail
    Right (_, Legacy.LegacyDelegateKey sKey _) -> pure sKey
deserialiseDelegateKey RealPBFT fp delSkey =
  case deserialiseFromBytes Crypto.fromCBORXPrv delSkey of
    Left deSerFail -> Left $ SigningKeyDeserialisationFailed fp deSerFail
    Right (_, sKey) -> Right $ SigningKey sKey
deserialiseDelegateKey ptcl _ _ = Left $ ProtocolNotSupported ptcl

serialiseDelegationCert :: CanonicalJSON.ToJSON Identity a => Protocol -> a -> Either CliError LB.ByteString
serialiseDelegationCert ByronLegacy dlgCert = pure $ canonicalEncodePretty dlgCert
serialiseDelegationCert RealPBFT dlgCert = pure $ canonicalEncodePretty dlgCert
serialiseDelegationCert ptcl _ = Left $ ProtocolNotSupported ptcl


serialiseDelegateKey :: Protocol -> SigningKey -> IO (Either CliError LB.ByteString)
serialiseDelegateKey ByronLegacy sk = pure
                                    . toLazyByteString
                                    . Legacy.encodeLegacyDelegateKey
                                    . Legacy.LegacyDelegateKey sk
                                    <$> Crypto.runSecureRandom Scrape.keyPairGenerate
serialiseDelegateKey RealPBFT sk = pure $ serialiseSigningKey' RealPBFT sk
serialiseDelegateKey ptcl _ = pure . Left $ ProtocolNotSupported ptcl

serialiseGenesis ::  Protocol -> Genesis.GenesisData -> Either CliError LB.ByteString
serialiseGenesis ByronLegacy gData = pure $ canonicalEncodePretty gData
serialiseGenesis RealPBFT gData = pure $ canonicalEncodePretty gData
serialiseGenesis ptcl _ = Left $ ProtocolNotSupported ptcl

serialisePoorKey :: Protocol -> Genesis.PoorSecret -> Either CliError LB.ByteString
serialisePoorKey ByronLegacy ps = serialiseSigningKey' ByronLegacy $ Genesis.poorSecretToKey ps
serialisePoorKey RealPBFT ps = serialiseSigningKey' RealPBFT $ Genesis.poorSecretToKey ps
serialisePoorKey ptcl _ = Left $ ProtocolNotSupported ptcl


serialiseSigningKey' :: Protocol -> SigningKey -> Either CliError LB.ByteString
serialiseSigningKey' ByronLegacy (SigningKey k) = pure . toLazyByteString $ Crypto.toCBORXPrv k
serialiseSigningKey' RealPBFT (SigningKey k) = pure . toLazyByteString $ Crypto.toCBORXPrv k
serialiseSigningKey' ptcl _ = Left $ ProtocolNotSupported ptcl

-- | Exception type for all errors thrown by the CLI.
--   Well, almost all, since we don't rethrow the errors from readFile & such.
data CliError
  -- Basic user errors
  = OutputMustNotAlreadyExist !FilePath
  | ProtocolNotSupported !Protocol
  | NotEnoughTxInputs
  | NotEnoughTxOutputs
  | ConfigError !ConfigError
  -- Validation errors
  | CertificateValidationErrors !FilePath ![Text]
  -- Serialization errors
  | ProtocolParametersParseFailed !FilePath !Text
  | GenesisReadError !FilePath !Genesis.GenesisDataError
  | SigningKeyDeserialisationFailed !FilePath !DeserialiseFailure
  | VerificationKeyDeserialisationFailed !FilePath !Text
  | DlgCertificateDeserialisationFailed !FilePath !Text
  | TxDeserialisationFailed !FilePath !DeserialiseFailure
  -- TODO:  sadly, VerificationKeyParseError isn't exported from Cardano.Crypto.Signing/*
  -- Inconsistencies
  | DelegationError !Genesis.GenesisDelegationError
  | GenesisSpecError !Text
  | GenesisGenerationError !Genesis.GenesisDataGenerationError
  -- Invariants/assertions -- does it belong here?
  | NoGenesisDelegationForKey !Text
  -- File reading errors
  | ReadVerificationKeyFailure !FilePath !Text
  -- ^ An exception was encountered while trying to read
  -- the verification key file.
  | ReadSigningKeyFailure !FilePath !Text
  -- ^ An exception was encountered while trying to read
  -- the signing key file.


instance Show CliError where
  show (OutputMustNotAlreadyExist fp)
    = "Output file/directory must not already exist: " <> fp
  show NotEnoughTxInputs
    = "Transactions must have at least one input."
  show NotEnoughTxOutputs
    = "Transactions must have at least one output."
  show (ConfigError e)
    = "Configuration error: " <> show e
  show (ProtocolNotSupported proto)
    = "Unsupported protocol "<> show proto
  show (CertificateValidationErrors fp errs)
    = Prelude.unlines $
      "Errors while validating certificate '" <> fp <> "':":
      (("  " <>) . T.unpack <$> errs)
  show (ProtocolParametersParseFailed fp err)
    = "Protocol parameters file '" <> fp <> "' read failure: "<> T.unpack err
  show (GenesisReadError fp err)
    = "Genesis file '" <> fp <> "' read failure: "<> show err
  show (SigningKeyDeserialisationFailed fp err)
    = "Signing key '" <> fp <> "' read failure: "<> show err
  show (VerificationKeyDeserialisationFailed fp err)
    = "Verification key '" <> fp <> "' read failure: "<> T.unpack err
  show (DlgCertificateDeserialisationFailed fp err)
    = "Delegation certificate '" <> fp <> "' read failure: "<> T.unpack err
  show (TxDeserialisationFailed fp err)
    = "Transaction file '" <> fp <> "' read failure: "<> show err
  show (DelegationError err)
    = "Error while issuing delegation: " <> show err
  show (GenesisSpecError err)
    = "Error in genesis specification: " <> T.unpack err
  show (GenesisGenerationError err)
    = "Genesis generation failed: " <> show err
  show (NoGenesisDelegationForKey key)
    = "Newly-generated genesis doesn't delegate to operational key: " <> T.unpack key
  show (ReadVerificationKeyFailure fp expt)
    = "Exception encountered while trying to read the verification key file at: " <> fp
       <> "Exception: " <> T.unpack expt
  show (ReadSigningKeyFailure fp expt)
    = "Exception encountered while trying to read the signing key file at: " <> fp
       <> "Exception: " <> T.unpack expt
instance Exception CliError
