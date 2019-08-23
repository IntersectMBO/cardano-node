{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.CLI.Ops
  ( CLIOps(..)
  , decideCLIOps
  , CliError(..)
  ) where

import qualified Prelude as Prelude
import           Cardano.Prelude hiding (option)

import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T

import           Cardano.Chain.Delegation hiding (epoch)
import           Cardano.Crypto (SigningKey (..))
import           Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import           Codec.CBOR.Write (toLazyByteString)
import qualified Cardano.Crypto.Random as CCr
import qualified Cardano.Crypto.Signing as CCr
import           Cardano.Chain.Genesis
import qualified Crypto.SCRAPE as Scrape

import           Cardano.Common.Protocol
import qualified Cardano.Legacy.Byron as Legacy
import           Cardano.Node.CanonicalJSON


-- | Generic operations for a specific system era.
data CLIOps m
  = CLIOps
  { coSerialiseGenesisKey       :: SigningKey    -> m LB.ByteString
  , coSerialiseDelegateKey      :: SigningKey    -> m LB.ByteString
  , coSerialisePoorKey          :: PoorSecret    -> m LB.ByteString
  , coSerialiseGenesis          :: GenesisData   -> m LB.ByteString
  , coSerialiseDelegationCert   :: Certificate   -> m LB.ByteString
  , coDeserialiseDelegateKey    :: FilePath
                                -> LB.ByteString -> m SigningKey
  , coProtocol                  :: Protocol
  }

decideCLIOps :: Protocol -> IO (CLIOps IO)
decideCLIOps coProtocol =
  let serialiseSigningKey (SigningKey x) = toLazyByteString $ CCr.toCBORXPrv x
  in case coProtocol of
    ByronLegacy ->
      pure CLIOps
      { coSerialiseGenesisKey          = pure . serialiseSigningKey
      , coSerialiseDelegateKey         = \sk->
          toLazyByteString . Legacy.encodeLegacyDelegateKey . Legacy.LegacyDelegateKey sk
          <$> CCr.runSecureRandom Scrape.keyPairGenerate
      , coSerialisePoorKey             = pure . serialiseSigningKey . poorSecretToKey
      , coSerialiseGenesis             = pure . canonicalEncPre
      , coSerialiseDelegationCert      = pure . canonicalEncPre
      , coDeserialiseDelegateKey       = \f ->
          flip (.) (deserialiseFromBytes Legacy.decodeLegacyDelegateKey) $
          \case Left  e -> throwIO $ SigningKeyDeserialisationFailed f e
                Right x -> pure . Legacy.lrkSigningKey . snd $ x
      , coProtocol = coProtocol
      }
    RealPBFT ->
      pure CLIOps
      { coSerialiseGenesisKey          = pure . serialiseSigningKey
      , coSerialiseDelegateKey         = pure . serialiseSigningKey
      , coSerialisePoorKey             = pure . serialiseSigningKey . poorSecretToKey
      , coSerialiseGenesis             = pure . canonicalEncPre
      , coSerialiseDelegationCert      = pure . canonicalEncPre
      , coDeserialiseDelegateKey       = \f ->
          flip (.) (deserialiseFromBytes CCr.fromCBORXPrv) $
          \case Left  e -> throwIO $ SigningKeyDeserialisationFailed f e
                Right x -> pure . SigningKey . snd $ x
      , coProtocol = coProtocol
      }
    x ->
      throwIO $ ProtocolNotSupported x

data CliError
  -- Basic user errors
  = OutputMustNotAlreadyExist FilePath
  | ProtocolNotSupported Protocol
  -- Validation errors
  | CertificateValidationErrors FilePath [Text]
  -- Serialization errors
  | ProtocolParametersParseFailed FilePath Text
  | GenesisReadError FilePath GenesisDataError
  | SigningKeyDeserialisationFailed FilePath DeserialiseFailure
  | VerificationKeyDeserialisationFailed FilePath Text
  | DlgCertificateDeserialisationFailed FilePath Text
  | TxDeserialisationFailed FilePath DeserialiseFailure
  -- TODO:  sadly, VerificationKeyParseError isn't exported from Cardano.Crypto.Signing/*
  -- Inconsistencies
  | DelegationError GenesisDelegationError
  | GenesisSpecError Text
  | GenesisGenerationError GenesisDataGenerationError
  -- Invariants/assertions -- does it belong here?
  | NoGenesisDelegationForKey Text

instance Show CliError where
  show (OutputMustNotAlreadyExist fp)
    = "Output file/directory must not already exist: " <> fp
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

instance Exception CliError
