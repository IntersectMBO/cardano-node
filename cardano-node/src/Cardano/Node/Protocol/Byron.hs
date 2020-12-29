{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Node.Protocol.Byron
  (
    -- * Protocol exposing the specific type
    -- | Use this when you need the specific instance
    mkConsensusProtocolByron

    -- * Protocols hiding the specific type
    -- | Use this when you want to handle protocols generically
  , mkSomeConsensusProtocolByron
    -- * Errors
  , ByronProtocolInstantiationError(..)
  , renderByronProtocolInstantiationError

    -- * Reusable parts
  , readGenesis
  , readLeaderCredentials
  ) where


import           Cardano.Prelude
import           Control.Monad.Trans.Except.Extra (bimapExceptT, firstExceptT, hoistEither,
                     hoistMaybe, left)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as Text

import           Cardano.Api.Byron

import qualified Cardano.Crypto.Hash as Crypto

import qualified Cardano.Crypto.Hashing as Byron.Crypto

import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Chain.UTxO as UTxO
import           Cardano.Crypto.ProtocolMagic (RequiresNetworkMagic)

import           Ouroboros.Consensus.Cardano hiding (Protocol)
import qualified Ouroboros.Consensus.Cardano as Consensus
import           Ouroboros.Consensus.Cardano.ByronHFC

import           Cardano.Node.Types

import           Cardano.Tracing.OrphanInstances.Byron ()
import           Cardano.Tracing.OrphanInstances.HardFork ()

import           Cardano.Node.Protocol.Types


------------------------------------------------------------------------------
-- Byron protocol
--

-- | Make 'SomeConsensusProtocol' using the Byron instance.
--
-- This lets us handle multiple protocols in a generic way.
--
-- This also serves a purpose as a sanity check that we have all the necessary
-- type class instances available.
--
mkSomeConsensusProtocolByron
  :: NodeByronProtocolConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT ByronProtocolInstantiationError IO SomeConsensusProtocol
mkSomeConsensusProtocolByron nc files =

    -- Applying the SomeConsensusProtocol here is a check that
    -- the type of mkConsensusProtocolByron fits all the class
    -- constraints we need to run the protocol.
    SomeConsensusProtocol <$> mkConsensusProtocolByron nc files


-- | Instantiate 'Consensus.Protocol' for Byron specifically.
--
-- Use this when you need to run the consensus with this specific protocol.
--
mkConsensusProtocolByron
  :: NodeByronProtocolConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT ByronProtocolInstantiationError IO
             (Consensus.Protocol IO ByronBlockHFC ProtocolByron)
mkConsensusProtocolByron NodeByronProtocolConfiguration {
                           npcByronGenesisFile,
                           npcByronGenesisFileHash,
                           npcByronReqNetworkMagic,
                           npcByronPbftSignatureThresh,
                           npcByronApplicationName,
                           npcByronApplicationVersion,
                           npcByronSupportedProtocolVersionMajor,
                           npcByronSupportedProtocolVersionMinor,
                           npcByronSupportedProtocolVersionAlt
                         }
                         files = do
    genesisConfig <- readGenesis npcByronGenesisFile
                                 npcByronGenesisFileHash
                                 npcByronReqNetworkMagic

    optionalLeaderCredentials <- readLeaderCredentials genesisConfig files

    return $
      Consensus.ProtocolByron $ Consensus.ProtocolParamsByron {
        byronGenesis = genesisConfig,
        byronPbftSignatureThreshold =
          PBftSignatureThreshold <$> npcByronPbftSignatureThresh,
        byronProtocolVersion =
          Update.ProtocolVersion
            npcByronSupportedProtocolVersionMajor
            npcByronSupportedProtocolVersionMinor
            npcByronSupportedProtocolVersionAlt,
        byronSoftwareVersion =
          Update.SoftwareVersion
            npcByronApplicationName
            npcByronApplicationVersion,
        byronLeaderCredentials =
          optionalLeaderCredentials
        }

readGenesis :: GenesisFile
            -> Maybe GenesisHash
            -> RequiresNetworkMagic
            -> ExceptT ByronProtocolInstantiationError IO
                       Genesis.Config
readGenesis (GenesisFile file) mbExpectedGenesisHash ncReqNetworkMagic = do
    (genesisData, genesisHash) <- firstExceptT (GenesisReadError file) $
                                  Genesis.readGenesisData file
    checkExpectedGenesisHash genesisHash
    return Genesis.Config {
      Genesis.configGenesisData       = genesisData,
      Genesis.configGenesisHash       = genesisHash,
      Genesis.configReqNetMagic       = ncReqNetworkMagic,
      Genesis.configUTxOConfiguration = UTxO.defaultUTxOConfiguration
      --TODO: add config support for the UTxOConfiguration if needed
    }
  where
    checkExpectedGenesisHash :: Genesis.GenesisHash
                             -> ExceptT ByronProtocolInstantiationError IO ()
    checkExpectedGenesisHash actual' =
      case mbExpectedGenesisHash of
        Just expected | actual /= expected ->
            throwError (GenesisHashMismatch actual expected)
          where
            actual = fromByronGenesisHash actual'

        _ -> return ()

    fromByronGenesisHash :: Genesis.GenesisHash -> GenesisHash
    fromByronGenesisHash (Genesis.GenesisHash h) =
        GenesisHash
      . fromMaybe impossible
      . Crypto.hashFromBytes
      . Byron.Crypto.hashToBytes
      $ h
      where
        impossible =
          panic "fromByronGenesisHash: old and new crypto libs disagree on hash size"



readLeaderCredentials :: Genesis.Config
                      -> Maybe ProtocolFilepaths
                      -> ExceptT ByronProtocolInstantiationError IO
                                 (Maybe ByronLeaderCredentials)
readLeaderCredentials _ Nothing = return Nothing
readLeaderCredentials genesisConfig
                      (Just ProtocolFilepaths {
                        byronCertFile,
                        byronKeyFile
                      }) =
  case (byronCertFile, byronKeyFile) of
    (Nothing, Nothing) -> pure Nothing
    (Just _, Nothing) -> left SigningKeyFilepathNotSpecified
    (Nothing, Just _) -> left DelegationCertificateFilepathNotSpecified
    (Just delegCertFile, Just signingKeyFile) -> do

         signingKeyFileBytes <- liftIO $ LB.readFile signingKeyFile
         delegCertFileBytes <- liftIO $ LB.readFile delegCertFile
         ByronSigningKey signingKey <- hoistMaybe (SigningKeyDeserialiseFailure signingKeyFile)
                         $ deserialiseFromRawBytes (AsSigningKey AsByronKey) $ LB.toStrict signingKeyFileBytes
         delegCert  <- firstExceptT (CanonicalDecodeFailure delegCertFile)
                         . hoistEither
                         $ canonicalDecodePretty delegCertFileBytes

         bimapExceptT CredentialsError Just
           . hoistEither
           $ mkByronLeaderCredentials genesisConfig signingKey delegCert "Byron"



------------------------------------------------------------------------------
-- Byron Errors
--

data ByronProtocolInstantiationError =
    CanonicalDecodeFailure !FilePath !Text
  | GenesisHashMismatch !GenesisHash !GenesisHash -- actual, expected
  | DelegationCertificateFilepathNotSpecified
  | GenesisConfigurationError !FilePath !Genesis.ConfigurationError
  | GenesisReadError !FilePath !Genesis.GenesisDataError
  | CredentialsError !ByronLeaderCredentialsError
  | SigningKeyDeserialiseFailure !FilePath
  | SigningKeyFilepathNotSpecified
  deriving Show


renderByronProtocolInstantiationError :: ByronProtocolInstantiationError -> Text
renderByronProtocolInstantiationError pie =
  case pie of
    CanonicalDecodeFailure fp failure -> "Canonical decode failure in " <> toS fp
                                         <> " Canonical failure: " <> failure
    GenesisHashMismatch actual expected ->
        "Wrong Byron genesis file: the actual hash is " <> show actual
     <> ", but the expected Byron genesis hash given in the node configuration "
     <> "file is " <> show expected
    DelegationCertificateFilepathNotSpecified -> "Delegation certificate filepath not specified"
    --TODO: Implement configuration error render function in cardano-ledger
    GenesisConfigurationError fp genesisConfigError -> "Genesis configuration error in: " <> toS fp
                                                       <> " Error: " <> Text.pack (show genesisConfigError)
    GenesisReadError fp err ->  "There was an error parsing the genesis file: " <> toS fp
                                <> " Error: " <> Text.pack (show err)
    -- TODO: Implement ByronLeaderCredentialsError render function in ouroboros-network
    CredentialsError byronLeaderCredentialsError -> "Byron leader credentials error: " <> Text.pack (show byronLeaderCredentialsError)
    SigningKeyDeserialiseFailure fp -> "Signing key deserialisation error in: " <> toS fp
    SigningKeyFilepathNotSpecified -> "Signing key filepath not specified"
