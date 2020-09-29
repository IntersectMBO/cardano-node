{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Node.Protocol.Shelley
  (
    -- * Protocol exposing the specific type
    -- | Use this when you need the specific instance
    mkConsensusProtocolShelley

    -- * Protocols hiding the specific type
    -- | Use this when you want to handle protocols generically
  , mkSomeConsensusProtocolShelley

    -- * Errors
  , ShelleyProtocolInstantiationError(..)
  , renderShelleyProtocolInstantiationError

    -- * Reusable parts
  , readGenesis
  , readLeaderCredentials
  , genesisHashToPraosNonce
  ) where

import           Cardano.Prelude
import           Prelude (String, id)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.Text as T

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither,
                     newExceptT)

import qualified Cardano.Crypto.Hash.Class as Crypto

import qualified Ouroboros.Consensus.Cardano as Consensus
import           Ouroboros.Consensus.Cardano.ShelleyHFC

import           Ouroboros.Consensus.Shelley.Node (MaxMajorProtVer (..), Nonce (..),
                     ShelleyGenesis, TPraosLeaderCredentials (..))
import           Ouroboros.Consensus.Shelley.Protocol (StandardShelley, TPraosCanBeLeader (..))

import           Shelley.Spec.Ledger.Genesis (ValidationErr (..), describeValidationErr,
                     validateGenesis)
import           Shelley.Spec.Ledger.Keys (coerceKeyRole)
import           Shelley.Spec.Ledger.PParams (ProtVer (..))

import           Cardano.Api.Typed hiding (FileError)
import qualified Cardano.Api.Typed as Api (FileError)

import           Cardano.Node.Types

import           Cardano.Tracing.OrphanInstances.HardFork ()
import           Cardano.Tracing.OrphanInstances.Shelley ()

import           Cardano.Node.Protocol.Types


------------------------------------------------------------------------------
-- Shelley protocol
--

-- | Make 'SomeConsensusProtocol' using the Shelley instance.
--
-- This lets us handle multiple protocols in a generic way.
--
-- This also serves a purpose as a sanity check that we have all the necessary
-- type class instances available.
--
mkSomeConsensusProtocolShelley
  :: NodeShelleyProtocolConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT ShelleyProtocolInstantiationError IO SomeConsensusProtocol
mkSomeConsensusProtocolShelley nc files =

    -- Applying the SomeConsensusProtocol here is a check that
    -- the type of mkConsensusProtocolShelley fits all the class
    -- constraints we need to run the protocol.
    SomeConsensusProtocol <$> mkConsensusProtocolShelley nc files


-- | Instantiate 'Consensus.Protocol' for Shelley specifically.
--
-- Use this when you need to run the consensus with this specific protocol.
--
mkConsensusProtocolShelley
  :: NodeShelleyProtocolConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT ShelleyProtocolInstantiationError IO
             (Consensus.Protocol IO (ShelleyBlockHFC StandardShelley)
                                 Consensus.ProtocolShelley)
mkConsensusProtocolShelley NodeShelleyProtocolConfiguration {
                            npcShelleyGenesisFile,
                            npcShelleyGenesisFileHash,
                            npcShelleySupportedProtocolVersionMajor,
                            npcShelleySupportedProtocolVersionMinor,
                            npcShelleyMaxSupportedProtocolVersion
                          }
                          files = do
    (genesis, genesisHash) <- readGenesis npcShelleyGenesisFile
                                          npcShelleyGenesisFileHash
    firstExceptT GenesisValidationFailure . hoistEither $ validateGenesis genesis
    optionalLeaderCredentials <- readLeaderCredentials files

    return $
      Consensus.ProtocolShelley
        genesis
        (genesisHashToPraosNonce genesisHash)
        (ProtVer npcShelleySupportedProtocolVersionMajor
                 npcShelleySupportedProtocolVersionMinor)
        (MaxMajorProtVer npcShelleyMaxSupportedProtocolVersion)
        optionalLeaderCredentials

genesisHashToPraosNonce :: GenesisHash -> Nonce
genesisHashToPraosNonce (GenesisHash h) = Nonce (Crypto.castHash h)

readGenesis :: GenesisFile
            -> Maybe GenesisHash
            -> ExceptT ShelleyProtocolInstantiationError IO
                       (ShelleyGenesis StandardShelley, GenesisHash)
readGenesis (GenesisFile file) mbExpectedGenesisHash = do
    content <- handleIOExceptT (GenesisReadError file) $
                 BS.readFile file
    let genesisHash = GenesisHash (Crypto.hashWith id content)
    checkExpectedGenesisHash genesisHash
    genesis <- firstExceptT (GenesisDecodeError file) $ hoistEither $
                 Aeson.eitherDecodeStrict' content
    return (genesis, genesisHash)
  where
    checkExpectedGenesisHash :: GenesisHash
                             -> ExceptT ShelleyProtocolInstantiationError IO ()
    checkExpectedGenesisHash actual =
      case mbExpectedGenesisHash of
        Just expected | actual /= expected
          -> throwError (GenesisHashMismatch actual expected)
        _ -> return ()


data ShelleyCredentialsFiles =
  ShelleyCredentialsFiles
  { certFile :: !FilePath
  , vrfFile  :: !FilePath
  , kesFile  :: !FilePath
  } deriving (Generic)

instance FromJSON ShelleyCredentialsFiles

readLeaderCredentials :: Maybe ProtocolFilepaths
                      -> ExceptT ShelleyProtocolInstantiationError IO
                                 [TPraosLeaderCredentials StandardShelley]

readLeaderCredentials Nothing = return []
readLeaderCredentials (Just pfp) =
  interpretProtocolFilepaths pfp
  >>= mapM readShelleyCredentials

interpretProtocolFilepaths ::
     ProtocolFilepaths
  -> ExceptT ShelleyProtocolInstantiationError IO
             [ShelleyCredentialsFiles]
interpretProtocolFilepaths pfp =
  -- The set of credentials files is a sum total of what comes from the CLI,
  -- as well as what is referred to by the bulk credentials file.
  (<>) <$> interpCLI pfp <*> interpBulkFile pfp
 where
   -- It's OK to supply none of the files on the CLI
   interpCLI ProtocolFilepaths
     { shelleyCertFile      = Nothing,
       shelleyVRFFile       = Nothing,
       shelleyKESFile       = Nothing
     } = pure []
   -- Or to supply all of the files
   interpCLI ProtocolFilepaths
     { shelleyCertFile      = Just certFile,
       shelleyVRFFile       = Just vrfFile,
       shelleyKESFile       = Just kesFile
     } = pure [ShelleyCredentialsFiles certFile vrfFile kesFile]
   -- But not OK to supply some of the files without the others.
   interpCLI ProtocolFilepaths {shelleyCertFile = Nothing} =
     throwError OCertNotSpecified
   interpCLI ProtocolFilepaths {shelleyVRFFile = Nothing} =
     throwError VRFKeyNotSpecified
   interpCLI ProtocolFilepaths {shelleyKESFile = Nothing} =
     throwError KESKeyNotSpecified

   interpBulkFile ProtocolFilepaths
     { shelleyBulkCredsFile = Nothing
     } = pure []
   interpBulkFile ProtocolFilepaths
     { shelleyBulkCredsFile = Just bulkFile
     } = readBulkFile bulkFile

   readBulkFile :: FilePath
                -> ExceptT ShelleyProtocolInstantiationError IO
                           [ShelleyCredentialsFiles]
   readBulkFile fp = do
     content <- handleIOExceptT (GenesisReadError fp) $
                  BS.readFile fp
     firstExceptT (GenesisDecodeError fp) $ hoistEither $
       Aeson.eitherDecodeStrict' content

readShelleyCredentials ::
     ShelleyCredentialsFiles
  -> ExceptT ShelleyProtocolInstantiationError IO
             (TPraosLeaderCredentials StandardShelley)
readShelleyCredentials ShelleyCredentialsFiles
                       { certFile, vrfFile, kesFile
                       } = do
    OperationalCertificate opcert (StakePoolVerificationKey vkey) <-
      firstExceptT FileError . newExceptT $ readFileTextEnvelope AsOperationalCertificate certFile
    VrfSigningKey vrfKey <-
      firstExceptT FileError . newExceptT $ readFileTextEnvelope (AsSigningKey AsVrfKey) vrfFile
    KesSigningKey kesKey <-
      firstExceptT FileError . newExceptT $ readFileTextEnvelope (AsSigningKey AsKesKey) kesFile

    return $ TPraosLeaderCredentials {
               tpraosLeaderCredentialsCanBeLeader =
                 TPraosCanBeLeader {
                   tpraosCanBeLeaderOpCert     = opcert,
                   tpraosCanBeLeaderColdVerKey = coerceKeyRole vkey,
                   tpraosCanBeLeaderSignKeyVRF = vrfKey
                 },
               tpraosLeaderCredentialsInitSignKey = kesKey
             }


------------------------------------------------------------------------------
-- Errors
--

data ShelleyProtocolInstantiationError =
       GenesisReadError !FilePath !IOException
     | GenesisHashMismatch !GenesisHash !GenesisHash -- actual, expected
     | GenesisDecodeError !FilePath !String
     | GenesisValidationFailure ![ValidationErr]
     | FileError !(Api.FileError TextEnvelopeError)
--TODO: pick a less generic constructor than FileError

     | OCertNotSpecified
     | VRFKeyNotSpecified
     | KESKeyNotSpecified
     deriving Show


renderShelleyProtocolInstantiationError :: ShelleyProtocolInstantiationError
                                        -> Text
renderShelleyProtocolInstantiationError pie =
  case pie of
    GenesisReadError fp err ->
        "There was an error reading the genesis file: "
     <> toS fp <> " Error: " <> T.pack (show err)

    GenesisHashMismatch actual expected ->
        "Wrong Shelley genesis file: the actual hash is " <> show actual
     <> ", but the expected Shelley genesis hash given in the node "
     <> "configuration file is " <> show expected

    GenesisDecodeError fp err ->
        "There was an error parsing the genesis file: "
     <> toS fp <> " Error: " <> T.pack (show err)

    GenesisValidationFailure vErrs -> T.unlines $ map describeValidationErr vErrs

    FileError fileErr -> T.pack $ displayError fileErr

    OCertNotSpecified  -> missingFlagMessage "shelley-operational-certificate"
    VRFKeyNotSpecified -> missingFlagMessage "shelley-vrf-key"
    KESKeyNotSpecified -> missingFlagMessage "shelley-kes-key"
  where
    missingFlagMessage flag =
      "To create blocks, the --" <> flag <> " must also be specified"
