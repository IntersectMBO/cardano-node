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

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Aeson as Aeson

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra
                   (firstExceptT, newExceptT, handleIOExceptT, hoistEither)

import qualified Cardano.Crypto.Hash.Class as Crypto

import qualified Ouroboros.Consensus.Cardano as Consensus
import           Ouroboros.Consensus.Cardano.ShelleyHFC

import           Ouroboros.Consensus.Shelley.Protocol
                   (TPraosStandardCrypto, TPraosIsCoreNode(..))
import           Ouroboros.Consensus.Shelley.Node
                   (TPraosLeaderCredentials(..), ShelleyGenesis, Nonce (..))

import           Shelley.Spec.Ledger.PParams (ProtVer(..))
import           Shelley.Spec.Ledger.Keys (coerceKeyRole)

import           Cardano.Api.Typed hiding (FileError)
import qualified Cardano.Api.Typed as Api (FileError)

import           Cardano.Node.Types
                   (NodeShelleyProtocolConfiguration(..), GenesisHash(..))
import           Cardano.Config.Types
                   (ProtocolFilepaths(..), GenesisFile (..))

import           Cardano.Tracing.OrphanInstances.Shelley ()
import           Cardano.Tracing.OrphanInstances.HardFork ()

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
             (Consensus.Protocol IO (ShelleyBlockHFC TPraosStandardCrypto)
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

    optionalLeaderCredentials <- readLeaderCredentials files

    return $
      Consensus.ProtocolShelley
        genesis
        (genesisHashToPraosNonce genesisHash)
        (ProtVer npcShelleySupportedProtocolVersionMajor
                 npcShelleySupportedProtocolVersionMinor)
        npcShelleyMaxSupportedProtocolVersion
        optionalLeaderCredentials

genesisHashToPraosNonce :: GenesisHash -> Nonce
genesisHashToPraosNonce (GenesisHash h) = Nonce (Crypto.castHash h)

readGenesis :: GenesisFile
            -> Maybe GenesisHash
            -> ExceptT ShelleyProtocolInstantiationError IO
                       (ShelleyGenesis TPraosStandardCrypto, GenesisHash)
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


readLeaderCredentials :: Maybe ProtocolFilepaths
                      -> ExceptT ShelleyProtocolInstantiationError IO
                                 (Maybe (TPraosLeaderCredentials TPraosStandardCrypto))

-- It's ok to supply none of the files
readLeaderCredentials Nothing = return Nothing
readLeaderCredentials (Just ProtocolFilepaths {
                              shelleyCertFile = Nothing,
                              shelleyVRFFile  = Nothing,
                              shelleyKESFile  = Nothing
                            }) = return Nothing

-- Or to supply all of the files
readLeaderCredentials (Just ProtocolFilepaths {
                              shelleyCertFile = Just certFile,
                              shelleyVRFFile  = Just vrfFile,
                              shelleyKESFile  = Just kesFile
                            }) = do

    OperationalCertificate opcert (StakePoolVerificationKey vkey) <-
      firstExceptT FileError . newExceptT $ readFileTextEnvelope AsOperationalCertificate certFile
    VrfSigningKey vrfKey <-
      firstExceptT FileError . newExceptT $ readFileTextEnvelope (AsSigningKey AsVrfKey) vrfFile
    KesSigningKey kesKey <-
      firstExceptT FileError . newExceptT $ readFileTextEnvelope (AsSigningKey AsKesKey) kesFile

    return $ Just TPraosLeaderCredentials {
               tpraosLeaderCredentialsIsCoreNode =
                 TPraosIsCoreNode {
                   tpraosIsCoreNodeOpCert     = opcert,
                   tpraosIsCoreNodeColdVerKey = coerceKeyRole vkey,
                   tpraosIsCoreNodeSignKeyVRF = vrfKey
                 },
               tpraosLeaderCredentialsSignKey = kesKey
             }

-- But not ok to supply some of the files without the others.
readLeaderCredentials (Just ProtocolFilepaths {shelleyCertFile = Nothing}) =
    throwError OCertNotSpecified
readLeaderCredentials (Just ProtocolFilepaths {shelleyVRFFile = Nothing}) =
    throwError VRFKeyNotSpecified
readLeaderCredentials (Just ProtocolFilepaths {shelleyKESFile = Nothing}) =
    throwError KESKeyNotSpecified


------------------------------------------------------------------------------
-- Errors
--

data ShelleyProtocolInstantiationError =
       GenesisReadError !FilePath !IOException
     | GenesisHashMismatch !GenesisHash !GenesisHash -- actual, expected
     | GenesisDecodeError !FilePath !String
     | FileError (Api.FileError TextEnvelopeError)
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
     <> toS fp <> " Error: " <> (T.pack $ show err)

    GenesisHashMismatch actual expected ->
        "Wrong Shelley genesis file: the actual hash is " <> show actual
     <> ", but the expected Shelley genesis hash given in the node "
     <> "configuration file is " <> show expected

    GenesisDecodeError fp err ->
        "There was an error parsing the genesis file: "
     <> toS fp <> " Error: " <> (T.pack $ show err)

    FileError fileErr -> T.pack $ displayError fileErr

    OCertNotSpecified  -> missingFlagMessage "shelley-operational-certificate"
    VRFKeyNotSpecified -> missingFlagMessage "shelley-vrf-key"
    KESKeyNotSpecified -> missingFlagMessage "shelley-kes-key"
  where
    missingFlagMessage flag =
      "To create blocks, the --" <> flag <> " must also be specified"
