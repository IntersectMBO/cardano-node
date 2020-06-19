{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Config.Shelley.Protocol
  (
    -- * Protocol exposing the specific type
    -- | Use this when you need the specific instance
    mkConsensusProtocolShelley

    -- * Protocols hiding the specific type
    -- | Use this when you want to handle protocols generically
  , mkSomeConsensusProtocolShelley

    -- * Client support
  , mkNodeClientProtocolShelley
  , mkSomeNodeClientProtocolShelley

    -- * Errors
  , ShelleyProtocolInstantiationError(..)
  , renderShelleyProtocolInstantiationError

    -- * Reusable parts
  , readGenesis
  , readLeaderCredentials
  ) where

import           Cardano.Prelude
import           Prelude (String)

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)
import qualified Data.Text as T

import qualified Data.Aeson as Aeson

import qualified Ouroboros.Consensus.Cardano as Consensus

import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Protocol
                   (TPraosStandardCrypto, TPraosIsCoreNode(..))
import           Ouroboros.Consensus.Shelley.Node
                   (TPraosLeaderCredentials(..), ShelleyGenesis)

import           Shelley.Spec.Ledger.PParams (ProtVer(..))

import           Cardano.Config.Types
                   (NodeShelleyProtocolConfiguration(..),
                    ProtocolFilepaths(..), GenesisFile (..),
                    SomeConsensusProtocol(..), SomeNodeClientProtocol(..))
import           Cardano.Config.Shelley.OCert
import           Cardano.Config.Shelley.VRF
import           Cardano.Config.Shelley.KES
import           Cardano.TracingOrphanInstances.Shelley ()


------------------------------------------------------------------------------
-- Shelley protocol, client support
--

mkNodeClientProtocolShelley :: Consensus.ProtocolClient
                                 (ShelleyBlock TPraosStandardCrypto)
                                 Consensus.ProtocolRealTPraos
mkNodeClientProtocolShelley = Consensus.ProtocolClientRealTPraos


mkSomeNodeClientProtocolShelley :: SomeNodeClientProtocol
mkSomeNodeClientProtocolShelley =
    SomeNodeClientProtocol mkNodeClientProtocolShelley


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
             (Consensus.Protocol IO (ShelleyBlock TPraosStandardCrypto)
                                 Consensus.ProtocolRealTPraos)
mkConsensusProtocolShelley NodeShelleyProtocolConfiguration {
                            npcShelleyGenesisFile,
                            npcShelleySupportedProtocolVersionMajor,
                            npcShelleySupportedProtocolVersionMinor,
                            npcShelleyMaxSupportedProtocolVersion
                          }
                          files = do
    genesis <- readGenesis npcShelleyGenesisFile
    optionalLeaderCredentials <- readLeaderCredentials files

    return $
      Consensus.ProtocolRealTPraos
        genesis
        (ProtVer npcShelleySupportedProtocolVersionMajor
                 npcShelleySupportedProtocolVersionMinor)
        npcShelleyMaxSupportedProtocolVersion
        optionalLeaderCredentials


readGenesis :: GenesisFile
            -> ExceptT ShelleyProtocolInstantiationError IO
                       (ShelleyGenesis TPraosStandardCrypto)
readGenesis (GenesisFile file) =
    firstExceptT (GenesisReadError file) $
      ExceptT $ handle (\(e :: IOException) -> return $ Left $ show e) $
        Aeson.eitherDecodeFileStrict' file


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

    (opcert, vkey) <- firstExceptT OCertError $ readOperationalCert certFile
    vrfKey <- firstExceptT VRFError $ readVRFSigningKey vrfFile
    kesKey <- firstExceptT KESError $ readKESSigningKey kesFile

    return $ Just TPraosLeaderCredentials {
               tpraosLeaderCredentialsIsCoreNode =
                 TPraosIsCoreNode {
                   tpraosIsCoreNodeOpCert     = opcert,
                   tpraosIsCoreNodeColdVerKey = vkey,
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

data ShelleyProtocolInstantiationError = GenesisReadError !FilePath !String
                                       | OCertError OperationalCertError
                                       | VRFError VRFError
                                       | KESError KESError

                                       | OCertNotSpecified
                                       | VRFKeyNotSpecified
                                       | KESKeyNotSpecified
                                       deriving Show


renderShelleyProtocolInstantiationError :: ShelleyProtocolInstantiationError
                                        -> Text
renderShelleyProtocolInstantiationError pie =
  case pie of
    GenesisReadError fp err ->
        "There was an error parsing the genesis file: "
     <> toS fp <> " Error: " <> (T.pack $ show err)

    KESError   err -> renderKESError err
    VRFError   err -> renderVRFError err
    OCertError err -> T.pack $ show err --TODO: renderOperationalCertError

    OCertNotSpecified  -> missingFlagMessage "shelley-operational-certificate"
    VRFKeyNotSpecified -> missingFlagMessage "shelley-vrf-key"
    KESKeyNotSpecified -> missingFlagMessage "shelley-kes-key"
  where
    missingFlagMessage flag =
      "To create blocks, the --" <> flag <> " must also be specified"
