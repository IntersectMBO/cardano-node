{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Config.Shelley.Protocol
  ( mkConsensusProtocolTPraos
  , ShelleyProtocolInstantiationError(..)
  , renderShelleyProtocolInstantiationError
  ) where

import           Cardano.Prelude
import           Prelude (String)

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)
import qualified Data.Text as T

import qualified Data.Aeson as Aeson

import           Ouroboros.Consensus.Cardano hiding (Protocol)
import qualified Ouroboros.Consensus.Cardano as Consensus

import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Protocol
                   (TPraosStandardCrypto, TPraosIsCoreNode(..))
import           Ouroboros.Consensus.Shelley.Node
                   (TPraosLeaderCredentials(..))

import           Shelley.Spec.Ledger.PParams (ProtVer(..))
import qualified Shelley.Spec.Ledger.Keys as Ledger

import           Cardano.Config.Types
                   (NodeConfiguration(..), ProtocolFilepaths(..),
                    GenesisFile (..), Update (..), LastKnownBlockVersion (..))
import           Cardano.Config.Shelley.OCert
import           Cardano.Config.Shelley.VRF
import           Cardano.Config.Shelley.KES
import           Cardano.TracingOrphanInstances.Shelley ()


------------------------------------------------------------------------------
-- Shelley protocol
--

mkConsensusProtocolTPraos
  :: NodeConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT ShelleyProtocolInstantiationError IO
             (Consensus.Protocol (ShelleyBlock TPraosStandardCrypto)
                                 ProtocolRealTPraos)
mkConsensusProtocolTPraos NodeConfiguration {
                              ncGenesisFile,
                              ncUpdate
                            }
                            files = do
    genesis <- readShelleyGenesis ncGenesisFile

    let protocolVersion = toShelleyProtocolVersion ncUpdate

    optionalLeaderCredentials <- readLeaderCredentials files

    let consensusProtocol =
          ProtocolRealTPraos
            genesis
            protocolVersion
            optionalLeaderCredentials

    return consensusProtocol


readShelleyGenesis :: GenesisFile
                   -> ExceptT ShelleyProtocolInstantiationError IO
                              (ShelleyGenesis TPraosStandardCrypto)
readShelleyGenesis (GenesisFile file) =
    firstExceptT (GenesisReadError file) $
      ExceptT $
        Aeson.eitherDecodeFileStrict' file


-- | We reuse the Byron config file's last known block version config
-- which has a three-component version number, but we only use two.
--
toShelleyProtocolVersion :: Update -> ProtVer
toShelleyProtocolVersion (Update _appName _appVer lastKnownBlockVersion) =
    ProtVer (fromIntegral lkbvMajor)
            (fromIntegral lkbvMinor)
  where
    LastKnownBlockVersion
      {lkbvMajor, lkbvMinor, lkbvAlt = _unused} = lastKnownBlockVersion


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
    Ledger.SKeyES kesKey <- firstExceptT KESError $ readKESSigningKey kesFile

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

