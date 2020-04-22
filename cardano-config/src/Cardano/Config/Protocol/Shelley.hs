{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Config.Protocol.Shelley
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

import           Ouroboros.Consensus.Node.NetworkProtocolVersion (HasNetworkProtocolVersion (..))
import           Ouroboros.Consensus.Cardano hiding (Protocol)

import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)

import           Shelley.Spec.Ledger.PParams (ProtVer(..))

import           Cardano.Config.Types
                   (NodeConfiguration(..),
                    MiscellaneousFilepaths(..), GenesisFile (..),
                    Update (..), LastKnownBlockVersion (..))
import           Cardano.Config.Shelley.Genesis ()
import           Cardano.Config.Protocol.Types (SomeConsensusProtocol (..))
import           Cardano.TracingOrphanInstances.Shelley ()


------------------------------------------------------------------------------
-- Shelley protocol
--

mkConsensusProtocolTPraos
  :: NodeConfiguration
  -> Maybe MiscellaneousFilepaths
  -> ExceptT ShelleyProtocolInstantiationError IO
             SomeConsensusProtocol
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

        proxy :: Proxy (ShelleyBlock TPraosStandardCrypto)
        proxy = Proxy

    return $ SomeConsensusProtocol consensusProtocol
                                   (supportedNodeToNodeVersions proxy)
                                   (supportedNodeToClientVersions proxy)


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


-- TODO: support reading leader credentials
readLeaderCredentials :: Maybe MiscellaneousFilepaths
                      -> ExceptT ShelleyProtocolInstantiationError IO
                                 (Maybe (TPraosLeaderCredentials TPraosStandardCrypto))
readLeaderCredentials _files = return Nothing


------------------------------------------------------------------------------
-- Errors
--

data ShelleyProtocolInstantiationError =
    GenesisReadError !FilePath !String
  deriving Show


renderShelleyProtocolInstantiationError :: ShelleyProtocolInstantiationError
                                        -> Text
renderShelleyProtocolInstantiationError pie =
  case pie of
    GenesisReadError fp err ->
        "There was an error parsing the genesis file: "
     <> toS fp <> " Error: " <> (T.pack $ show err)
