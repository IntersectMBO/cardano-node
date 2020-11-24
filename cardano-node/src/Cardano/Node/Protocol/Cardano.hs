{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Node.Protocol.Cardano
  (
    -- * Protocol exposing the specific type
    -- | Use this when you need the specific instance
    mkConsensusProtocolCardano

    -- * Protocols hiding the specific type
    -- | Use this when you want to handle protocols generically
  , mkSomeConsensusProtocolCardano

    -- * Errors
  , CardanoProtocolInstantiationError(..)
  , renderCardanoProtocolInstantiationError
  ) where

import           Prelude

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)
import qualified Data.Text as T

import qualified Cardano.Chain.Update as Byron

import           Ouroboros.Consensus.Cardano hiding (Protocol)
import qualified Ouroboros.Consensus.Cardano as Consensus
import qualified Ouroboros.Consensus.Cardano.CanHardFork as Consensus
import           Ouroboros.Consensus.HardFork.Combinator.Condense ()

import           Ouroboros.Consensus.Cardano.Condense ()

import           Ouroboros.Consensus.Shelley.Protocol (StandardCrypto)

import           Cardano.Node.Types

import           Cardano.Tracing.OrphanInstances.Byron ()
import           Cardano.Tracing.OrphanInstances.Shelley ()

import qualified Cardano.Node.Protocol.Byron as Byron
import qualified Cardano.Node.Protocol.Shelley as Shelley

import           Cardano.Node.Protocol.Types

------------------------------------------------------------------------------
-- Real Cardano protocol
--

-- | Make 'SomeConsensusProtocol' using the Cardano instance.
--
-- The Cardano protocol instance is currently the sequential composition of
-- the Byron and Shelley protocols, and will likely be extended in future
-- with further sequentially composed protocol revisions.
--
-- The use of 'SomeConsensusProtocol' lets us handle multiple protocols in a
-- generic way.
--
-- This also serves a purpose as a sanity check that we have all the necessary
-- type class instances available.
--
mkSomeConsensusProtocolCardano
  :: NodeByronProtocolConfiguration
  -> NodeShelleyProtocolConfiguration
  -> NodeHardForkProtocolConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT CardanoProtocolInstantiationError IO SomeConsensusProtocol
mkSomeConsensusProtocolCardano ncb ncs nch files =

    -- Applying the SomeConsensusProtocol here is a check that
    -- the type of mkConsensusProtocolCardano fits all the class
    -- constraints we need to run the protocol.
    SomeConsensusProtocol
      <$> mkConsensusProtocolCardano ncb ncs nch files


-- | Instantiate 'Consensus.Protocol' for Byron specifically.
--
-- Use this when you need to run the consensus with this specific protocol.
--
mkConsensusProtocolCardano
  :: NodeByronProtocolConfiguration
  -> NodeShelleyProtocolConfiguration
  -> NodeHardForkProtocolConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT CardanoProtocolInstantiationError IO
             (Consensus.Protocol IO (CardanoBlock StandardCrypto)
                                    ProtocolCardano)
mkConsensusProtocolCardano NodeByronProtocolConfiguration {
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
                           NodeShelleyProtocolConfiguration {
                             npcShelleyGenesisFile,
                             npcShelleyGenesisFileHash,
                             npcShelleySupportedProtocolVersionMajor,
                             npcShelleySupportedProtocolVersionMinor
                           }
                           NodeHardForkProtocolConfiguration {
                             npcTestShelleyHardForkAtEpoch,
                             npcTestShelleyHardForkAtVersion,
                             npcTestAllegraHardForkAtEpoch,
                             npcTestAllegraHardForkAtVersion,
                             npcTestMaryHardForkAtEpoch,
                             npcTestMaryHardForkAtVersion
                           }
                           files = do
    byronGenesis <-
      firstExceptT CardanoProtocolInstantiationErrorByron $
        Byron.readGenesis npcByronGenesisFile
                          npcByronGenesisFileHash
                          npcByronReqNetworkMagic

    byronLeaderCredentials <-
      firstExceptT CardanoProtocolInstantiationErrorByron $
        Byron.readLeaderCredentials byronGenesis files

    (shelleyGenesis, shelleyGenesisHash) <-
      firstExceptT CardanoProtocolInstantiationErrorShelley $
        Shelley.readGenesis npcShelleyGenesisFile
                            npcShelleyGenesisFileHash

    shelleyLeaderCredentials <-
      firstExceptT CardanoProtocolInstantiationErrorShelley $
        Shelley.readLeaderCredentials files

    return $!
      Consensus.ProtocolCardano
        Consensus.ProtocolParamsByron {
          byronGenesis = byronGenesis,
          byronPbftSignatureThreshold =
            PBftSignatureThreshold <$> npcByronPbftSignatureThresh,
          byronProtocolVersion =
            Byron.ProtocolVersion
              npcByronSupportedProtocolVersionMajor
              npcByronSupportedProtocolVersionMinor
              npcByronSupportedProtocolVersionAlt,
          byronSoftwareVersion =
            Byron.SoftwareVersion
              npcByronApplicationName
              npcByronApplicationVersion,
          byronLeaderCredentials =
            byronLeaderCredentials
        }
        Consensus.ProtocolParamsShelley {
          shelleyGenesis = shelleyGenesis,
          shelleyInitialNonce =
            Shelley.genesisHashToPraosNonce shelleyGenesisHash,
          shelleyProtVer =
            ProtVer
              npcShelleySupportedProtocolVersionMajor
              npcShelleySupportedProtocolVersionMinor,
          shelleyLeaderCredentials =
            shelleyLeaderCredentials
        }
        Consensus.ProtocolParamsAllegra {
          allegraProtVer =
            ProtVer
              npcShelleySupportedProtocolVersionMajor
              npcShelleySupportedProtocolVersionMinor,
          allegraLeaderCredentials =
            -- TODO: separate credentials for Allega
            Nothing
        }
        Consensus.ProtocolParamsMary {
          maryProtVer =
            ProtVer
              npcShelleySupportedProtocolVersionMajor
              npcShelleySupportedProtocolVersionMinor,
          maryLeaderCredentials =
            -- TODO: separate credentials for Mary
            Nothing
        }
        -- ProtocolParamsTransition specifies the parameters needed to transition between two eras
        -- The comments below also apply for the Shelley -> Allegra and Allegra -> Mary hard forks.
        -- Byron to Shelley hard fork parameters
        Consensus.ProtocolParamsTransition {
          transitionTrigger =
            -- What will trigger the Byron -> Shelley hard fork?
            case npcTestShelleyHardForkAtEpoch of

               -- This specifies the major protocol version number update that will
               -- trigger us moving to the Shelley protocol.
               --
               -- Version 0 is Byron with Ouroboros classic
               -- Version 1 is Byron with Ouroboros Permissive BFT
               -- Version 2 is Shelley
               -- Version 3 is Allegra
               -- Version 4 is Mary
               --
               -- But we also provide an override to allow for simpler test setups
               -- such as triggering at the 0 -> 1 transition .
               --
               Nothing -> Consensus.TriggerHardForkAtVersion
                            (maybe 2 fromIntegral npcTestShelleyHardForkAtVersion)

               -- Alternatively, for testing we can transition at a specific epoch.
               --
               Just epochNo -> Consensus.TriggerHardForkAtEpoch epochNo
        }
        -- Shelley to Allegra hard fork parameters
        Consensus.ProtocolParamsTransition {
          transitionTrigger =
            case npcTestAllegraHardForkAtEpoch of
               Nothing -> Consensus.TriggerHardForkAtVersion
                            (maybe 3 fromIntegral npcTestAllegraHardForkAtVersion)
               Just epochNo -> Consensus.TriggerHardForkAtEpoch epochNo
        }
        -- Allegra to Mary hard fork parameters
        Consensus.ProtocolParamsTransition {
          transitionTrigger =
            case npcTestMaryHardForkAtEpoch of
               Nothing -> Consensus.TriggerHardForkAtVersion
                            (maybe 4 fromIntegral npcTestMaryHardForkAtVersion)
               Just epochNo -> Consensus.TriggerHardForkAtEpoch epochNo
        }

------------------------------------------------------------------------------
-- Errors
--

data CardanoProtocolInstantiationError =
       CardanoProtocolInstantiationErrorByron
         Byron.ByronProtocolInstantiationError

     | CardanoProtocolInstantiationErrorShelley
         Shelley.ShelleyProtocolInstantiationError
  deriving Show

renderCardanoProtocolInstantiationError :: CardanoProtocolInstantiationError
                                        -> T.Text
renderCardanoProtocolInstantiationError
  (CardanoProtocolInstantiationErrorByron err) =
    Byron.renderByronProtocolInstantiationError err

renderCardanoProtocolInstantiationError
  (CardanoProtocolInstantiationErrorShelley err) =
    Shelley.renderShelleyProtocolInstantiationError err
