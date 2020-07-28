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
import           Data.SOP.Strict (NP ((:*), Nil))
import qualified Data.Text as T

import qualified Cardano.Chain.Update as Byron

import           Ouroboros.Consensus.Block (ForgeState)
import           Ouroboros.Consensus.Cardano hiding (Protocol)
import qualified Ouroboros.Consensus.Cardano as Consensus
import qualified Ouroboros.Consensus.Cardano.CanHardFork as Consensus
import           Ouroboros.Consensus.HardFork.Combinator.Condense ()
import           Ouroboros.Consensus.HardFork.Combinator.Forge
                     (distribForgeState)

import           Ouroboros.Consensus.Cardano.Block (CardanoBlock)
import           Ouroboros.Consensus.Cardano.Condense ()

import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)
import qualified Shelley.Spec.Ledger.PParams as Shelley

import           Cardano.Config.Types (HasKESMetricsData (..),
                     ProtocolFilepaths (..))
import           Cardano.Node.Types (NodeByronProtocolConfiguration (..),
                     NodeHardForkProtocolConfiguration (..),
                     NodeShelleyProtocolConfiguration (..))

import           Cardano.TracingOrphanInstances.Byron ()
import           Cardano.TracingOrphanInstances.Shelley ()

import qualified Cardano.Node.Protocol.Byron as Byron
import qualified Cardano.Node.Protocol.Shelley as Shelley

import           Cardano.Node.Protocol.Types



--TODO: move ToObject tracing instances to Cardano.TracingOrphanInstances.Consensus
--      and do them generically for the hard fork combinator
instance forall c. HasKESMetricsData (CardanoBlock c) where
  getKESMetricsData cardanoForgeState =
    let (_byronForgeState :* shelleyForgeState :* Nil) = distribForgeState cardanoForgeState
     in getKESMetricsData (shelleyForgeState :: ForgeState (ShelleyBlock c))

-- TODO: Ideally, we would like to distinguish here between whether we are in
-- the Byron or Shelley era, but that's currently not possible to determine
-- from the ForgeState alone.

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
    SomeConsensusProtocol <$> mkConsensusProtocolCardano ncb ncs nch files


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
             (Consensus.Protocol IO (CardanoBlock TPraosStandardCrypto)
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
                             npcShelleySupportedProtocolVersionMinor,
                             npcShelleyMaxSupportedProtocolVersion
                           }
                           NodeHardForkProtocolConfiguration {
                             npcTestShelleyHardForkAtEpoch,
                             npcTestShelleyHardForkAtVersion,
                             npcShelleyHardForkNotBeforeEpoch
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
        -- Byron parameters
        byronGenesis
        (PBftSignatureThreshold <$> npcByronPbftSignatureThresh)
        (Byron.ProtocolVersion npcByronSupportedProtocolVersionMajor
                               npcByronSupportedProtocolVersionMinor
                               npcByronSupportedProtocolVersionAlt)
        (Byron.SoftwareVersion npcByronApplicationName
                               npcByronApplicationVersion)
        byronLeaderCredentials

        -- Shelley parameters
        shelleyGenesis
        (Shelley.genesisHashToPraosNonce shelleyGenesisHash)
        (Shelley.ProtVer npcShelleySupportedProtocolVersionMajor
                         npcShelleySupportedProtocolVersionMinor)
        npcShelleyMaxSupportedProtocolVersion
        shelleyLeaderCredentials

        -- Hard fork parameters
        npcShelleyHardForkNotBeforeEpoch

        -- What will trigger the hard fork?
        (case npcTestShelleyHardForkAtEpoch of

           -- This specifies the major protocol version number update that will
           -- trigger us moving to the Shelley protocol.
           --
           -- Version 0 is Byron with Ouroboros classic
           -- Version 1 is Byron with Ouroboros Permissive BFT
           -- Version 2 is Shelley
           --
           -- But we also provide an override to allow for simpler test setups
           -- such as triggering at the 0 -> 1 transition .
           --
           Nothing -> Consensus.TriggerHardForkAtVersion
                        (maybe 2 fromIntegral npcTestShelleyHardForkAtVersion)

           -- Alternatively, for testing we can transition at a specific epoch.
           --
           Just epochNo -> Consensus.TriggerHardForkAtEpoch epochNo)


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
