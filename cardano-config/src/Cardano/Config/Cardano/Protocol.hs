{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Config.Cardano.Protocol
  (
    -- * Protocol exposing the specific type
    -- | Use this when you need the specific instance
    mkConsensusProtocolCardano

    -- * Protocols hiding the specific type
    -- | Use this when you want to handle protocols generically
  , mkSomeConsensusProtocolCardano

    -- * Client support
  , mkNodeClientProtocolCardano
  , mkSomeNodeClientProtocolCardano

    -- * Errors
  , CardanoProtocolInstantiationError(..)
  , renderCardanoProtocolInstantiationError
  ) where

import           Prelude

import qualified Data.Text as T
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)

import           Cardano.Chain.Slotting (EpochSlots)

import qualified Cardano.Chain.Update as Byron

import           Ouroboros.Consensus.Cardano hiding (Protocol)
import qualified Ouroboros.Consensus.Cardano as Consensus
import qualified Ouroboros.Consensus.Cardano.CanHardFork as Consensus
import           Ouroboros.Consensus.HardFork.Combinator.Condense ()

import           Ouroboros.Consensus.Cardano.Block (CardanoBlock)
import           Ouroboros.Consensus.Cardano.Condense ()

import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)
import qualified Shelley.Spec.Ledger.PParams as Shelley

import           Cardano.Config.Types
                   (NodeByronProtocolConfiguration(..),
                    NodeShelleyProtocolConfiguration(..),
                    ProtocolFilepaths(..), SomeConsensusProtocol(..),
                    SomeNodeClientProtocol(..),
                    HasKESMetricsData(..), KESMetricsData(..))

import           Cardano.TracingOrphanInstances.Byron ()
import           Cardano.TracingOrphanInstances.Shelley ()
import           Cardano.TracingOrphanInstances.HardFork ()

import qualified Cardano.Config.Byron.Protocol as Byron
import qualified Cardano.Config.Shelley.Protocol as Shelley


--TODO: move ToObject tracing instances to Cardano.TracingOrphanInstances.Consensus
--      and do them generically for the hard fork combinator
instance HasKESMetricsData (CardanoBlock c) where
    getKESMetricsData _protoInfo _forgeState = NoKESMetricsData
    --TODO distinguish on the era and use getKESMetricsData on the appropriate era


------------------------------------------------------------------------------
-- Real Cardano protocol, client support
--

mkNodeClientProtocolCardano :: EpochSlots
                            -> SecurityParam
                            -> ProtocolClient (CardanoBlock TPraosStandardCrypto)
                                              ProtocolCardano
mkNodeClientProtocolCardano epochSlots securityParam =
    ProtocolClientCardano epochSlots securityParam


mkSomeNodeClientProtocolCardano :: EpochSlots
                                -> SecurityParam
                                -> SomeNodeClientProtocol
mkSomeNodeClientProtocolCardano epochSlots securityParam =
    SomeNodeClientProtocol
      (mkNodeClientProtocolCardano epochSlots securityParam)


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
  -> Maybe ProtocolFilepaths
  -> ExceptT CardanoProtocolInstantiationError IO SomeConsensusProtocol
mkSomeConsensusProtocolCardano ncb ncs files =

    -- Applying the SomeConsensusProtocol here is a check that
    -- the type of mkConsensusProtocolCardano fits all the class
    -- constraints we need to run the protocol.
    SomeConsensusProtocol <$> mkConsensusProtocolCardano ncb ncs files


-- | Instantiate 'Consensus.Protocol' for Byron specifically.
--
-- Use this when you need to run the consensus with this specific protocol.
--
mkConsensusProtocolCardano
  :: NodeByronProtocolConfiguration
  -> NodeShelleyProtocolConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT CardanoProtocolInstantiationError IO
             (Consensus.Protocol IO (CardanoBlock TPraosStandardCrypto)
                                    ProtocolCardano)
mkConsensusProtocolCardano NodeByronProtocolConfiguration {
                             npcByronGenesisFile,
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
                             npcShelleySupportedProtocolVersionMajor,
                             npcShelleySupportedProtocolVersionMinor,
                             npcShelleyMaxSupportedProtocolVersion
                           }
                           files = do
    byronGenesis <-
      firstExceptT CardanoProtocolInstantiationErrorByron $
        Byron.readGenesis npcByronGenesisFile npcByronReqNetworkMagic

    byronLeaderCredentials <-
      firstExceptT CardanoProtocolInstantiationErrorByron $
        Byron.readLeaderCredentials byronGenesis files

    shelleyGenesis <-
      firstExceptT CardanoProtocolInstantiationErrorShelley $
        Shelley.readGenesis npcShelleyGenesisFile

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
        (Shelley.ProtVer npcShelleySupportedProtocolVersionMajor
                         npcShelleySupportedProtocolVersionMinor)
        npcShelleyMaxSupportedProtocolVersion
        shelleyLeaderCredentials

        -- Hard fork parameters
        (Just 190) --TODO: Optimisation: once the epoch of the transition is
                   -- known, set this to the first shelley epoch.
        (Consensus.NoHardCodedTransition (fromIntegral npcShelleyMaxSupportedProtocolVersion))
        --TODO is this the right value?


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

