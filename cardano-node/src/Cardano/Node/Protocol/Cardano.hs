{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Node.Protocol.Cardano
  ( mkSomeConsensusProtocolCardano

    -- * Errors
  , CardanoProtocolInstantiationError(..)
  ) where

import           Cardano.Api
import           Cardano.Api.Byron as Byron

import qualified Cardano.Chain.Update as Update
import qualified Cardano.Ledger.Api.Transition as Ledger
import           Cardano.Ledger.BaseTypes (natVersion)
import qualified Cardano.Node.Protocol.Alonzo as Alonzo
import qualified Cardano.Node.Protocol.Byron as Byron
import           Cardano.Node.Protocol.Checkpoints
import qualified Cardano.Node.Protocol.Conway as Conway
import qualified Cardano.Node.Protocol.Shelley as Shelley
import           Cardano.Node.Protocol.Types
import           Cardano.Node.Types
import           Cardano.Tracing.OrphanInstances.Byron ()
import           Cardano.Tracing.OrphanInstances.Shelley ()
import           Data.Function ((&))
import           Ouroboros.Consensus.Cardano
import qualified Ouroboros.Consensus.Cardano as Consensus
import           Ouroboros.Consensus.Cardano.Condense ()
import qualified Ouroboros.Consensus.Cardano.Node as Consensus
import           Ouroboros.Consensus.HardFork.Combinator.Condense ()

import           Prelude

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
  -> NodeAlonzoProtocolConfiguration
  -> NodeConwayProtocolConfiguration
  -> NodeHardForkProtocolConfiguration
  -> NodeCheckpointsConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT CardanoProtocolInstantiationError IO SomeConsensusProtocol
mkSomeConsensusProtocolCardano NodeByronProtocolConfiguration {
                             npcByronGenesisFile,
                             npcByronGenesisFileHash,
                             npcByronReqNetworkMagic,
                             npcByronPbftSignatureThresh,
                             npcByronSupportedProtocolVersionMajor,
                             npcByronSupportedProtocolVersionMinor,
                             npcByronSupportedProtocolVersionAlt
                           }
                           NodeShelleyProtocolConfiguration {
                             npcShelleyGenesisFile,
                             npcShelleyGenesisFileHash
                           }
                           NodeAlonzoProtocolConfiguration {
                             npcAlonzoGenesisFile,
                             npcAlonzoGenesisFileHash
                           }
                           NodeConwayProtocolConfiguration {
                             npcConwayGenesisFile,
                             npcConwayGenesisFileHash
                           }
                           npc@NodeHardForkProtocolConfiguration {
                            -- During testing of the Alonzo era, we conditionally declared that we
                            -- knew about the Alonzo era. We do so only when a config option for
                            -- testing development/unstable eras is used. This lets us include
                            -- not-yet-ready eras in released node versions without mainnet nodes
                            -- prematurely advertising that they could hard fork into the new era.
                             npcTestShelleyHardForkAtEpoch,
                             npcTestAllegraHardForkAtEpoch,
                             npcTestMaryHardForkAtEpoch,
                             npcTestAlonzoHardForkAtEpoch,
                             npcTestBabbageHardForkAtEpoch,
                             npcTestConwayHardForkAtEpoch
                           }
                           checkpointsConfiguration
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
      firstExceptT CardanoProtocolInstantiationShelleyGenesisReadError $
        Shelley.readGenesis npcShelleyGenesisFile
                            npcShelleyGenesisFileHash

    (alonzoGenesis, _alonzoGenesisHash) <-
      firstExceptT CardanoProtocolInstantiationAlonzoGenesisReadError $
        case npcTestStartingEra npc of
          Nothing ->
            Alonzo.readGenesis Nothing
                               npcAlonzoGenesisFile
                               npcAlonzoGenesisFileHash
          Just (AnyShelleyBasedEra sbe) -> do
            Alonzo.readGenesis (Just $ toCardanoEra sbe)
                               npcAlonzoGenesisFile
                               npcAlonzoGenesisFileHash

    (conwayGenesis, _conwayGenesisHash) <-
      firstExceptT CardanoProtocolInstantiationConwayGenesisReadError $
        Conway.readGenesis npcConwayGenesisFile
                                npcConwayGenesisFileHash

    shelleyLeaderCredentials <-
      firstExceptT CardanoProtocolInstantiationPraosLeaderCredentialsError $
        Shelley.readLeaderCredentials files

    checkpointsMap <-
      firstExceptT CardanoProtocolInstantiationCheckpointsReadError $
        readCheckpointsMap checkpointsConfiguration

    return $!
      SomeConsensusProtocol CardanoBlockType $ ProtocolInfoArgsCardano $ Consensus.CardanoProtocolParams {
        Consensus.byronProtocolParams =
        Consensus.ProtocolParamsByron {
          byronGenesis = byronGenesis,
          byronPbftSignatureThreshold =
            PBftSignatureThreshold <$> npcByronPbftSignatureThresh,

          -- This is /not/ the Byron protocol version. It is the protocol
          -- version that this node will use in blocks it creates. It is used
          -- in the Byron update mechanism to signal that this block-producing
          -- node is ready to move to the new protocol. For example, when the
          -- protocol version (according to the ledger state) is 0, this setting
          -- should be 1 when we are ready to move. Similarly when the current
          -- protocol version is 1, this should be 2 to indicate we are ready
          -- to move into the Shelley era.
          byronProtocolVersion =
            Update.ProtocolVersion
              npcByronSupportedProtocolVersionMajor
              npcByronSupportedProtocolVersionMinor
              npcByronSupportedProtocolVersionAlt,
          byronSoftwareVersion = Byron.softwareVersion,
          byronLeaderCredentials =
            byronLeaderCredentials
        }
      , Consensus.shelleyBasedProtocolParams =
        Consensus.ProtocolParamsShelleyBased {
          shelleyBasedInitialNonce      = Shelley.genesisHashToPraosNonce
                                            shelleyGenesisHash,
          shelleyBasedLeaderCredentials = shelleyLeaderCredentials
        }
      , Consensus.cardanoProtocolVersion = ProtVer (natVersion @10) 2
        -- The remaining arguments specify the parameters needed to transition between two eras
      , Consensus.cardanoLedgerTransitionConfig =
          Ledger.mkLatestTransitionConfig
            shelleyGenesis
            alonzoGenesis
            conwayGenesis
      , Consensus.cardanoHardForkTriggers =
        Consensus.CardanoHardForkTriggers' {
          triggerHardForkShelley =
            -- What will trigger the Byron -> Shelley hard fork?
            npcTestShelleyHardForkAtEpoch & maybe

               -- This specifies the major protocol version number update that will
               -- trigger us moving to the Shelley protocol.
               --
               -- Version 0 is Byron with Ouroboros classic
               -- Version 1 is Byron with Ouroboros Permissive BFT
               -- Version 2 is Shelley
               -- Version 3 is Allegra
               -- Version 4 is Mary
               -- Version 5 is Alonzo
               -- Version 6 is Alonzo (intra era hardfork)
               -- Version 7 is Babbage
               -- Version 8 is Babbage (intra era hardfork)
               -- Version 9 is Conway (bootstrap era)
               -- Version 10 is Conway + 1
               --
               -- But we also provide an override to allow for simpler test setups
               -- such as triggering at the 0 -> 1 transition .
               --
               Consensus.CardanoTriggerHardForkAtDefaultVersion

               -- Alternatively, for testing we can transition at a specific epoch.
               --
               Consensus.CardanoTriggerHardForkAtEpoch
        , triggerHardForkAllegra =
            npcTestAllegraHardForkAtEpoch &
              maybe
                Consensus.CardanoTriggerHardForkAtDefaultVersion
                Consensus.CardanoTriggerHardForkAtEpoch
        , triggerHardForkMary =
            npcTestMaryHardForkAtEpoch &
              maybe
                Consensus.CardanoTriggerHardForkAtDefaultVersion
                Consensus.CardanoTriggerHardForkAtEpoch
        , triggerHardForkAlonzo =
            npcTestAlonzoHardForkAtEpoch &
              maybe
                Consensus.CardanoTriggerHardForkAtDefaultVersion
                Consensus.CardanoTriggerHardForkAtEpoch
        , triggerHardForkBabbage =
            npcTestBabbageHardForkAtEpoch &
              maybe
                Consensus.CardanoTriggerHardForkAtDefaultVersion
                Consensus.CardanoTriggerHardForkAtEpoch
        , triggerHardForkConway =
            npcTestConwayHardForkAtEpoch &
              maybe
                Consensus.CardanoTriggerHardForkAtDefaultVersion
                Consensus.CardanoTriggerHardForkAtEpoch
        }
      , Consensus.cardanoCheckpoints = checkpointsMap
      }

        ----------------------------------------------------------------------
        -- WARNING When adding new entries above, be aware that if there is an
        -- intra-era fork, then the numbering is not consecutive.
        ----------------------------------------------------------------------

------------------------------------------------------------------------------
-- Errors
--

data CardanoProtocolInstantiationError =
       CardanoProtocolInstantiationErrorByron
         Byron.ByronProtocolInstantiationError

     | CardanoProtocolInstantiationShelleyGenesisReadError
         Shelley.GenesisReadError

     | CardanoProtocolInstantiationAlonzoGenesisReadError
         Shelley.GenesisReadError

     | CardanoProtocolInstantiationConwayGenesisReadError
         Shelley.GenesisReadError

     | CardanoProtocolInstantiationPraosLeaderCredentialsError
         Shelley.PraosLeaderCredentialsError

     | CardanoProtocolInstantiationErrorAlonzo
         Alonzo.AlonzoProtocolInstantiationError

     | CardanoProtocolInstantiationCheckpointsReadError
         CheckpointsReadError
  deriving Show

instance Error CardanoProtocolInstantiationError where
  prettyError (CardanoProtocolInstantiationErrorByron err) =
    prettyError err
  prettyError (CardanoProtocolInstantiationShelleyGenesisReadError err) =
    "Shelley related: " <> prettyError err
  prettyError (CardanoProtocolInstantiationAlonzoGenesisReadError err) =
    "Alonzo related: " <> prettyError err
  prettyError (CardanoProtocolInstantiationConwayGenesisReadError err) =
    "Conway related : " <> prettyError err
  prettyError (CardanoProtocolInstantiationPraosLeaderCredentialsError err) =
    prettyError err
  prettyError (CardanoProtocolInstantiationErrorAlonzo err) =
    prettyError err
  prettyError (CardanoProtocolInstantiationCheckpointsReadError err) =
    prettyError err
