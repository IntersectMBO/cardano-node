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
import qualified Cardano.Node.Protocol.Conway as Conway
import qualified Cardano.Node.Protocol.Shelley as Shelley
import           Cardano.Node.Protocol.Types
import           Cardano.Node.Types
import           Cardano.Tracing.OrphanInstances.Byron ()
import           Cardano.Tracing.OrphanInstances.Shelley ()
import           Ouroboros.Consensus.Cardano
import qualified Ouroboros.Consensus.Cardano as Consensus
import qualified Ouroboros.Consensus.Cardano.CanHardFork as Consensus
import           Ouroboros.Consensus.Cardano.Condense ()
import           Ouroboros.Consensus.HardFork.Combinator.Condense ()
import qualified Ouroboros.Consensus.Mempool.Capacity as TxLimits
import qualified Ouroboros.Consensus.Shelley.Node.Praos as Praos

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
                           NodeHardForkProtocolConfiguration {
                            npcExperimentalHardForksEnabled,
                            -- During testing of the Alonzo era, we conditionally declared that we
                            -- knew about the Alonzo era. We do so only when a config option for
                            -- testing development/unstable eras is used. This lets us include
                            -- not-yet-ready eras in released node versions without mainnet nodes
                            -- prematurely advertising that they could hard fork into the new era.
                             npcTestShelleyHardForkAtEpoch,
                             npcTestShelleyHardForkAtVersion,
                             npcTestAllegraHardForkAtEpoch,
                             npcTestAllegraHardForkAtVersion,
                             npcTestMaryHardForkAtEpoch,
                             npcTestMaryHardForkAtVersion,
                             npcTestAlonzoHardForkAtEpoch,
                             npcTestAlonzoHardForkAtVersion,
                             npcTestBabbageHardForkAtEpoch,
                             npcTestBabbageHardForkAtVersion,
                             npcTestConwayHardForkAtEpoch,
                             npcTestConwayHardForkAtVersion
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
      firstExceptT CardanoProtocolInstantiationShelleyGenesisReadError $
        Shelley.readGenesis npcShelleyGenesisFile
                            npcShelleyGenesisFileHash

    (alonzoGenesis, _alonzoGenesisHash) <-
      firstExceptT CardanoProtocolInstantiationAlonzoGenesisReadError $
        Alonzo.readGenesis npcAlonzoGenesisFile
                           npcAlonzoGenesisFileHash

    (conwayGenesis, _conwayGenesisHash) <-
      firstExceptT CardanoProtocolInstantiationConwayGenesisReadError $
        Conway.readGenesis npcConwayGenesisFile
                           npcConwayGenesisFileHash

    shelleyLeaderCredentials <-
      firstExceptT CardanoProtocolInstantiationPraosLeaderCredentialsError $
        Shelley.readLeaderCredentials files

    --TODO: all these protocol versions below are confusing and unnecessary.
    -- It could and should all be automated and these config entries eliminated.
    return $!
      SomeConsensusProtocol CardanoBlockType $ ProtocolInfoArgsCardano $ CardanoProtocolParams {
        paramsByron =
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
            byronLeaderCredentials,
          byronMaxTxCapacityOverrides =
            TxLimits.mkOverrides TxLimits.noOverridesMeasure
        }
      , paramsShelleyBased =
        Consensus.ProtocolParamsShelleyBased {
          shelleyBasedInitialNonce      = Shelley.genesisHashToPraosNonce
                                            shelleyGenesisHash,
          shelleyBasedLeaderCredentials = shelleyLeaderCredentials
        }
      , paramsShelley =
        Consensus.ProtocolParamsShelley {
          -- This is /not/ the Shelley protocol version. It is the protocol
          -- version that this node will declare that it understands, when it
          -- is in the Shelley era. That is, it is the version of protocol
          -- /after/ Shelley, i.e. Allegra.
          shelleyProtVer =
            ProtVer (natVersion @3) 0,
          shelleyMaxTxCapacityOverrides =
            TxLimits.mkOverrides TxLimits.noOverridesMeasure
        }
      , paramsAllegra =
        Consensus.ProtocolParamsAllegra {
          -- This is /not/ the Allegra protocol version. It is the protocol
          -- version that this node will declare that it understands, when it
          -- is in the Allegra era. That is, it is the version of protocol
          -- /after/ Allegra, i.e. Mary.
          allegraProtVer =
            ProtVer (natVersion @4) 0,
          allegraMaxTxCapacityOverrides =
            TxLimits.mkOverrides TxLimits.noOverridesMeasure
        }
      , paramsMary =
        Consensus.ProtocolParamsMary {
          -- This is /not/ the Mary protocol version. It is the protocol
          -- version that this node will declare that it understands, when it
          -- is in the Mary era. That is, it is the version of protocol
          -- /after/ Mary, i.e. Alonzo.
          maryProtVer = ProtVer (natVersion @5) 0,
          maryMaxTxCapacityOverrides =
            TxLimits.mkOverrides TxLimits.noOverridesMeasure
        }
      , paramsAlonzo =
        Consensus.ProtocolParamsAlonzo {
          -- This is /not/ the Alonzo protocol version. It is the protocol
          -- version that this node will declare that it understands, when it
          -- is in the Alonzo era. That is, it is the version of protocol
          -- /after/ Alonzo, i.e. Babbage.
          -- NOTE:
          -- We are not actually transitioning to version 7.2,
          -- this is a HACK so that we can distinguish between others
          -- versions of the node that are broadcasting major version 7.
          -- We intentionally broadcast 7.0 starting in Babbage.
          alonzoProtVer = ProtVer (natVersion @7) 2,
          alonzoMaxTxCapacityOverrides =
            TxLimits.mkOverrides TxLimits.noOverridesMeasure
        }
      , paramsBabbage =
        Praos.ProtocolParamsBabbage {
          -- If Conway is not enabled, this is the Babbage protocol version,
          -- since that's the last one node understands.
          --
          -- If Conway is enabled, then this is /not/ the Babbage protocol
          -- version. It is the protocol version that this node will declare
          -- that it understands during the Babbage era. That is, it is the
          -- version of protocol /after/ Babbage, i.e. Conway.
          Praos.babbageProtVer =
            if npcExperimentalHardForksEnabled
              then ProtVer (natVersion @9) 0
              else ProtVer (natVersion @8) 0,
          Praos.babbageMaxTxCapacityOverrides =
            TxLimits.mkOverrides TxLimits.noOverridesMeasure
        }
      , paramsConway =
        Praos.ProtocolParamsConway {
          -- If Conway is not enabled, this is the Babbage protocol version.
          --
          -- If Conway is enabled, this is the Conway protocol version.
          Praos.conwayProtVer =
            if npcExperimentalHardForksEnabled
              then ProtVer (natVersion @9) 0
              else ProtVer (natVersion @8) 0,
          Praos.conwayMaxTxCapacityOverrides =
            TxLimits.mkOverrides TxLimits.noOverridesMeasure
        }
        -- The remaining arguments specify the parameters needed to transition between two eras
      , ledgerTransitionConfig =
          Ledger.mkLatestTransitionConfig
            shelleyGenesis
            alonzoGenesis
            conwayGenesis
      , hardForkTriggers =
        Consensus.CardanoHardForkTriggers' {
          triggerHardForkShelley =
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
               -- Version 5 is Alonzo
               -- Version 6 is Alonzo (intra era hardfork)
               -- Version 7 is Babbage
               -- Version 8 is Babbage (intra era hardfork)
               -- Version 9 is Conway
               --
               -- But we also provide an override to allow for simpler test setups
               -- such as triggering at the 0 -> 1 transition .
               --
               Nothing -> Consensus.TriggerHardForkAtVersion
                            (maybe 2 fromIntegral npcTestShelleyHardForkAtVersion)

               -- Alternatively, for testing we can transition at a specific epoch.
               --
               Just epochNo -> Consensus.TriggerHardForkAtEpoch epochNo
        , triggerHardForkAllegra =
            case npcTestAllegraHardForkAtEpoch of
               Nothing -> Consensus.TriggerHardForkAtVersion
                            (maybe 3 fromIntegral npcTestAllegraHardForkAtVersion)
               Just epochNo -> Consensus.TriggerHardForkAtEpoch epochNo
        , triggerHardForkMary =
            case npcTestMaryHardForkAtEpoch of
               Nothing -> Consensus.TriggerHardForkAtVersion
                            (maybe 4 fromIntegral npcTestMaryHardForkAtVersion)
               Just epochNo -> Consensus.TriggerHardForkAtEpoch epochNo
        , triggerHardForkAlonzo =
            case npcTestAlonzoHardForkAtEpoch of
               Nothing -> Consensus.TriggerHardForkAtVersion
                            (maybe 5 fromIntegral npcTestAlonzoHardForkAtVersion)
               Just epochNo -> Consensus.TriggerHardForkAtEpoch epochNo
        , triggerHardForkBabbage =
             case npcTestBabbageHardForkAtEpoch of
                Nothing -> Consensus.TriggerHardForkAtVersion
                             (maybe 7 fromIntegral npcTestBabbageHardForkAtVersion)
                Just epochNo -> Consensus.TriggerHardForkAtEpoch epochNo
        , triggerHardForkConway =
             case npcTestConwayHardForkAtEpoch of
                Nothing -> Consensus.TriggerHardForkAtVersion
                             (maybe 9 fromIntegral npcTestConwayHardForkAtVersion)
                Just epochNo -> Consensus.TriggerHardForkAtEpoch epochNo
        }
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
