{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Node.Protocol.Cardano
  ( mkSomeConsensusProtocolCardano

    -- * Errors
  , CardanoProtocolInstantiationError(..)
  ) where

import           Prelude

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)

import qualified Cardano.Chain.Update as Byron

import           Ouroboros.Consensus.Cardano
import qualified Ouroboros.Consensus.Cardano as Consensus
import qualified Ouroboros.Consensus.Cardano.CanHardFork as Consensus
import           Ouroboros.Consensus.HardFork.Combinator.Condense ()
import qualified Ouroboros.Consensus.Shelley.Node.Praos as Praos

import           Ouroboros.Consensus.Cardano.Condense ()
import qualified Ouroboros.Consensus.Mempool.TxLimits as TxLimits

import           Cardano.Api
import           Cardano.Api.Orphans ()
import           Cardano.Api.Protocol.Types
import           Cardano.Node.Types

import           Cardano.Tracing.OrphanInstances.Byron ()
import           Cardano.Tracing.OrphanInstances.Shelley ()

import qualified Cardano.Node.Protocol.Alonzo as Alonzo
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
  -> NodeAlonzoProtocolConfiguration
  -> NodeHardForkProtocolConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT CardanoProtocolInstantiationError IO SomeConsensusProtocol
mkSomeConsensusProtocolCardano NodeByronProtocolConfiguration {
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
                             npcShelleyGenesisFileHash
                           }
                           NodeAlonzoProtocolConfiguration {
                             npcAlonzoGenesisFile,
                             npcAlonzoGenesisFileHash
                           }
                           NodeHardForkProtocolConfiguration {
                            -- npcTestEnableDevelopmentHardForkEras,
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
                             npcTestBabbageHardForkAtVersion
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

    shelleyLeaderCredentials <-
      firstExceptT CardanoProtocolInstantiationPraosLeaderCredentialsError $
        Shelley.readLeaderCredentials files

    --TODO: all these protocol versions below are confusing and unnecessary.
    -- It could and should all be automated and these config entries eliminated.
    return $!
      SomeConsensusProtocol CardanoBlockType $ ProtocolInfoArgsCardano
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
            Byron.ProtocolVersion
              npcByronSupportedProtocolVersionMajor
              npcByronSupportedProtocolVersionMinor
              npcByronSupportedProtocolVersionAlt,
          byronSoftwareVersion =
            Byron.SoftwareVersion
              npcByronApplicationName
              npcByronApplicationVersion,
          byronLeaderCredentials =
            byronLeaderCredentials,
          byronMaxTxCapacityOverrides =
            TxLimits.mkOverrides TxLimits.noOverridesMeasure
        }
        Consensus.ProtocolParamsShelleyBased {
          shelleyBasedGenesis           = shelleyGenesis,
          shelleyBasedInitialNonce      = Shelley.genesisHashToPraosNonce
                                            shelleyGenesisHash,
          shelleyBasedLeaderCredentials = shelleyLeaderCredentials
        }
        Consensus.ProtocolParamsShelley {
          -- This is /not/ the Shelley protocol version. It is the protocol
          -- version that this node will declare that it understands, when it
          -- is in the Shelley era. That is, it is the version of protocol
          -- /after/ Shelley, i.e. Allegra.
          shelleyProtVer =
            ProtVer 3 0,
          shelleyMaxTxCapacityOverrides =
            TxLimits.mkOverrides TxLimits.noOverridesMeasure
        }
        Consensus.ProtocolParamsAllegra {
          -- This is /not/ the Allegra protocol version. It is the protocol
          -- version that this node will declare that it understands, when it
          -- is in the Allegra era. That is, it is the version of protocol
          -- /after/ Allegra, i.e. Mary.
          allegraProtVer =
            ProtVer 4 0,
          allegraMaxTxCapacityOverrides =
            TxLimits.mkOverrides TxLimits.noOverridesMeasure
        }
        Consensus.ProtocolParamsMary {
          -- This is /not/ the Mary protocol version. It is the protocol
          -- version that this node will declare that it understands, when it
          -- is in the Mary era. That is, it is the version of protocol
          -- /after/ Mary, i.e. Alonzo.
          maryProtVer = ProtVer 5 0,
          maryMaxTxCapacityOverrides =
            TxLimits.mkOverrides TxLimits.noOverridesMeasure
        }
        Consensus.ProtocolParamsAlonzo {
          -- This is /not/ the Alonzo protocol version. It is the protocol
          -- version that this node will declare that it understands, when it
          -- is in the Alonzo era. That is, it is the version of protocol
          -- /after/ Alonzo, i.e. Babbage.
          alonzoProtVer = ProtVer 7 0,
          alonzoMaxTxCapacityOverrides =
            TxLimits.mkOverrides TxLimits.noOverridesMeasure
        }
        Praos.ProtocolParamsBabbage {
          -- This is /not/ the Babbage protocol version. It is the protocol
          -- version that this node will declare that it understands, when it
          -- is in the Babbage era. Since Babbage is currently the last known
          -- protocol version then this is also the Babbage protocol version.
          Praos.babbageProtVer = ProtVer 7 0,
          Praos.babbageMaxTxCapacityOverrides =
            TxLimits.mkOverrides TxLimits.noOverridesMeasure
        }
        -- ProtocolParamsTransition specifies the parameters needed to transition between two eras
        -- The comments below also apply for the Shelley -> Allegra and Allegra -> Mary hard forks.
        -- Byron to Shelley hard fork parameters
        Consensus.ProtocolTransitionParamsShelleyBased {
          transitionTranslationContext = (),
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
               -- Version 5 is Alonzo
               -- Version 6 is Alonzo (intra era hardfork)
               -- Version 7 is Babbage
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
        Consensus.ProtocolTransitionParamsShelleyBased {
          transitionTranslationContext = (),
          transitionTrigger =
            case npcTestAllegraHardForkAtEpoch of
               Nothing -> Consensus.TriggerHardForkAtVersion
                            (maybe 3 fromIntegral npcTestAllegraHardForkAtVersion)
               Just epochNo -> Consensus.TriggerHardForkAtEpoch epochNo
        }
        -- Allegra to Mary hard fork parameters
        Consensus.ProtocolTransitionParamsShelleyBased {
          transitionTranslationContext = (),
          transitionTrigger =
            case npcTestMaryHardForkAtEpoch of
               Nothing -> Consensus.TriggerHardForkAtVersion
                            (maybe 4 fromIntegral npcTestMaryHardForkAtVersion)
               Just epochNo -> Consensus.TriggerHardForkAtEpoch epochNo
        }
        -- Mary to Alonzo hard fork parameters
        Consensus.ProtocolTransitionParamsShelleyBased {
          transitionTranslationContext = alonzoGenesis,
          transitionTrigger =
            case npcTestAlonzoHardForkAtEpoch of
               Nothing -> Consensus.TriggerHardForkAtVersion
                            (maybe 5 fromIntegral npcTestAlonzoHardForkAtVersion)
               Just epochNo -> Consensus.TriggerHardForkAtEpoch epochNo
        }
        -- Alonzo to Babbage hard fork parameters
        Consensus.ProtocolTransitionParamsShelleyBased {
          transitionTranslationContext = alonzoGenesis,
          transitionTrigger =
             case npcTestBabbageHardForkAtEpoch of
                Nothing -> Consensus.TriggerHardForkAtVersion
                             (maybe 7 fromIntegral npcTestBabbageHardForkAtVersion)
                Just epochNo -> Consensus.TriggerHardForkAtEpoch epochNo

        }

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

     | CardanoProtocolInstantiationPraosLeaderCredentialsError
         Shelley.PraosLeaderCredentialsError

     | CardanoProtocolInstantiationErrorAlonzo
         Alonzo.AlonzoProtocolInstantiationError
  deriving Show

instance Error CardanoProtocolInstantiationError where
  displayError (CardanoProtocolInstantiationErrorByron err) =
    displayError err
  displayError (CardanoProtocolInstantiationShelleyGenesisReadError err) =
    "Shelley related: " <> displayError err
  displayError (CardanoProtocolInstantiationAlonzoGenesisReadError err) =
    "Alonzo related: " <> displayError err
  displayError (CardanoProtocolInstantiationPraosLeaderCredentialsError err) =
    displayError err
  displayError (CardanoProtocolInstantiationErrorAlonzo err) =
    displayError err
