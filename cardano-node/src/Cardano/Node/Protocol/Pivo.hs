{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Node.Protocol.Pivo where

import           Cardano.Prelude
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither)

import           Shelley.Spec.Ledger.Genesis (validateGenesis)
import           Shelley.Spec.Ledger.PParams (ProtVer (..))

import qualified Ouroboros.Consensus.Cardano as Consensus
import           Ouroboros.Consensus.Shelley.Eras (StandardShelley, StandardPivo)
import           Ouroboros.Consensus.Shelley.Node (ProtocolParamsPivo (pivoProtVer)
                                                  , shelleyBasedGenesis
                                                  , shelleyBasedInitialNonce
                                                  , shelleyBasedLeaderCredentials
                                                  , ShelleyGenesis (..)
                                                  )

import Cardano.Node.Types (NodeShelleyProtocolConfiguration
                             (npcShelleyGenesisFile, npcShelleyGenesisFileHash)
                          , ProtocolFilepaths
                          , NodeShelleyProtocolConfiguration (..)
                          )
import Cardano.Node.Protocol.Types (SomeConsensusProtocol (..))
import Cardano.Node.Protocol.Shelley (ShelleyProtocolInstantiationError
                                       (GenesisValidationFailure)
                                     , readGenesis
                                     , readLeaderCredentials
                                     , genesisHashToPraosNonce
                                     )


-- todo: this duplicates 'mkSomeConsensusProtocolShelley' and
-- 'mkConsensusProtocolShelley'. I'm copying this for now to minimize changes
-- in my branch.
mkSomeConsensusProtocolPivo
  :: NodeShelleyProtocolConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT ShelleyProtocolInstantiationError IO SomeConsensusProtocol
mkSomeConsensusProtocolPivo nc files =
  SomeConsensusProtocol <$> mkConsensusProtocolPivo
  where
    NodeShelleyProtocolConfiguration
      { npcShelleyGenesisFile
      , npcShelleyGenesisFileHash
      } = nc
    mkConsensusProtocolPivo = do
      (genesis, genesisHash) <- readGenesis npcShelleyGenesisFile
                                            npcShelleyGenesisFileHash
      firstExceptT GenesisValidationFailure . hoistEither
        $ validateGenesis genesis
      leaderCredentials <- readLeaderCredentials files
      return
        $ Consensus.ProtocolPivo
            Consensus.ProtocolParamsShelleyBased
              { shelleyBasedGenesis =
                -- todo: we need to convert the 'genesis' value returned by
                -- 'readGenesis', which has type
                --
                -- > ShelleyGenesis StandardShelley
                --
                -- to a value of type:
                --
                -- > ShelleyGenesis (PivoEra StandardCrypto)
                --
                toPivoGenesis genesis
              , shelleyBasedInitialNonce = genesisHashToPraosNonce genesisHash
              , shelleyBasedLeaderCredentials = leaderCredentials
              }
            Consensus.ProtocolParamsPivo { pivoProtVer = ProtVer 3 0 }

-- todo: define this in the appropriate place
--
-- todo: it is also strange that this module which is shelley specific has to
-- be aware of Pivo. We need to revisit the dependencies we are introducing.
toPivoGenesis
  :: ShelleyGenesis StandardShelley
  -> ShelleyGenesis StandardPivo
toPivoGenesis ShelleyGenesis { sgSystemStart
                             , sgNetworkMagic
                             , sgNetworkId
                             , sgActiveSlotsCoeff
                             , sgSecurityParam
                             , sgEpochLength
                             , sgSlotsPerKESPeriod
                             , sgMaxKESEvolutions
                             , sgSlotLength
                             , sgUpdateQuorum
                             , sgMaxLovelaceSupply
                             , sgProtocolParams
                             , sgGenDelegs
                             , sgInitialFunds
                             , sgStaking
                             } =
              ShelleyGenesis { sgSystemStart = sgSystemStart
                             , sgNetworkMagic = sgNetworkMagic
                             , sgNetworkId = sgNetworkId
                             , sgActiveSlotsCoeff = sgActiveSlotsCoeff
                             , sgSecurityParam = sgSecurityParam
                             , sgEpochLength = sgEpochLength
                             , sgSlotsPerKESPeriod = sgSlotsPerKESPeriod
                             , sgMaxKESEvolutions = sgMaxKESEvolutions
                             , sgSlotLength = sgSlotLength
                             , sgUpdateQuorum = sgUpdateQuorum
                             , sgMaxLovelaceSupply = sgMaxLovelaceSupply
                             , sgProtocolParams = undefined
                               -- todo: add a function in the specs that
                               -- convert 'PParams era0' to 'PParams era1' for
                               -- any era.
                               --
                               -- > ??? sgProtocolParams
                             , sgGenDelegs = sgGenDelegs
                             , sgInitialFunds = sgInitialFunds
                             , sgStaking = sgStaking
                             }
