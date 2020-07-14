{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Node.Protocol.Mock
  (
    -- * Protocols exposing the specific type
    -- | Use these when you need the specific instance
    mkConsensusProtocolMockBFT
  , mkConsensusProtocolMockPBFT
  , mkConsensusProtocolMockPraos

    -- * Protocols hiding the specific type
    -- | Use these when you want to handle protocols generically
  , mkSomeConsensusProtocolMockBFT
  , mkSomeConsensusProtocolMockPBFT
  , mkSomeConsensusProtocolMockPraos
  ) where

import           Cardano.Prelude

import           Ouroboros.Consensus.BlockchainTime (SlotLength, slotLengthFromSec)
import           Ouroboros.Consensus.Cardano hiding (Protocol)
import qualified Ouroboros.Consensus.Cardano as Consensus
import           Ouroboros.Consensus.HardFork.History (defaultEraParams)
import           Ouroboros.Consensus.Mock.Ledger.Block
import           Ouroboros.Consensus.Mock.Ledger.Block.BFT
import           Ouroboros.Consensus.Mock.Ledger.Block.Praos
import           Ouroboros.Consensus.Mock.Ledger.Block.PBFT
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))

import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))

import           Cardano.Node.Types (NodeMockProtocolConfiguration(..))
import           Cardano.Tracing.OrphanInstances.Mock ()

import           Cardano.Node.Protocol.Types


------------------------------------------------------------------------------
-- Mock/testing protocols
--

mkSomeConsensusProtocolMockBFT,
  mkSomeConsensusProtocolMockPBFT,
  mkSomeConsensusProtocolMockPraos
  :: NodeMockProtocolConfiguration
  -> SomeConsensusProtocol

-- Applying the SomeConsensusProtocol here is a check that
-- the type of mkConsensusProtocolRealPBFT fits all the class
-- constraints we need to run the protocol.

mkSomeConsensusProtocolMockBFT nc =
    SomeConsensusProtocol $ mkConsensusProtocolMockBFT nc

mkSomeConsensusProtocolMockPBFT nc =
    SomeConsensusProtocol $ mkConsensusProtocolMockPBFT nc

mkSomeConsensusProtocolMockPraos nc =
    SomeConsensusProtocol $ mkConsensusProtocolMockPraos nc


mkConsensusProtocolMockBFT
  :: NodeMockProtocolConfiguration
  -> Consensus.Protocol IO
               (SimpleBlock SimpleMockCrypto
                            (SimpleBftExt SimpleMockCrypto BftMockCrypto))
               (Bft BftMockCrypto)
mkConsensusProtocolMockBFT NodeMockProtocolConfiguration {
                             npcMockNodeId       = nodeId,
                             npcMockNumCoreNodes = numCoreNodes
                           } =
      Consensus.ProtocolMockBFT
        (NumCoreNodes numCoreNodes)
        nodeId
        mockSecurityParam
        (defaultEraParams mockSecurityParam mockSlotLength)


mkConsensusProtocolMockPBFT
  :: NodeMockProtocolConfiguration
  -> Consensus.Protocol IO
               (SimpleBlock SimpleMockCrypto
                            (SimplePBftExt SimpleMockCrypto PBftMockCrypto))
               (PBft PBftMockCrypto)
mkConsensusProtocolMockPBFT NodeMockProtocolConfiguration {
                              npcMockNodeId       = nodeId,
                              npcMockNumCoreNodes = numCoreNodes
                            } =
      Consensus.ProtocolMockPBFT
        PBftParams {
          pbftSecurityParam      = mockSecurityParam
        , pbftNumNodes           = NumCoreNodes numCoreNodes
        , pbftSignatureThreshold = (1.0 / fromIntegral numCoreNodes) + 0.1
        }
        (defaultEraParams mockSecurityParam mockSlotLength)
        nodeId


mkConsensusProtocolMockPraos
  :: NodeMockProtocolConfiguration
  -> Consensus.Protocol IO
               (SimpleBlock SimpleMockCrypto
                            (SimplePraosExt SimpleMockCrypto PraosMockCrypto))
               (Praos PraosMockCrypto)
mkConsensusProtocolMockPraos NodeMockProtocolConfiguration {
                               npcMockNodeId       = nodeId,
                               npcMockNumCoreNodes = numCoreNodes
                             } =
      Consensus.ProtocolMockPraos
        (NumCoreNodes numCoreNodes)
        nodeId
        PraosParams {
            praosSecurityParam = mockSecurityParam
          , praosSlotsPerEpoch = 3
          , praosLeaderF       = 0.5
          , praosLifetimeKES   = 1000000
          }
        (defaultEraParams mockSecurityParam (slotLengthFromSec 2))


mockSecurityParam :: SecurityParam
mockSecurityParam = SecurityParam 5

mockSlotLength :: SlotLength
mockSlotLength = slotLengthFromSec 20
