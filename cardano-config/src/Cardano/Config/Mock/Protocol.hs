{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Config.Mock.Protocol
  (
    -- * Protocols exposing the specific type
    -- | Use these when you need the specific instance
    mkConsensusProtocolBFT
  , mkConsensusProtocolPBFT
  , mkConsensusProtocolPraos

    -- * Protocols hiding the specific type
    -- | Use these when you want to handle protocols generically
  , mkSomeConsensusProtocolBFT
  , mkSomeConsensusProtocolPBFT
  , mkSomeConsensusProtocolPraos
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

import           Cardano.Config.Types (NodeMockProtocolConfiguration(..),
                   SomeConsensusProtocol(..))
import           Cardano.TracingOrphanInstances.Mock ()


------------------------------------------------------------------------------
-- Mock/testing protocols
--

mkSomeConsensusProtocolBFT,
  mkSomeConsensusProtocolPBFT,
  mkSomeConsensusProtocolPraos
  :: NodeMockProtocolConfiguration
  -> SomeConsensusProtocol

-- Applying the SomeConsensusProtocol here is a check that
-- the type of mkConsensusProtocolRealPBFT fits all the class
-- constraints we need to run the protocol.

mkSomeConsensusProtocolBFT nc =
    SomeConsensusProtocol $ mkConsensusProtocolBFT nc

mkSomeConsensusProtocolPBFT nc =
    SomeConsensusProtocol $ mkConsensusProtocolPBFT nc

mkSomeConsensusProtocolPraos nc =
    SomeConsensusProtocol $ mkConsensusProtocolPraos nc


mkConsensusProtocolBFT
  :: NodeMockProtocolConfiguration
  -> Consensus.Protocol IO
               (SimpleBlock SimpleMockCrypto
                            (SimpleBftExt SimpleMockCrypto BftMockCrypto))
               (Bft BftMockCrypto)
mkConsensusProtocolBFT NodeMockProtocolConfiguration {
                             npcMockNodeId       = nodeId,
                             npcMockNumCoreNodes = numCoreNodes
                           } =
      Consensus.ProtocolMockBFT
        (NumCoreNodes numCoreNodes)
        nodeId
        mockSecurityParam
        (defaultEraParams mockSecurityParam mockSlotLength)


mkConsensusProtocolPBFT
  :: NodeMockProtocolConfiguration
  -> Consensus.Protocol IO
               (SimpleBlock SimpleMockCrypto
                            (SimplePBftExt SimpleMockCrypto PBftMockCrypto))
               (PBft PBftMockCrypto)
mkConsensusProtocolPBFT NodeMockProtocolConfiguration {
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


mkConsensusProtocolPraos
  :: NodeMockProtocolConfiguration
  -> Consensus.Protocol IO
               (SimpleBlock SimpleMockCrypto
                            (SimplePraosExt SimpleMockCrypto PraosMockCrypto))
               (Praos PraosMockCrypto)
mkConsensusProtocolPraos NodeMockProtocolConfiguration {
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

