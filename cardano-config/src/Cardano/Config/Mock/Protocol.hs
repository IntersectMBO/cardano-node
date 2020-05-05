{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Config.Mock.Protocol
  ( mkConsensusProtocolBFT
  , mkConsensusProtocolPBFT
  , mkConsensusProtocolPraos
  , MockProtocolInstantiationError(..)
  , renderMockProtocolInstantiationError
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (hoistEither)

import           Ouroboros.Consensus.BlockchainTime (SlotLength, slotLengthFromSec)
import           Ouroboros.Consensus.Cardano hiding (Protocol)
import qualified Ouroboros.Consensus.Cardano as Consensus
import           Ouroboros.Consensus.Mock.Ledger.Block
import           Ouroboros.Consensus.Mock.Ledger.Block.BFT
import           Ouroboros.Consensus.Mock.Ledger.Block.Praos
import           Ouroboros.Consensus.Mock.Ledger.Block.PBFT
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))

import           Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))

import           Cardano.Config.Types (NodeConfiguration(..),SomeConsensusProtocol(..))
import           Cardano.TracingOrphanInstances.Mock ()


------------------------------------------------------------------------------
-- Mock/testing protocols
--

mkConsensusProtocolBFT
  :: NodeConfiguration
  -> ExceptT MockProtocolInstantiationError IO SomeConsensusProtocol
mkConsensusProtocolBFT NodeConfiguration {
                         ncNodeId,
                         ncNumCoreNodes
                       } =
  hoistEither $ do
    (nodeId, numCoreNodes) <- checkProtocolParams ncNodeId ncNumCoreNodes

    let consensusProtocol ::
          Consensus.Protocol
            (SimpleBlock SimpleMockCrypto
                         (SimpleBftExt SimpleMockCrypto BftMockCrypto))
            (Bft BftMockCrypto)
        consensusProtocol =
          Consensus.ProtocolMockBFT
            numCoreNodes
            nodeId
            mockSecurityParam
            (defaultSimpleBlockConfig mockSecurityParam mockSlotLength)

    return (SomeConsensusProtocol consensusProtocol)


mkConsensusProtocolPBFT
  :: NodeConfiguration
  -> ExceptT MockProtocolInstantiationError IO SomeConsensusProtocol
mkConsensusProtocolPBFT NodeConfiguration {
                              ncNodeId,
                              ncNumCoreNodes
                            } =
  hoistEither $ do
    (nodeId, numCoreNodes) <- checkProtocolParams ncNodeId ncNumCoreNodes
    let (NumCoreNodes numNodes) = numCoreNodes

    let consensusProtocol ::
          Consensus.Protocol
            (SimpleBlock SimpleMockCrypto
                         (SimplePBftExt SimpleMockCrypto PBftMockCrypto))
            (PBft PBftMockCrypto)
        consensusProtocol =
          Consensus.ProtocolMockPBFT
            PBftParams {
              pbftSecurityParam      = mockSecurityParam
            , pbftNumNodes           = numCoreNodes
            , pbftSignatureThreshold = (1.0 / fromIntegral numNodes) + 0.1
            }
            (defaultSimpleBlockConfig mockSecurityParam mockSlotLength)
            nodeId

    return (SomeConsensusProtocol consensusProtocol)


mkConsensusProtocolPraos
  :: NodeConfiguration
  -> ExceptT MockProtocolInstantiationError IO SomeConsensusProtocol
mkConsensusProtocolPraos NodeConfiguration {
                           ncNodeId,
                           ncNumCoreNodes
                         } =
  hoistEither $ do
    (nodeId, numCoreNodes) <- checkProtocolParams ncNodeId ncNumCoreNodes

    let consensusProtocol ::
          Consensus.Protocol
            (SimpleBlock SimpleMockCrypto
                         (SimplePraosExt SimpleMockCrypto PraosMockCrypto))
            (Praos PraosMockCrypto)
        consensusProtocol =
          Consensus.ProtocolMockPraos
            numCoreNodes
            nodeId
            PraosParams {
                praosSecurityParam = mockSecurityParam
              , praosSlotsPerEpoch = 3
              , praosLeaderF       = 0.5
              , praosLifetimeKES   = 1000000
              }
            (defaultSimpleBlockConfig mockSecurityParam (slotLengthFromSec 2))

    return (SomeConsensusProtocol consensusProtocol)


mockSecurityParam :: SecurityParam
mockSecurityParam = SecurityParam 5

mockSlotLength :: SlotLength
mockSlotLength = slotLengthFromSec 20


-- | Helper for creating a 'Consensus.Protocol' for a mock protocol that
-- needs the 'CoreNodeId' and NumCoreNodes'. If one of them is missing from the
-- 'CardanoConfiguration', a 'MockProtocolInstantiationError' exception is thrown.
checkProtocolParams
  :: Maybe NodeId
  -> Maybe Word64
  -> Either MockProtocolInstantiationError (CoreNodeId, NumCoreNodes)
checkProtocolParams nId mNumCoreNodes = do

    nodeId <- case nId of
                Just (CoreId nodeId) -> pure nodeId
                _                        -> Left MissingCoreNodeId
    numCoreNodes <- maybe (Left MissingNumCoreNodes) Right mNumCoreNodes

    return (nodeId, NumCoreNodes numCoreNodes)


------------------------------------------------------------------------------
-- Errors
--

data MockProtocolInstantiationError =
    MissingCoreNodeId
  | MissingNumCoreNodes
  deriving Show


renderMockProtocolInstantiationError :: MockProtocolInstantiationError -> Text
renderMockProtocolInstantiationError pie =
  case pie of
    MissingCoreNodeId   -> "Missing core node id"
    MissingNumCoreNodes -> "NumCoreNodes: not specified in configuration yaml file."
