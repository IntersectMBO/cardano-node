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

    -- * Errors
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

mkSomeConsensusProtocolBFT,
  mkSomeConsensusProtocolPBFT,
  mkSomeConsensusProtocolPraos
  :: NodeConfiguration
  -> ExceptT MockProtocolInstantiationError IO SomeConsensusProtocol

-- Applying the SomeConsensusProtocol here is a check that
-- the type of mkConsensusProtocolRealPBFT fits all the class
-- constraints we need to run the protocol.

mkSomeConsensusProtocolBFT nc =
    SomeConsensusProtocol <$> mkConsensusProtocolBFT nc

mkSomeConsensusProtocolPBFT nc =
    SomeConsensusProtocol <$> mkConsensusProtocolPBFT nc

mkSomeConsensusProtocolPraos nc =
    SomeConsensusProtocol <$> mkConsensusProtocolPraos nc


mkConsensusProtocolBFT
  :: NodeConfiguration
  -> ExceptT MockProtocolInstantiationError IO
             (Consensus.Protocol
               (SimpleBlock SimpleMockCrypto
                            (SimpleBftExt SimpleMockCrypto BftMockCrypto))
               (Bft BftMockCrypto))
mkConsensusProtocolBFT NodeConfiguration {
                         ncNodeId,
                         ncNumCoreNodes
                       } = do

    (nodeId, numCoreNodes) <- hoistEither $
                                checkProtocolParams ncNodeId ncNumCoreNodes
    return $
      Consensus.ProtocolMockBFT
        numCoreNodes
        nodeId
        mockSecurityParam
        (defaultSimpleBlockConfig mockSecurityParam mockSlotLength)


mkConsensusProtocolPBFT
  :: NodeConfiguration
  -> ExceptT MockProtocolInstantiationError IO
             (Consensus.Protocol
               (SimpleBlock SimpleMockCrypto
                            (SimplePBftExt SimpleMockCrypto PBftMockCrypto))
               (PBft PBftMockCrypto))
mkConsensusProtocolPBFT NodeConfiguration {
                              ncNodeId,
                              ncNumCoreNodes
                            } = do

    (nodeId, numCoreNodes) <- hoistEither $
                                checkProtocolParams ncNodeId ncNumCoreNodes
    let (NumCoreNodes numNodes) = numCoreNodes
    return $
      Consensus.ProtocolMockPBFT
        PBftParams {
          pbftSecurityParam      = mockSecurityParam
        , pbftNumNodes           = numCoreNodes
        , pbftSignatureThreshold = (1.0 / fromIntegral numNodes) + 0.1
        }
        (defaultSimpleBlockConfig mockSecurityParam mockSlotLength)
        nodeId


mkConsensusProtocolPraos
  :: NodeConfiguration
  -> ExceptT MockProtocolInstantiationError IO
             (Consensus.Protocol
               (SimpleBlock SimpleMockCrypto
                            (SimplePraosExt SimpleMockCrypto PraosMockCrypto))
               (Praos PraosMockCrypto))
mkConsensusProtocolPraos NodeConfiguration {
                           ncNodeId,
                           ncNumCoreNodes
                         } = do

    (nodeId, numCoreNodes) <- hoistEither $
                                checkProtocolParams ncNodeId ncNumCoreNodes
    return $
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
