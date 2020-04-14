{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Config.Protocol.Mock
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

import           Cardano.Config.Types (NodeConfiguration(..))


------------------------------------------------------------------------------
-- Mock/testing protocols
--

type BftBlock =
       SimpleBlock SimpleMockCrypto
                   (SimpleBftExt SimpleMockCrypto BftMockCrypto)

mkConsensusProtocolBFT
  :: NodeConfiguration
  -> ExceptT MockProtocolInstantiationError IO
             (Consensus.Protocol BftBlock (Bft BftMockCrypto))
mkConsensusProtocolBFT NodeConfiguration {
                         ncNodeId,
                         ncNumCoreNodes
                       } =
  hoistEither $ do
    (nodeId, numCoreNodes) <- checkProtocolParams ncNodeId ncNumCoreNodes

    return $ Consensus.ProtocolMockBFT
               numCoreNodes
               nodeId
               mockSecurityParam
               (defaultSimpleBlockConfig mockSecurityParam mockSlotLength)


type PBFTBlock =
       SimpleBlock SimpleMockCrypto
                   (SimplePBftExt SimpleMockCrypto PBftMockCrypto)

mkConsensusProtocolPBFT
  :: NodeConfiguration
  -> ExceptT MockProtocolInstantiationError IO
             (Consensus.Protocol PBFTBlock (PBft PBftMockCrypto))
mkConsensusProtocolPBFT NodeConfiguration {
                              ncNodeId,
                              ncNumCoreNodes
                            } =
  hoistEither $ do
    (nodeId, numCoreNodes) <- checkProtocolParams ncNodeId ncNumCoreNodes
    let (NumCoreNodes numNodes) = numCoreNodes

    return $ Consensus.ProtocolMockPBFT
               PBftParams {
                 pbftSecurityParam      = mockSecurityParam
               , pbftNumNodes           = numCoreNodes
               , pbftSignatureThreshold = (1.0 / fromIntegral numNodes) + 0.1
               }
               (defaultSimpleBlockConfig mockSecurityParam mockSlotLength)
               nodeId


type PraosBlock =
       SimpleBlock SimpleMockCrypto
                   (SimplePraosExt SimpleMockCrypto PraosMockCrypto)

mkConsensusProtocolPraos
  :: NodeConfiguration
  -> ExceptT MockProtocolInstantiationError IO
             (Consensus.Protocol PraosBlock (Praos PraosMockCrypto))
mkConsensusProtocolPraos NodeConfiguration {
                           ncNodeId,
                           ncNumCoreNodes
                         } =
  hoistEither $ do
    (nodeId, numCoreNodes) <- checkProtocolParams ncNodeId ncNumCoreNodes

    return $ Consensus.ProtocolMockPraos
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

