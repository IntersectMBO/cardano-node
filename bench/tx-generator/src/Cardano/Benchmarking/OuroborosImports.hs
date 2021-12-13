{- HLINT ignore "Eta reduce" -}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Benchmarking.OuroborosImports
  (
    CardanoBlock
  , LocalSubmitTx
  , LoggingLayer
  , PaymentKey
  , ShelleyGenesis
  , SigningKey
  , SigningKeyFile
  , StandardShelley
  , NetworkId
  , getGenesis
  , makeLocalConnectInfo
  , protocolToTopLevelConfig
  , protocolToNetworkId
  , protocolToCodecConfig
  , submitTxToNodeLocal
  ) where

import           Prelude

import           Ouroboros.Consensus.Block.Abstract
import qualified Ouroboros.Consensus.Cardano as Consensus
import           Ouroboros.Consensus.Config (TopLevelConfig, configBlock, configCodec)
import           Ouroboros.Consensus.Config.SupportsNode
                 (ConfigSupportsNode(..), getNetworkMagic)
import           Ouroboros.Consensus.Node (ProtocolInfo(..))
import           Ouroboros.Consensus.Shelley.Eras (StandardCrypto, StandardShelley)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult (..))

import           Cardano.Node.Configuration.Logging (LoggingLayer)
import           Cardano.Node.Protocol.Types ( SomeConsensusProtocol(..))

import           Cardano.Api.Shelley (CardanoMode)
import           Cardano.CLI.Types (SigningKeyFile)

import           Cardano.Api (NetworkId(..), LocalNodeConnectInfo(..), ConsensusModeParams(..), EpochSlots(..)
                             , TxInMode, TxValidationErrorInMode
                             , SigningKey, PaymentKey
                             , submitTxToNodeLocal)
import           Cardano.Api.Protocol.Types (BlockType(..), ProtocolInfoArgs(..), protocolInfo)

import           Cardano.Ledger.Shelley.Genesis (ShelleyGenesis)

type CardanoBlock = Consensus.CardanoBlock StandardCrypto

toProtocolInfo :: SomeConsensusProtocol -> ProtocolInfo IO CardanoBlock
toProtocolInfo (SomeConsensusProtocol CardanoBlockType info) = protocolInfo info
toProtocolInfo _ = error "toProtocolInfo unkown protocol"

getGenesis :: SomeConsensusProtocol -> ShelleyGenesis StandardShelley
getGenesis (SomeConsensusProtocol CardanoBlockType info) = shelleyBasedGenesis
 where
  (ProtocolInfoArgsCardano
   _
   Consensus.ProtocolParamsShelleyBased{Consensus.shelleyBasedGenesis}
    _ _ _ _ _ _ _ _) = info
getGenesis (SomeConsensusProtocol _ _ ) = error "getGenesis (SomeConsensusProtocol _ _ ) unknown protocol"

protocolToTopLevelConfig :: SomeConsensusProtocol -> TopLevelConfig CardanoBlock
protocolToTopLevelConfig ptcl = pInfoConfig
 where
   ProtocolInfo {pInfoConfig} = toProtocolInfo ptcl

protocolToCodecConfig :: SomeConsensusProtocol -> CodecConfig CardanoBlock
protocolToCodecConfig = configCodec . protocolToTopLevelConfig

protocolToNetworkId :: SomeConsensusProtocol -> NetworkId
protocolToNetworkId ptcl
  = Testnet $ getNetworkMagic $ configBlock $ protocolToTopLevelConfig ptcl

makeLocalConnectInfo :: NetworkId -> FilePath -> LocalNodeConnectInfo CardanoMode
makeLocalConnectInfo networkId sock
  = LocalNodeConnectInfo
      (CardanoModeParams (EpochSlots 21600))
      networkId
      sock

type LocalSubmitTx = (TxInMode CardanoMode -> IO (SubmitResult (TxValidationErrorInMode CardanoMode)))
