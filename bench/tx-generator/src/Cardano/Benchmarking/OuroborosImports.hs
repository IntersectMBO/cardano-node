{- HLINT ignore "Eta reduce" -}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

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
  -- , getGenesis
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
import           Ouroboros.Consensus.Config.SupportsNode (ConfigSupportsNode (..), getNetworkMagic)
import           Ouroboros.Consensus.Node (ProtocolInfo (..))
import           Ouroboros.Consensus.Shelley.Eras (StandardCrypto, StandardShelley)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult (..))

import           Cardano.Node.Configuration.Logging (LoggingLayer)
import           Cardano.Node.Protocol.Types (SomeConsensusProtocol (..))

import           Cardano.CLI.Types.Common (SigningKeyFile)

import           Cardano.Api (BlockType (..), ConsensusModeParams (..), EpochSlots (..),
                   LocalNodeConnectInfo (..), NetworkId (..), PaymentKey, SigningKey, SocketPath,
                   TxInMode, TxValidationErrorInCardanoMode, protocolInfo, submitTxToNodeLocal)
import           Cardano.Ledger.Shelley.Genesis (ShelleyGenesis)

type CardanoBlock = Consensus.CardanoBlock StandardCrypto

toProtocolInfo :: SomeConsensusProtocol -> ProtocolInfo CardanoBlock
toProtocolInfo (SomeConsensusProtocol CardanoBlockType info) = fst $ protocolInfo @IO info
toProtocolInfo _ = error "toProtocolInfo unknown protocol"

protocolToTopLevelConfig :: SomeConsensusProtocol -> TopLevelConfig CardanoBlock
protocolToTopLevelConfig ptcl = pInfoConfig
 where
   ProtocolInfo {pInfoConfig} = toProtocolInfo ptcl

protocolToCodecConfig :: SomeConsensusProtocol -> CodecConfig CardanoBlock
protocolToCodecConfig = configCodec . protocolToTopLevelConfig

protocolToNetworkId :: SomeConsensusProtocol -> NetworkId
protocolToNetworkId ptcl
  = Testnet $ getNetworkMagic $ configBlock $ protocolToTopLevelConfig ptcl

makeLocalConnectInfo :: NetworkId -> SocketPath -> LocalNodeConnectInfo
makeLocalConnectInfo networkId socketPath
  = LocalNodeConnectInfo (CardanoModeParams (EpochSlots 21600)) networkId socketPath

type LocalSubmitTx = (TxInMode -> IO (SubmitResult TxValidationErrorInCardanoMode))
