{- HLINT ignore "Eta reduce" -}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Benchmarking.OuroborosImports
  (
    CardanoBlock
  , LocalSubmitTx
  , PaymentKey
  , ShelleyGenesis
  , SigningKey
  , SigningKeyFile
  , NetworkId
  -- , getGenesis
  , makeLocalConnectInfo
  , protocolToTopLevelConfig
  , protocolToNetworkId
  , protocolToCodecConfig
  , submitTxToNodeLocal
  ) where

import           Cardano.Api (BlockType (..), ConsensusModeParams (..), EpochSlots (..),
                   LocalNodeConnectInfo (..), NetworkId (..), PaymentKey, SigningKey, SocketPath,
                   TxInMode, TxSubmitResult (..), protocolInfo, submitTxToNodeLocal)

import           Cardano.CLI.Type.Common (SigningKeyFile)
import           Cardano.Ledger.Shelley.Genesis (ShelleyGenesis)
import           Cardano.Node.Protocol.Types (SomeConsensusProtocol (..))
import           Ouroboros.Consensus.Block.Abstract
import qualified Ouroboros.Consensus.Cardano as Consensus
import           Ouroboros.Consensus.Config (TopLevelConfig, configBlock, configCodec)
import           Ouroboros.Consensus.Config.SupportsNode (ConfigSupportsNode (..), getNetworkMagic)
import           Ouroboros.Consensus.Node (ProtocolInfo (..))
import           Ouroboros.Consensus.Shelley.Eras (StandardCrypto)

import           Prelude

type CardanoBlock = Consensus.CardanoBlock StandardCrypto

toProtocolInfo :: SomeConsensusProtocol -> IO (ProtocolInfo CardanoBlock)
toProtocolInfo (SomeConsensusProtocol CardanoBlockType info) = fst <$> protocolInfo @IO info
toProtocolInfo _ = error "toProtocolInfo unknown protocol"

protocolToTopLevelConfig :: SomeConsensusProtocol -> IO (TopLevelConfig CardanoBlock)
protocolToTopLevelConfig ptcl = pInfoConfig <$> toProtocolInfo ptcl

protocolToCodecConfig :: SomeConsensusProtocol -> IO (CodecConfig CardanoBlock)
protocolToCodecConfig = fmap configCodec . protocolToTopLevelConfig

protocolToNetworkId :: SomeConsensusProtocol -> IO NetworkId
protocolToNetworkId ptcl
  = Testnet . getNetworkMagic . configBlock <$> protocolToTopLevelConfig ptcl

makeLocalConnectInfo :: NetworkId -> SocketPath -> LocalNodeConnectInfo
makeLocalConnectInfo networkId socketPath
  = LocalNodeConnectInfo (CardanoModeParams (EpochSlots 21600)) networkId socketPath

type LocalSubmitTx = (TxInMode -> IO TxSubmitResult)
