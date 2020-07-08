{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Api.LocalChainSync
  ( LocalTip (..)
  , getLocalTip
  , getLocalTip'
  ) where

import           Cardano.Prelude hiding (atomically, catch)

import           Data.Aeson (ToJSON (..))

import           Control.Concurrent.STM

import           Cardano.Api.Protocol (ProtocolData (..))
import           Cardano.Api.Protocol.Byron (mkNodeClientProtocolByron)
import           Cardano.Api.Protocol.Cardano (mkNodeClientProtocolCardano)
import           Cardano.Api.Protocol.Shelley (mkNodeClientProtocolShelley)
import           Cardano.Api.Typed

import           Cardano.Config.Orphanage ()
import           Cardano.Config.Shelley.Orphans ()

import           Ouroboros.Network.Block (Tip)
import           Ouroboros.Network.Protocol.ChainSync.Client
                   (ChainSyncClient(..), ClientStIdle(..), ClientStNext(..))

import           Ouroboros.Consensus.Block (BlockProtocol)
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import           Ouroboros.Consensus.Cardano (CardanoBlock, ProtocolClient)
import           Ouroboros.Consensus.Node.Run (RunNode)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)


data LocalTip
  = ByronLocalTip (Tip ByronBlock)
  | ShelleyLocalTip (Tip (ShelleyBlock TPraosStandardCrypto))
  | CardanoLocalTip (Tip (CardanoBlock TPraosStandardCrypto))
  deriving (Eq, Show)

instance ToJSON LocalTip where
  toJSON lt =
    case lt of
      ByronLocalTip t -> toJSON t
      ShelleyLocalTip t -> toJSON t
      CardanoLocalTip t -> toJSON t

-- | Get the node's tip using the local chain sync protocol.
getLocalTip
  :: FilePath
  -> NetworkId
  -> ProtocolData
  -> IO LocalTip
getLocalTip sockPath nw protocolData =
  case protocolData of
    ProtocolDataByron epSlots secParam ->
      let ptcl = mkNodeClientProtocolByron epSlots secParam
      in ByronLocalTip <$> getLocalTip' sockPath nw ptcl

    ProtocolDataShelley ->
      let ptcl = mkNodeClientProtocolShelley
      in ShelleyLocalTip <$> getLocalTip' sockPath nw ptcl

    ProtocolDataCardano epSlots secParam ->
      let ptcl = mkNodeClientProtocolCardano epSlots secParam
      in CardanoLocalTip <$> getLocalTip' sockPath nw ptcl

-- | Get the node's tip using the local chain sync protocol.
--
-- This is an alternative version of the 'getLocalTip' function that instead
-- accepts a 'CodecConfig blk' parameter and returns a 'Tip blk'.
getLocalTip'
  :: forall blk.
     RunNode blk
  => FilePath
  -> NetworkId
  -> ProtocolClient blk (BlockProtocol blk)
  -> IO (Tip blk)
getLocalTip' sockPath network ptcl = do
    resultVar <- newEmptyTMVarIO
    connectToLocalNode
      sockPath
      network
      ptcl
      (\_ -> nullLocalNodeClientProtocols {
        localChainSyncClient = Just (chainSyncGetCurrentTip resultVar)
      })

    atomically (takeTMVar resultVar)

chainSyncGetCurrentTip :: forall blk.
                          TMVar (Tip blk)
                       -> ChainSyncClient blk (Tip blk) IO ()
chainSyncGetCurrentTip tipVar =
  ChainSyncClient (pure clientStIdle)
 where
  clientStIdle :: ClientStIdle blk (Tip blk) IO ()
  clientStIdle =
    SendMsgRequestNext clientStNext (pure clientStNext)

  --TODO: we should be able to simply return the tip as the result with
  -- SendMsgDone and collect this as the result of the overall protocol.
  -- While currently we can have protocols return things, the current OuroborosApplication
  -- stuff gets in the way of returning an overall result, but that's being worked on,
  -- and this can be improved when that's ready.
  clientStNext :: ClientStNext blk (Tip blk) IO ()
  clientStNext = ClientStNext
    { recvMsgRollForward = \_blk tip -> ChainSyncClient $ do
        void $ atomically $ tryPutTMVar tipVar tip
        pure $ SendMsgDone ()
    , recvMsgRollBackward = \_point tip -> ChainSyncClient $ do
        void $ atomically $ tryPutTMVar tipVar tip
        pure $ SendMsgDone ()
    }
