{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Cardano.Node.Tracing.Tracers.Peer
  ( PeerT (..)
  , startPeerTracer
  , ppPeer
  ) where

import           Cardano.Logging hiding (traceWith)
import           Cardano.Node.Orphans ()
import           Cardano.Logging.Types.DataPoint (PeerT(..), ppMaxSlotNo, ppStatus)
import           Cardano.Node.Queries
import           Ouroboros.Consensus.Block (Header)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client (ChainSyncClientHandle,
                   csCandidate, cschcMap, viewChainSyncState)
import           Ouroboros.Consensus.Util.Orphans ()
import qualified Ouroboros.Network.AnchoredFragment as Net
import           Ouroboros.Network.Block (unSlotNo)
import qualified Ouroboros.Network.Block as Net
import qualified Ouroboros.Network.BlockFetch.ClientRegistry as Net
import           Ouroboros.Network.BlockFetch.ClientState (PeerFetchInFlight (..),
                   PeerFetchStatus (..), readFetchClientState)
import           Ouroboros.Network.ConnectionId (remoteAddress)
import           Ouroboros.Network.NodeToNode (RemoteAddress)

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async
import qualified Control.Concurrent.Class.MonadSTM.Strict as STM
import           Control.Monad (forever)
import           "contra-tracer" Control.Tracer
import           Data.Aeson (ToJSON (..), Value (..), toJSON, (.=))
import           Data.Functor ((<&>))
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           GHC.Conc (labelThread, myThreadId)

{- HLINT ignore "Use =<<" -}
{- HLINT ignore "Use <=<" -}

-- | Starts a background thread to periodically trace the current peer list.
-- The thread is linked to the parent thread for proper error propagation
-- and labeled for easier debugging and identification.
startPeerTracer
  :: Tracer IO [PeerT blk]  -- ^ Tracer for the peer list
  -> NodeKernelData blk     -- ^ Node kernel containing peer data
  -> Int                    -- ^ Delay in milliseconds between traces
  -> IO ()
startPeerTracer tracer nodeKernel delayMilliseconds = do
  thread <- async peersThread
  -- Link the thread to the parent to propagate exceptions properly.
  link thread
  where
    -- | The background thread that periodically traces the peer list.
    peersThread :: IO ()
    peersThread = do
      -- Label the thread for easier debugging and identification.
      myThreadId >>= flip labelThread "Peer Tracer"
      forever $ do
        peers <- getCurrentPeers nodeKernel
        traceWith tracer peers
        threadDelay (delayMilliseconds * 1000)


getCurrentPeers
  :: NodeKernelData blk
  -> IO [PeerT blk]
getCurrentPeers nkd = mapNodeKernelDataIO extractPeers nkd
                      <&> fromSMaybe mempty
 where
  tuple3pop :: (a, b, c) -> (a, b)
  tuple3pop (a, b, _) = (a, b)

  getCandidates
    :: STM.STM IO (Map peer (ChainSyncClientHandle IO blk))
    -> STM.STM IO (Map peer (Net.AnchoredFragment (Header blk)))
  getCandidates handle = viewChainSyncState handle csCandidate

  extractPeers :: NodeKernel IO RemoteAddress LocalConnectionId blk
                -> IO [PeerT blk]
  extractPeers kernel = do
    peerStates <- fmap tuple3pop <$> (   STM.atomically
                                       . (>>= traverse readFetchClientState)
                                       . Net.readFetchClientsStateVars
                                       . getFetchClientRegistry $ kernel
                                     )
    candidates <- STM.atomically . getCandidates . cschcMap . getChainSyncHandles $ kernel

    let peers = flip Map.mapMaybeWithKey candidates $ \cid af ->
                  maybe Nothing
                        (\(status, inflight) -> Just $ PeerT cid af status inflight)
                        $ Map.lookup cid peerStates
    pure . Map.elems $ peers
