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
import           Text.Printf (printf)

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

data PeerT blk = PeerT
    RemoteConnectionId
    (Net.AnchoredFragment (Header blk))
    (PeerFetchStatus (Header blk))
    (PeerFetchInFlight (Header blk))

ppPeer :: PeerT blk -> Text
ppPeer (PeerT cid _af status inflight) =
  Text.pack $ printf "%-15s %-8s %s" (ppCid cid) (ppStatus status) (ppInFlight inflight)

  where
  ppCid :: RemoteConnectionId -> String
  ppCid = takeWhile (/= ':') . show . remoteAddress

  ppInFlight :: PeerFetchInFlight header -> String
  ppInFlight f = printf
    "%5s  %3d  %5d  %6d"
    (ppMaxSlotNo $ peerFetchMaxSlotNo f)
    (peerFetchReqsInFlight f)
    (Set.size $ peerFetchBlocksInFlight f)
    (peerFetchBytesInFlight f)

ppStatus :: PeerFetchStatus header -> String
ppStatus = \case
  PeerFetchStatusStarting -> "starting"
  PeerFetchStatusShutdown -> "shutdown"
  PeerFetchStatusAberrant -> "aberrant"
  PeerFetchStatusBusy     -> "fetching"
  PeerFetchStatusReady {} -> "ready"

ppMaxSlotNo :: Net.MaxSlotNo -> String
ppMaxSlotNo Net.NoMaxSlotNo   = "???"
ppMaxSlotNo (Net.MaxSlotNo x) = show (unSlotNo x)

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

-- --------------------------------------------------------------------------------
-- -- Peers Tracer
-- --------------------------------------------------------------------------------

instance LogFormatting [PeerT blk] where
  forMachine _ []       = mempty
  forMachine dtal xs    = mconcat
    [ "peers" .= toJSON (List.foldl' (\acc x -> forMachine dtal x : acc) [] xs)
    ]
  forHuman peers = Text.concat $ List.intersperse ", " (map ppPeer peers)
  asMetrics peers = [IntM "peersFromNodeKernel" (fromIntegral (length peers))]

instance LogFormatting (PeerT blk) where
  forMachine _dtal (PeerT cid _af status inflight) =
    mconcat [  "peerAddress"   .= String (Text.pack . show . remoteAddress $ cid)
             , "peerStatus"    .= String (Text.pack . ppStatus $ status)
             , "peerSlotNo"    .= String (Text.pack . ppMaxSlotNo . peerFetchMaxSlotNo $ inflight)
             , "peerReqsInF"   .= String (Text.pack . show . peerFetchReqsInFlight $ inflight)
             , "peerBlocksInF" .= String (Text.pack . show . Set.size . peerFetchBlocksInFlight $ inflight)
             , "peerBytesInF"  .= String (Text.pack . show . peerFetchBytesInFlight $ inflight)
             ]

instance MetaTrace [PeerT blk] where
  namespaceFor _  =
    Namespace [] ["PeersFromNodeKernel"]
  severityFor  (Namespace _ ["PeersFromNodeKernel"]) (Just []) =
    Just Debug
  severityFor  (Namespace _ ["PeersFromNodeKernel"]) _ =
    Just Info
  severityFor _ns _ =
    Nothing
  documentFor (Namespace _ ["PeersFromNodeKernel"]) =
    Just ""
  documentFor _ns =
    Nothing
  metricsDocFor (Namespace _ ["PeersFromNodeKernel"]) =
    [("peersFromNodeKernel","")]
  metricsDocFor _ns = []
  allNamespaces = [ Namespace [] ["PeersFromNodeKernel"]]
