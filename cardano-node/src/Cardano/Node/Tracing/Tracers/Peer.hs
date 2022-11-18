{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Cardano.Node.Tracing.Tracers.Peer
  ( PeerT (..)
  , startPeerTracer
  , namesForPeers
  , severityPeers
  , docPeers
  , ppPeer
  ) where

import           Cardano.Prelude hiding (atomically)
import           Prelude (String)

import qualified Control.Concurrent.Class.MonadSTM.Strict as STM
import           "contra-tracer" Control.Tracer

import           Data.Aeson (ToJSON (..), Value (..), toJSON, (.=))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Text.Printf (printf)

import           Ouroboros.Consensus.Block (Header)
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Network.ConnectionId (remoteAddress)

import qualified Ouroboros.Network.AnchoredFragment as Net
import           Ouroboros.Network.Block (unSlotNo)
import qualified Ouroboros.Network.Block as Net
import qualified Ouroboros.Network.BlockFetch.ClientRegistry as Net
import           Ouroboros.Network.BlockFetch.ClientState (PeerFetchInFlight (..),
                   PeerFetchStatus (..), readFetchClientState)

import           Cardano.Logging hiding (traceWith)
import           Cardano.Node.Queries

startPeerTracer
  :: Tracer IO [PeerT blk]
  -> NodeKernelData blk
  -> Int
  -> IO ()
startPeerTracer tr nodeKern delayMilliseconds = do
    as <- async peersThread
    link as
  where
    peersThread :: IO ()
    peersThread = forever $ do
          peers <- getCurrentPeers nodeKern
          traceWith tr peers
          threadDelay (delayMilliseconds * 1000)

data PeerT blk = PeerT
    RemoteConnectionId
    (Net.AnchoredFragment (Header blk))
    (PeerFetchStatus (Header blk))
    (PeerFetchInFlight (Header blk))


ppPeer :: PeerT blk -> Text
ppPeer (PeerT cid _af status inflight) =
  Text.pack $ printf "%-15s %-8s %s" (ppCid cid) (ppStatus status) (ppInFlight inflight)

ppCid :: RemoteConnectionId -> String
ppCid = takeWhile (/= ':') . show . remoteAddress

ppInFlight :: PeerFetchInFlight header -> String
ppInFlight f = printf
 "%5s  %3d  %5d  %6d"
 (ppMaxSlotNo $ peerFetchMaxSlotNo f)
 (peerFetchReqsInFlight f)
 (Set.size $ peerFetchBlocksInFlight f)
 (peerFetchBytesInFlight f)

ppMaxSlotNo :: Net.MaxSlotNo -> String
ppMaxSlotNo Net.NoMaxSlotNo   = "???"
ppMaxSlotNo (Net.MaxSlotNo x) = show (unSlotNo x)

ppStatus :: PeerFetchStatus header -> String
ppStatus PeerFetchStatusShutdown = "shutdown"
ppStatus PeerFetchStatusAberrant = "aberrant"
ppStatus PeerFetchStatusBusy     = "fetching"
ppStatus PeerFetchStatusReady {} = "ready"

getCurrentPeers
  :: NodeKernelData blk
  -> IO [PeerT blk]
getCurrentPeers nkd = mapNodeKernelDataIO extractPeers nkd
                      <&> fromSMaybe mempty
 where
  tuple3pop :: (a, b, c) -> (a, b)
  tuple3pop (a, b, _) = (a, b)

  getCandidates
    :: STM.StrictTVar IO (Map peer (STM.StrictTVar IO (Net.AnchoredFragment (Header blk))))
    -> STM.STM IO (Map peer (Net.AnchoredFragment (Header blk)))
  getCandidates var = STM.readTVar var >>= traverse STM.readTVar

  extractPeers :: NodeKernel IO RemoteConnectionId LocalConnectionId blk
                -> IO [PeerT blk]
  extractPeers kernel = do
    peerStates <- fmap tuple3pop <$> (   STM.atomically
                                       . (>>= traverse readFetchClientState)
                                       . Net.readFetchClientsStateVars
                                       . getFetchClientRegistry $ kernel
                                     )
    candidates <- STM.atomically . getCandidates . getNodeCandidates $ kernel

    let peers = flip Map.mapMaybeWithKey candidates $ \cid af ->
                  maybe Nothing
                        (\(status, inflight) -> Just $ PeerT cid af status inflight)
                        $ Map.lookup cid peerStates
    pure . Map.elems $ peers

--------------------------------------------------------------------------------
-- Peers Tracer
--------------------------------------------------------------------------------

namesForPeers :: [PeerT blk] -> [Text]
namesForPeers _ = []

severityPeers :: [PeerT blk] -> SeverityS
severityPeers [] = Debug
severityPeers _ = Notice

instance LogFormatting [PeerT blk] where
  forMachine _ []       = mempty
  forMachine dtal xs    = mconcat
    [ "kind"  .= String "NodeKernelPeers"
    , "peers" .= toJSON (foldl' (\acc x -> forMachine dtal x : acc) [] xs)
    ]
  forHuman peers = Text.concat $ intersperse ", " (map ppPeer peers)
  asMetrics peers = [IntM "Net.PeersFromNodeKernel" (fromIntegral (length peers))]

instance LogFormatting (PeerT blk) where
  forMachine _dtal (PeerT cid _af status inflight) =
    mconcat [ "peerAddress"   .= String (Text.pack . show . remoteAddress $ cid)
             , "peerStatus"    .= String (Text.pack . ppStatus $ status)
             , "peerSlotNo"    .= String (Text.pack . ppMaxSlotNo . peerFetchMaxSlotNo $ inflight)
             , "peerReqsInF"   .= String (show . peerFetchReqsInFlight $ inflight)
             , "peerBlocksInF" .= String (show . Set.size . peerFetchBlocksInFlight $ inflight)
             , "peerBytesInF"  .= String (show . peerFetchBytesInFlight $ inflight)
             ]

docPeers :: Documented [PeerT blk]
docPeers = Documented [
      DocMsg
        []
        [("Net.PeersFromNodeKernel","")]
        ""
    ]
